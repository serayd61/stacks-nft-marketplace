;; NFT Staking Contract
;; Stake NFTs to earn rewards over time

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-staker (err u101))
(define-constant err-already-staked (err u102))
(define-constant err-not-staked (err u103))
(define-constant err-lock-period-active (err u104))
(define-constant err-invalid-nft (err u105))
(define-constant err-insufficient-rewards (err u106))
(define-constant err-pool-not-active (err u107))

;; Reward settings
(define-constant reward-per-block u1000) ;; 0.001 STX per block
(define-constant min-lock-period u1440) ;; ~1 day in blocks
(define-constant bonus-multiplier u150) ;; 1.5x for long-term staking

;; Data Variables
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-pool uint u0)
(define-data-var pool-active bool true)
(define-data-var stake-nonce uint u0)

;; Staking info per NFT
(define-map staked-nfts 
  { nft-contract: principal, token-id: uint }
  {
    staker: principal,
    staked-at: uint,
    lock-until: uint,
    accumulated-rewards: uint,
    last-claim-block: uint,
    stake-id: uint
  }
)

;; User staking stats
(define-map user-stakes principal
  {
    total-staked: uint,
    total-claimed: uint,
    stake-count: uint
  }
)

;; Whitelisted NFT contracts
(define-map whitelisted-nfts principal bool)

;; Stake tier bonuses
(define-map stake-tiers uint { min-duration: uint, bonus: uint })

;; Initialize tiers
(map-set stake-tiers u1 { min-duration: u1440, bonus: u100 })    ;; 1 day: 1x
(map-set stake-tiers u2 { min-duration: u10080, bonus: u125 })   ;; 7 days: 1.25x
(map-set stake-tiers u3 { min-duration: u43200, bonus: u150 })   ;; 30 days: 1.5x
(map-set stake-tiers u4 { min-duration: u129600, bonus: u200 })  ;; 90 days: 2x

;; Public Functions

(define-public (stake-nft (nft-contract principal) (token-id uint) (lock-duration uint))
  (let (
    (stake-id (var-get stake-nonce))
    (lock-until (+ stacks-block-height lock-duration))
  )
    (asserts! (var-get pool-active) err-pool-not-active)
    (asserts! (default-to false (map-get? whitelisted-nfts nft-contract)) err-invalid-nft)
    (asserts! (>= lock-duration min-lock-period) err-lock-period-active)
    (asserts! (is-none (map-get? staked-nfts { nft-contract: nft-contract, token-id: token-id })) err-already-staked)
    
    ;; Record stake
    (map-set staked-nfts 
      { nft-contract: nft-contract, token-id: token-id }
      {
        staker: tx-sender,
        staked-at: stacks-block-height,
        lock-until: lock-until,
        accumulated-rewards: u0,
        last-claim-block: stacks-block-height,
        stake-id: stake-id
      }
    )
    
    ;; Update user stats
    (map-set user-stakes tx-sender
      (merge (get-user-stakes tx-sender)
        { 
          total-staked: (+ (get total-staked (get-user-stakes tx-sender)) u1),
          stake-count: (+ (get stake-count (get-user-stakes tx-sender)) u1)
        }
      )
    )
    
    (var-set total-staked (+ (var-get total-staked) u1))
    (var-set stake-nonce (+ stake-id u1))
    
    (ok { stake-id: stake-id, lock-until: lock-until })
  )
)

(define-public (unstake-nft (nft-contract principal) (token-id uint))
  (match (map-get? staked-nfts { nft-contract: nft-contract, token-id: token-id })
    stake-info
    (begin
      (asserts! (is-eq (get staker stake-info) tx-sender) err-not-staker)
      (asserts! (>= stacks-block-height (get lock-until stake-info)) err-lock-period-active)
      
      ;; Claim pending rewards first
      (try! (claim-rewards-internal nft-contract token-id stake-info))
      
      ;; Remove stake
      (map-delete staked-nfts { nft-contract: nft-contract, token-id: token-id })
      
      ;; Update user stats
      (map-set user-stakes tx-sender
        (merge (get-user-stakes tx-sender)
          { total-staked: (- (get total-staked (get-user-stakes tx-sender)) u1) }
        )
      )
      
      (var-set total-staked (- (var-get total-staked) u1))
      
      (ok { token-id: token-id, unstaked-at: stacks-block-height })
    )
    err-not-staked
  )
)

(define-public (claim-rewards (nft-contract principal) (token-id uint))
  (match (map-get? staked-nfts { nft-contract: nft-contract, token-id: token-id })
    stake-info
    (begin
      (asserts! (is-eq (get staker stake-info) tx-sender) err-not-staker)
      (claim-rewards-internal nft-contract token-id stake-info)
    )
    err-not-staked
  )
)

(define-private (claim-rewards-internal (nft-contract principal) (token-id uint) (stake-info { staker: principal, staked-at: uint, lock-until: uint, accumulated-rewards: uint, last-claim-block: uint, stake-id: uint }))
  (let (
    (pending (calculate-pending-rewards nft-contract token-id))
    (total-rewards (+ pending (get accumulated-rewards stake-info)))
  )
    (asserts! (>= (var-get reward-pool) total-rewards) err-insufficient-rewards)
    
    ;; Transfer rewards
    (if (> total-rewards u0)
      (begin
        (try! (as-contract (stx-transfer? total-rewards tx-sender (get staker stake-info))))
        (var-set reward-pool (- (var-get reward-pool) total-rewards))
        (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) total-rewards))
        
        ;; Update stake info
        (map-set staked-nfts 
          { nft-contract: nft-contract, token-id: token-id }
          (merge stake-info { 
            accumulated-rewards: u0, 
            last-claim-block: stacks-block-height 
          })
        )
        
        ;; Update user stats
        (map-set user-stakes (get staker stake-info)
          (merge (get-user-stakes (get staker stake-info))
            { total-claimed: (+ (get total-claimed (get-user-stakes (get staker stake-info))) total-rewards) }
          )
        )
        
        (ok { claimed: total-rewards })
      )
      (ok { claimed: u0 })
    )
  )
)

;; Read-only Functions

(define-read-only (get-stake-info (nft-contract principal) (token-id uint))
  (map-get? staked-nfts { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-user-stakes (user principal))
  (default-to 
    { total-staked: u0, total-claimed: u0, stake-count: u0 }
    (map-get? user-stakes user)
  )
)

(define-read-only (calculate-pending-rewards (nft-contract principal) (token-id uint))
  (match (map-get? staked-nfts { nft-contract: nft-contract, token-id: token-id })
    stake-info
    (let (
      (blocks-staked (- stacks-block-height (get last-claim-block stake-info)))
      (base-reward (* blocks-staked reward-per-block))
      (bonus (get-stake-bonus (- (get lock-until stake-info) (get staked-at stake-info))))
    )
      (/ (* base-reward bonus) u100)
    )
    u0
  )
)

(define-read-only (get-stake-bonus (lock-duration uint))
  (if (>= lock-duration u129600) u200
    (if (>= lock-duration u43200) u150
      (if (>= lock-duration u10080) u125
        u100
      )
    )
  )
)

(define-read-only (get-pool-stats)
  {
    total-staked: (var-get total-staked),
    reward-pool: (var-get reward-pool),
    total-distributed: (var-get total-rewards-distributed),
    pool-active: (var-get pool-active),
    reward-per-block: reward-per-block
  }
)

(define-read-only (is-nft-whitelisted (nft-contract principal))
  (default-to false (map-get? whitelisted-nfts nft-contract))
)

;; Admin Functions

(define-public (add-to-reward-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok (var-get reward-pool))
  )
)

(define-public (whitelist-nft (nft-contract principal) (whitelisted bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set whitelisted-nfts nft-contract whitelisted)
    (ok true)
  )
)

(define-public (toggle-pool-active)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set pool-active (not (var-get pool-active)))
    (ok (var-get pool-active))
  )
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set reward-pool (- (var-get reward-pool) amount))
    (ok true)
  )
)
