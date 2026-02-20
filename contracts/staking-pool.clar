;; Staking Pool Contract
;; Stake tokens to earn rewards with flexible/locked options

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-stake (err u101))
(define-constant err-lock-active (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-pool-paused (err u104))
(define-constant err-no-rewards (err u105))
(define-constant err-invalid-tier (err u106))
(define-constant err-cooldown-active (err u107))

;; Staking tiers
(define-constant tier-flexible u0)
(define-constant tier-30-days u1)
(define-constant tier-90-days u2)
(define-constant tier-180-days u3)
(define-constant tier-365-days u4)

;; Tier configurations (APY in basis points)
(define-map tier-configs uint { duration: uint, apy: uint, early-unstake-fee: uint })

;; Initialize tiers
(map-set tier-configs tier-flexible { duration: u0, apy: u500, early-unstake-fee: u0 })
(map-set tier-configs tier-30-days { duration: u4320, apy: u800, early-unstake-fee: u500 })
(map-set tier-configs tier-90-days { duration: u12960, apy: u1200, early-unstake-fee: u1000 })
(map-set tier-configs tier-180-days { duration: u25920, apy: u1800, early-unstake-fee: u1500 })
(map-set tier-configs tier-365-days { duration: u52560, apy: u2500, early-unstake-fee: u2000 })

;; Data Variables
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-pool uint u0)
(define-data-var pool-paused bool false)
(define-data-var stake-nonce uint u0)
(define-data-var cooldown-period uint u144) ;; ~1 day in blocks

;; User stakes
(define-map stakes uint
  {
    staker: principal,
    amount: uint,
    tier: uint,
    staked-at: uint,
    lock-until: uint,
    last-claim: uint,
    accumulated-rewards: uint,
    cooldown-start: (optional uint)
  }
)

;; User to stakes mapping
(define-map user-stakes principal (list 10 uint))

;; User stats
(define-map user-stats principal
  {
    total-staked: uint,
    total-rewards: uint,
    stake-count: uint
  }
)

;; Public Functions

(define-public (deposit-stake (amount uint) (tier uint))
  (let (
    (stake-id (var-get stake-nonce))
    (tier-config (unwrap! (map-get? tier-configs tier) err-invalid-tier))
    (lock-until (+ stacks-block-height (get duration tier-config)))
  )
    (asserts! (not (var-get pool-paused)) err-pool-paused)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Create stake
    (map-set stakes stake-id
      {
        staker: tx-sender,
        amount: amount,
        tier: tier,
        staked-at: stacks-block-height,
        lock-until: lock-until,
        last-claim: stacks-block-height,
        accumulated-rewards: u0,
        cooldown-start: none
      }
    )
    
    ;; Add to user's stakes
    (map-set user-stakes tx-sender
      (unwrap! (as-max-len? 
        (append (get-user-stake-ids tx-sender) stake-id) 
        u10) err-invalid-amount)
    )
    
    ;; Update stats
    (var-set stake-nonce (+ stake-id u1))
    (var-set total-staked (+ (var-get total-staked) amount))
    
    (map-set user-stats tx-sender
      (merge (get-user-stats tx-sender)
        {
          total-staked: (+ (get total-staked (get-user-stats tx-sender)) amount),
          stake-count: (+ (get stake-count (get-user-stats tx-sender)) u1)
        }
      )
    )
    
    (ok { stake-id: stake-id, lock-until: lock-until, tier: tier })
  )
)

(define-public (initiate-unstake (stake-id uint))
  (match (map-get? stakes stake-id)
    stake
    (begin
      (asserts! (is-eq (get staker stake) tx-sender) err-owner-only)
      (asserts! (is-none (get cooldown-start stake)) err-cooldown-active)
      
      ;; Start cooldown
      (map-set stakes stake-id
        (merge stake { cooldown-start: (some stacks-block-height) })
      )
      
      (ok { cooldown-ends: (+ stacks-block-height (var-get cooldown-period)) })
    )
    err-insufficient-stake
  )
)

(define-public (unstake (stake-id uint))
  (match (map-get? stakes stake-id)
    stake
    (let (
      (tier-config (unwrap! (map-get? tier-configs (get tier stake)) err-invalid-tier))
      (is-early (< stacks-block-height (get lock-until stake)))
      (early-fee (if is-early 
                   (/ (* (get amount stake) (get early-unstake-fee tier-config)) u10000)
                   u0))
      (pending-rewards (calculate-pending-rewards stake-id))
      (total-rewards (+ pending-rewards (get accumulated-rewards stake)))
      (return-amount (- (get amount stake) early-fee))
    )
      (asserts! (is-eq (get staker stake) tx-sender) err-owner-only)
      
      ;; Check cooldown for flexible tier
      (if (is-eq (get tier stake) tier-flexible)
        (match (get cooldown-start stake)
          cooldown-block 
          (asserts! (>= stacks-block-height (+ cooldown-block (var-get cooldown-period))) err-cooldown-active)
          (asserts! false err-cooldown-active)
        )
        true
      )
      
      ;; Claim pending rewards first
      (if (> total-rewards u0)
        (if (<= total-rewards (var-get reward-pool))
          (begin
            (try! (as-contract (stx-transfer? total-rewards tx-sender (get staker stake))))
            (var-set reward-pool (- (var-get reward-pool) total-rewards))
            (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) total-rewards))
          )
          true
        )
        true
      )
      
      ;; Return staked amount (minus early fee if applicable)
      (try! (as-contract (stx-transfer? return-amount tx-sender (get staker stake))))
      
      ;; Delete stake
      (map-delete stakes stake-id)
      
      ;; Update stats
      (var-set total-staked (- (var-get total-staked) (get amount stake)))
      
      (map-set user-stats tx-sender
        (merge (get-user-stats tx-sender)
          {
            total-staked: (- (get total-staked (get-user-stats tx-sender)) (get amount stake)),
            total-rewards: (+ (get total-rewards (get-user-stats tx-sender)) total-rewards)
          }
        )
      )
      
      (ok { 
        unstaked: return-amount, 
        rewards-claimed: total-rewards, 
        early-fee-paid: early-fee 
      })
    )
    err-insufficient-stake
  )
)

(define-public (claim-rewards (stake-id uint))
  (match (map-get? stakes stake-id)
    stake
    (let (
      (pending (calculate-pending-rewards stake-id))
      (total-rewards (+ pending (get accumulated-rewards stake)))
    )
      (asserts! (is-eq (get staker stake) tx-sender) err-owner-only)
      (asserts! (> total-rewards u0) err-no-rewards)
      (asserts! (<= total-rewards (var-get reward-pool)) err-no-rewards)
      
      ;; Transfer rewards
      (try! (as-contract (stx-transfer? total-rewards tx-sender (get staker stake))))
      
      ;; Update stake
      (map-set stakes stake-id
        (merge stake {
          last-claim: stacks-block-height,
          accumulated-rewards: u0
        })
      )
      
      ;; Update stats
      (var-set reward-pool (- (var-get reward-pool) total-rewards))
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) total-rewards))
      
      (map-set user-stats tx-sender
        (merge (get-user-stats tx-sender)
          { total-rewards: (+ (get total-rewards (get-user-stats tx-sender)) total-rewards) }
        )
      )
      
      (ok { claimed: total-rewards })
    )
    err-insufficient-stake
  )
)

(define-public (add-to-reward-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok { new-pool: (var-get reward-pool) })
  )
)

;; Read-only Functions

(define-read-only (get-stake (stake-id uint))
  (map-get? stakes stake-id)
)

(define-read-only (get-user-stake-ids (user principal))
  (default-to (list) (map-get? user-stakes user))
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { total-staked: u0, total-rewards: u0, stake-count: u0 }
    (map-get? user-stats user)
  )
)

(define-read-only (get-tier-config (tier uint))
  (map-get? tier-configs tier)
)

(define-read-only (calculate-pending-rewards (stake-id uint))
  (match (map-get? stakes stake-id)
    stake
    (match (map-get? tier-configs (get tier stake))
      tier-config
      (let (
        (blocks-elapsed (- stacks-block-height (get last-claim stake)))
        (annual-reward (/ (* (get amount stake) (get apy tier-config)) u10000))
        (reward-per-block (/ annual-reward u52560)) ;; blocks per year
      )
        (* blocks-elapsed reward-per-block)
      )
      u0
    )
    u0
  )
)

(define-read-only (get-stake-info (stake-id uint))
  (match (map-get? stakes stake-id)
    stake
    (match (map-get? tier-configs (get tier stake))
      tier-config
      (ok {
        staker: (get staker stake),
        amount: (get amount stake),
        tier: (get tier stake),
        apy: (get apy tier-config),
        lock-until: (get lock-until stake),
        is-locked: (< stacks-block-height (get lock-until stake)),
        pending-rewards: (calculate-pending-rewards stake-id),
        accumulated-rewards: (get accumulated-rewards stake),
        cooldown-active: (is-some (get cooldown-start stake)),
        blocks-until-unlock: (if (> (get lock-until stake) stacks-block-height)
                               (- (get lock-until stake) stacks-block-height)
                               u0)
      })
      err-invalid-tier
    )
    err-insufficient-stake
  )
)

(define-read-only (get-pool-stats)
  {
    total-staked: (var-get total-staked),
    reward-pool: (var-get reward-pool),
    total-distributed: (var-get total-rewards-distributed),
    total-stakes: (var-get stake-nonce),
    pool-paused: (var-get pool-paused),
    cooldown-period: (var-get cooldown-period)
  }
)

(define-read-only (estimate-rewards (amount uint) (tier uint) (duration uint))
  (match (map-get? tier-configs tier)
    tier-config
    (let (
      (annual-reward (/ (* amount (get apy tier-config)) u10000))
      (reward-per-block (/ annual-reward u52560))
    )
      (ok {
        estimated-rewards: (* duration reward-per-block),
        apy: (get apy tier-config),
        early-unstake-fee: (get early-unstake-fee tier-config)
      })
    )
    err-invalid-tier
  )
)

;; Admin Functions

(define-public (toggle-pool-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set pool-paused (not (var-get pool-paused)))
    (ok (var-get pool-paused))
  )
)

(define-public (set-cooldown-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set cooldown-period new-period)
    (ok new-period)
  )
)

(define-public (update-tier-config (tier uint) (duration uint) (apy uint) (early-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set tier-configs tier { duration: duration, apy: apy, early-unstake-fee: early-fee })
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)
