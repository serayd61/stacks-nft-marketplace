;; Stacking Vault Contract
;; Non-custodial STX staking vault with reward tracking and auto-compounding
;; Built for Stacks ecosystem - integrates with PoX cycle rewards

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u700))
(define-constant err-not-found (err u701))
(define-constant err-not-authorized (err u702))
(define-constant err-insufficient-funds (err u703))
(define-constant err-invalid-amount (err u704))
(define-constant err-vault-locked (err u705))
(define-constant err-cooldown-active (err u706))
(define-constant err-already-staking (err u707))
(define-constant err-min-stake-required (err u708))

;; Vault parameters
(define-constant MIN-STAKE u500000000)          ;; 500 STX minimum
(define-constant COOLDOWN-BLOCKS u144)           ;; ~1 day unstake cooldown
(define-constant EARLY-UNSTAKE-PENALTY-BPS u200) ;; 2% penalty for early exit
(define-constant FEE-DENOMINATOR u10000)

;; Data Variables
(define-data-var vault-active bool true)
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var staker-count uint u0)
(define-data-var vault-nonce uint u0)
(define-data-var reward-pool uint u0)
(define-data-var current-apy-bps uint u800)      ;; 8% default APY in bps
(define-data-var pox-cycle uint u0)
(define-data-var treasury principal contract-owner)

;; Staker positions
(define-map staker-positions principal
  {
    staked-amount: uint,
    staked-at: uint,
    last-reward-claim: uint,
    unstake-requested-at: (optional uint),
    pending-unstake: uint,
    total-rewards-earned: uint,
    lock-until: uint,             ;; block height lock expires
    auto-compound: bool
  }
)

;; Reward snapshots per PoX cycle
(define-map cycle-rewards uint
  {
    cycle: uint,
    total-staked-at-snapshot: uint,
    reward-amount: uint,
    snapshot-block: uint,
    distributed: bool
  }
)

;; Vault events log
(define-map vault-events uint
  {
    event-type: (string-ascii 20),
    staker: principal,
    amount: uint,
    block-height: uint,
    cycle: uint
  }
)

;; Whitelist for early access
(define-map whitelisted-stakers principal bool)

;; Read-only functions

(define-read-only (get-staker-position (staker principal))
  (map-get? staker-positions staker)
)

(define-read-only (get-vault-stats)
  {
    total-staked: (var-get total-staked),
    staker-count: (var-get staker-count),
    reward-pool: (var-get reward-pool),
    total-rewards-distributed: (var-get total-rewards-distributed),
    current-apy-bps: (var-get current-apy-bps),
    vault-active: (var-get vault-active),
    current-cycle: (var-get pox-cycle)
  }
)

(define-read-only (get-cycle-rewards (cycle uint))
  (map-get? cycle-rewards cycle)
)

(define-read-only (calculate-pending-rewards (staker principal))
  (match (map-get? staker-positions staker)
    position
    (let (
      (blocks-staked (- stacks-block-height (get last-reward-claim position)))
      (staked (get staked-amount position))
      (apy (var-get current-apy-bps))
      ;; Approximate: (staked * apy * blocks) / (10000 * 52596) where 52596 = ~1 year in blocks
      (annual-reward (/ (* staked apy) FEE-DENOMINATOR))
      (block-reward (/ annual-reward u52596))
      (pending (* block-reward blocks-staked))
    )
      (ok pending)
    )
    err-not-found
  )
)

(define-read-only (can-unstake (staker principal))
  (match (map-get? staker-positions staker)
    position
    (match (get unstake-requested-at position)
      requested-at
      (ok (>= stacks-block-height (+ requested-at COOLDOWN-BLOCKS)))
      (ok false)
    )
    err-not-found
  )
)

(define-read-only (get-vault-share-bps (staker principal))
  (let ((total (var-get total-staked)))
    (if (> total u0)
      (match (map-get? staker-positions staker)
        position (ok (/ (* (get staked-amount position) u10000) total))
        (ok u0)
      )
      (ok u0)
    )
  )
)

(define-read-only (is-whitelisted (staker principal))
  (default-to false (map-get? whitelisted-stakers staker))
)

;; Public functions

;; Stake STX into vault
(define-public (stake (amount uint) (lock-blocks uint) (auto-compound bool))
  (let (
    (event-id (var-get vault-nonce))
  )
    (asserts! (var-get vault-active) err-vault-locked)
    (asserts! (>= amount MIN-STAKE) err-min-stake-required)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)

    ;; Transfer STX to vault
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update or create position
    (match (map-get? staker-positions tx-sender)
      existing-position
      ;; Add to existing stake
      (map-set staker-positions tx-sender (merge existing-position {
        staked-amount: (+ (get staked-amount existing-position) amount),
        lock-until: (+ stacks-block-height lock-blocks),
        auto-compound: auto-compound
      }))
      ;; New staker
      (begin
        (map-set staker-positions tx-sender {
          staked-amount: amount,
          staked-at: stacks-block-height,
          last-reward-claim: stacks-block-height,
          unstake-requested-at: none,
          pending-unstake: u0,
          total-rewards-earned: u0,
          lock-until: (+ stacks-block-height lock-blocks),
          auto-compound: auto-compound
        })
        (var-set staker-count (+ (var-get staker-count) u1))
      )
    )

    (var-set total-staked (+ (var-get total-staked) amount))

    ;; Log event
    (map-set vault-events event-id {
      event-type: "stake",
      staker: tx-sender,
      amount: amount,
      block-height: stacks-block-height,
      cycle: (var-get pox-cycle)
    })
    (var-set vault-nonce (+ event-id u1))

    (ok { staked: amount, lock-until: (+ stacks-block-height lock-blocks), auto-compound: auto-compound })
  )
)

;; Request unstake (starts cooldown)
(define-public (request-unstake (amount uint))
  (match (map-get? staker-positions tx-sender)
    position
    (begin
      (asserts! (>= (get staked-amount position) amount) err-insufficient-funds)
      (asserts! (> amount u0) err-invalid-amount)
      ;; Check lock period
      (asserts! (>= stacks-block-height (get lock-until position)) err-vault-locked)
      ;; No pending unstake already
      (asserts! (is-none (get unstake-requested-at position)) err-cooldown-active)

      (map-set staker-positions tx-sender (merge position {
        unstake-requested-at: (some stacks-block-height),
        pending-unstake: amount
      }))

      (ok { unstake-amount: amount, available-at: (+ stacks-block-height COOLDOWN-BLOCKS) })
    )
    err-not-found
  )
)

;; Complete unstake after cooldown
(define-public (complete-unstake)
  (match (map-get? staker-positions tx-sender)
    position
    (match (get unstake-requested-at position)
      requested-at
      (let (
        (amount (get pending-unstake position))
        (cooldown-passed (>= stacks-block-height (+ requested-at COOLDOWN-BLOCKS)))
        (event-id (var-get vault-nonce))
      )
        (asserts! (> amount u0) err-invalid-amount)

        (if cooldown-passed
          ;; Full unstake - no penalty
          (begin
            (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
            (map-set staker-positions tx-sender (merge position {
              staked-amount: (- (get staked-amount position) amount),
              unstake-requested-at: none,
              pending-unstake: u0
            }))
            (var-set total-staked (- (var-get total-staked) amount))
            (map-set vault-events event-id {
              event-type: "unstake",
              staker: tx-sender,
              amount: amount,
              block-height: stacks-block-height,
              cycle: (var-get pox-cycle)
            })
            (var-set vault-nonce (+ event-id u1))
            (ok { unstaked: amount, penalty: u0 })
          )
          ;; Early unstake - apply penalty
          (let (
            (penalty (/ (* amount EARLY-UNSTAKE-PENALTY-BPS) FEE-DENOMINATOR))
            (net-amount (- amount penalty))
          )
            (try! (as-contract (stx-transfer? net-amount tx-sender tx-sender)))
            (try! (as-contract (stx-transfer? penalty tx-sender (var-get treasury))))
            (map-set staker-positions tx-sender (merge position {
              staked-amount: (- (get staked-amount position) amount),
              unstake-requested-at: none,
              pending-unstake: u0
            }))
            (var-set total-staked (- (var-get total-staked) amount))
            (ok { unstaked: net-amount, penalty: penalty })
          )
        )
      )
      err-cooldown-active
    )
    err-not-found
  )
)

;; Claim rewards
(define-public (claim-rewards)
  (match (map-get? staker-positions tx-sender)
    position
    (let (
      (pending (unwrap! (calculate-pending-rewards tx-sender) err-not-found))
      (pool (var-get reward-pool))
      (claimable (if (> pending pool) pool pending))
      (event-id (var-get vault-nonce))
    )
      (asserts! (> claimable u0) err-invalid-amount)

      (if (get auto-compound position)
        ;; Auto-compound: add to staked amount
        (begin
          (map-set staker-positions tx-sender (merge position {
            staked-amount: (+ (get staked-amount position) claimable),
            last-reward-claim: stacks-block-height,
            total-rewards-earned: (+ (get total-rewards-earned position) claimable)
          }))
          (var-set total-staked (+ (var-get total-staked) claimable))
        )
        ;; Direct claim
        (begin
          (try! (as-contract (stx-transfer? claimable tx-sender tx-sender)))
          (map-set staker-positions tx-sender (merge position {
            last-reward-claim: stacks-block-height,
            total-rewards-earned: (+ (get total-rewards-earned position) claimable)
          }))
        )
      )

      (var-set reward-pool (- pool claimable))
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) claimable))

      (map-set vault-events event-id {
        event-type: "claim",
        staker: tx-sender,
        amount: claimable,
        block-height: stacks-block-height,
        cycle: (var-get pox-cycle)
      })
      (var-set vault-nonce (+ event-id u1))

      (ok { claimed: claimable, auto-compounded: (get auto-compound position) })
    )
    err-not-found
  )
)

;; Fund reward pool (owner or treasury)
(define-public (fund-rewards (amount uint))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (var-get treasury))) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok { funded: amount, total-pool: (var-get reward-pool) })
  )
)

;; Update APY (owner only)
(define-public (set-apy-bps (new-apy-bps uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-apy-bps u5000) err-invalid-amount) ;; Max 50% APY
    (var-set current-apy-bps new-apy-bps)
    (ok { new-apy-bps: new-apy-bps })
  )
)

;; Whitelist staker
(define-public (whitelist-staker (staker principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set whitelisted-stakers staker true)
    (ok { staker: staker, whitelisted: true })
  )
)

;; Advance PoX cycle (called by authorized updater)
(define-public (advance-cycle)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set pox-cycle (+ (var-get pox-cycle) u1))
    (ok { new-cycle: (var-get pox-cycle) })
  )
)
