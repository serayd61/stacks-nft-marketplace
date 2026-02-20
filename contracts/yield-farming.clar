;; Yield Farming Contract
;; Stake LP tokens to earn reward tokens

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-farm-not-found (err u101))
(define-constant err-insufficient-stake (err u102))
(define-constant err-farm-ended (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-already-staked (err u105))
(define-constant err-no-rewards (err u106))
(define-constant err-farm-paused (err u107))

;; Data Variables
(define-data-var farm-nonce uint u0)
(define-data-var total-farms uint u0)

;; Farm configurations
(define-map farms uint
  {
    name: (string-ascii 64),
    lp-token: principal,
    reward-token: principal,
    reward-per-block: uint,
    total-staked: uint,
    total-rewards-distributed: uint,
    start-block: uint,
    end-block: uint,
    last-reward-block: uint,
    acc-reward-per-share: uint,
    paused: bool
  }
)

;; User stakes per farm
(define-map user-stakes { farm-id: uint, user: principal }
  {
    amount: uint,
    reward-debt: uint,
    pending-rewards: uint,
    staked-at: uint
  }
)

;; User farming stats
(define-map user-farm-stats principal
  {
    total-staked-value: uint,
    total-rewards-earned: uint,
    farms-participated: uint
  }
)

;; Precision for reward calculation
(define-constant precision u1000000000000)

;; Public Functions

(define-public (create-farm 
  (name (string-ascii 64))
  (lp-token principal)
  (reward-token principal)
  (reward-per-block uint)
  (duration uint)
  (total-rewards uint))
  (let (
    (farm-id (var-get farm-nonce))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> reward-per-block u0) err-invalid-amount)
    
    ;; Transfer reward tokens to contract
    (try! (stx-transfer? total-rewards tx-sender (as-contract tx-sender)))
    
    ;; Create farm
    (map-set farms farm-id
      {
        name: name,
        lp-token: lp-token,
        reward-token: reward-token,
        reward-per-block: reward-per-block,
        total-staked: u0,
        total-rewards-distributed: u0,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        last-reward-block: stacks-block-height,
        acc-reward-per-share: u0,
        paused: false
      }
    )
    
    (var-set farm-nonce (+ farm-id u1))
    (var-set total-farms (+ (var-get total-farms) u1))
    
    (ok { farm-id: farm-id, end-block: (+ stacks-block-height duration) })
  )
)

(define-public (stake (farm-id uint) (amount uint))
  (match (map-get? farms farm-id)
    farm
    (let (
      (user-stake (get-user-stake farm-id tx-sender))
      (pending (calculate-pending-rewards farm-id tx-sender))
    )
      (asserts! (not (get paused farm)) err-farm-paused)
      (asserts! (<= stacks-block-height (get end-block farm)) err-farm-ended)
      (asserts! (> amount u0) err-invalid-amount)
      
      ;; Update farm rewards
      (try! (update-farm-rewards farm-id))
      
      ;; Transfer LP tokens to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update user stake
      (map-set user-stakes { farm-id: farm-id, user: tx-sender }
        {
          amount: (+ (get amount user-stake) amount),
          reward-debt: (/ (* (+ (get amount user-stake) amount) (get acc-reward-per-share (unwrap-panic (map-get? farms farm-id)))) precision),
          pending-rewards: (+ (get pending-rewards user-stake) pending),
          staked-at: stacks-block-height
        }
      )
      
      ;; Update farm total staked
      (map-set farms farm-id
        (merge farm { total-staked: (+ (get total-staked farm) amount) })
      )
      
      ;; Update user stats
      (map-set user-farm-stats tx-sender
        (merge (get-user-farm-stats tx-sender)
          {
            total-staked-value: (+ (get total-staked-value (get-user-farm-stats tx-sender)) amount),
            farms-participated: (+ (get farms-participated (get-user-farm-stats tx-sender)) u1)
          }
        )
      )
      
      (ok { staked: amount, total-staked: (+ (get amount user-stake) amount) })
    )
    err-farm-not-found
  )
)

(define-public (unstake (farm-id uint) (amount uint))
  (match (map-get? farms farm-id)
    farm
    (let (
      (user-stake (get-user-stake farm-id tx-sender))
      (pending (calculate-pending-rewards farm-id tx-sender))
    )
      (asserts! (<= amount (get amount user-stake)) err-insufficient-stake)
      (asserts! (> amount u0) err-invalid-amount)
      
      ;; Update farm rewards
      (try! (update-farm-rewards farm-id))
      
      ;; Transfer LP tokens back to user
      (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
      
      ;; Update user stake
      (map-set user-stakes { farm-id: farm-id, user: tx-sender }
        {
          amount: (- (get amount user-stake) amount),
          reward-debt: (/ (* (- (get amount user-stake) amount) (get acc-reward-per-share (unwrap-panic (map-get? farms farm-id)))) precision),
          pending-rewards: (+ (get pending-rewards user-stake) pending),
          staked-at: (get staked-at user-stake)
        }
      )
      
      ;; Update farm total staked
      (map-set farms farm-id
        (merge farm { total-staked: (- (get total-staked farm) amount) })
      )
      
      ;; Update user stats
      (map-set user-farm-stats tx-sender
        (merge (get-user-farm-stats tx-sender)
          { total-staked-value: (- (get total-staked-value (get-user-farm-stats tx-sender)) amount) }
        )
      )
      
      (ok { unstaked: amount, remaining: (- (get amount user-stake) amount) })
    )
    err-farm-not-found
  )
)

(define-public (harvest (farm-id uint))
  (match (map-get? farms farm-id)
    farm
    (let (
      (user-stake (get-user-stake farm-id tx-sender))
      (pending (calculate-pending-rewards farm-id tx-sender))
      (total-rewards (+ pending (get pending-rewards user-stake)))
    )
      (asserts! (> total-rewards u0) err-no-rewards)
      
      ;; Update farm rewards
      (try! (update-farm-rewards farm-id))
      
      ;; Transfer rewards to user
      (try! (as-contract (stx-transfer? total-rewards tx-sender tx-sender)))
      
      ;; Update user stake
      (map-set user-stakes { farm-id: farm-id, user: tx-sender }
        (merge user-stake {
          reward-debt: (/ (* (get amount user-stake) (get acc-reward-per-share (unwrap-panic (map-get? farms farm-id)))) precision),
          pending-rewards: u0
        })
      )
      
      ;; Update farm stats
      (map-set farms farm-id
        (merge farm { total-rewards-distributed: (+ (get total-rewards-distributed farm) total-rewards) })
      )
      
      ;; Update user stats
      (map-set user-farm-stats tx-sender
        (merge (get-user-farm-stats tx-sender)
          { total-rewards-earned: (+ (get total-rewards-earned (get-user-farm-stats tx-sender)) total-rewards) }
        )
      )
      
      (ok { harvested: total-rewards })
    )
    err-farm-not-found
  )
)

(define-public (compound (farm-id uint))
  (match (map-get? farms farm-id)
    farm
    (let (
      (user-stake (get-user-stake farm-id tx-sender))
      (pending (calculate-pending-rewards farm-id tx-sender))
      (total-rewards (+ pending (get pending-rewards user-stake)))
    )
      (asserts! (not (get paused farm)) err-farm-paused)
      (asserts! (> total-rewards u0) err-no-rewards)
      
      ;; Update farm rewards
      (try! (update-farm-rewards farm-id))
      
      ;; Add rewards to stake (compound)
      (map-set user-stakes { farm-id: farm-id, user: tx-sender }
        {
          amount: (+ (get amount user-stake) total-rewards),
          reward-debt: (/ (* (+ (get amount user-stake) total-rewards) (get acc-reward-per-share (unwrap-panic (map-get? farms farm-id)))) precision),
          pending-rewards: u0,
          staked-at: (get staked-at user-stake)
        }
      )
      
      ;; Update farm total staked
      (map-set farms farm-id
        (merge farm { 
          total-staked: (+ (get total-staked farm) total-rewards),
          total-rewards-distributed: (+ (get total-rewards-distributed farm) total-rewards)
        })
      )
      
      (ok { compounded: total-rewards, new-stake: (+ (get amount user-stake) total-rewards) })
    )
    err-farm-not-found
  )
)

(define-private (update-farm-rewards (farm-id uint))
  (match (map-get? farms farm-id)
    farm
    (let (
      (current-block (if (> stacks-block-height (get end-block farm)) (get end-block farm) stacks-block-height))
      (blocks-elapsed (- current-block (get last-reward-block farm)))
      (reward (if (> (get total-staked farm) u0)
                (/ (* (* blocks-elapsed (get reward-per-block farm)) precision) (get total-staked farm))
                u0))
    )
      (map-set farms farm-id
        (merge farm {
          acc-reward-per-share: (+ (get acc-reward-per-share farm) reward),
          last-reward-block: current-block
        })
      )
      (ok true)
    )
    err-farm-not-found
  )
)

;; Read-only Functions

(define-read-only (get-farm (farm-id uint))
  (map-get? farms farm-id)
)

(define-read-only (get-user-stake (farm-id uint) (user principal))
  (default-to 
    { amount: u0, reward-debt: u0, pending-rewards: u0, staked-at: u0 }
    (map-get? user-stakes { farm-id: farm-id, user: user })
  )
)

(define-read-only (get-user-farm-stats (user principal))
  (default-to 
    { total-staked-value: u0, total-rewards-earned: u0, farms-participated: u0 }
    (map-get? user-farm-stats user)
  )
)

(define-read-only (calculate-pending-rewards (farm-id uint) (user principal))
  (match (map-get? farms farm-id)
    farm
    (let (
      (user-stake (get-user-stake farm-id user))
      (current-block (if (> stacks-block-height (get end-block farm)) (get end-block farm) stacks-block-height))
      (blocks-elapsed (- current-block (get last-reward-block farm)))
      (additional-reward (if (> (get total-staked farm) u0)
                           (/ (* (* blocks-elapsed (get reward-per-block farm)) precision) (get total-staked farm))
                           u0))
      (acc-reward (+ (get acc-reward-per-share farm) additional-reward))
    )
      (- (/ (* (get amount user-stake) acc-reward) precision) (get reward-debt user-stake))
    )
    u0
  )
)

(define-read-only (get-apr (farm-id uint))
  (match (map-get? farms farm-id)
    farm
    (if (> (get total-staked farm) u0)
      (/ (* (* (get reward-per-block farm) u144000 u365) u10000) (get total-staked farm)) ;; APR in basis points
      u0
    )
    u0
  )
)

(define-read-only (get-farm-info (farm-id uint))
  (match (map-get? farms farm-id)
    farm
    (ok {
      name: (get name farm),
      total-staked: (get total-staked farm),
      reward-per-block: (get reward-per-block farm),
      apr: (get-apr farm-id),
      end-block: (get end-block farm),
      blocks-remaining: (if (> (get end-block farm) stacks-block-height) 
                          (- (get end-block farm) stacks-block-height) 
                          u0),
      paused: (get paused farm)
    })
    err-farm-not-found
  )
)

(define-read-only (get-platform-stats)
  {
    total-farms: (var-get total-farms),
    farm-nonce: (var-get farm-nonce)
  }
)

;; Admin Functions

(define-public (toggle-farm-pause (farm-id uint))
  (match (map-get? farms farm-id)
    farm
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (map-set farms farm-id (merge farm { paused: (not (get paused farm)) }))
      (ok (not (get paused farm)))
    )
    err-farm-not-found
  )
)

(define-public (extend-farm (farm-id uint) (additional-blocks uint) (additional-rewards uint))
  (match (map-get? farms farm-id)
    farm
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (try! (stx-transfer? additional-rewards tx-sender (as-contract tx-sender)))
      (map-set farms farm-id
        (merge farm { end-block: (+ (get end-block farm) additional-blocks) })
      )
      (ok { new-end-block: (+ (get end-block farm) additional-blocks) })
    )
    err-farm-not-found
  )
)

(define-public (update-reward-rate (farm-id uint) (new-rate uint))
  (match (map-get? farms farm-id)
    farm
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (try! (update-farm-rewards farm-id))
      (map-set farms farm-id (merge farm { reward-per-block: new-rate }))
      (ok { new-rate: new-rate })
    )
    err-farm-not-found
  )
)

(define-public (emergency-withdraw-rewards (farm-id uint) (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)
