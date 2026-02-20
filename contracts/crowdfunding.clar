;; Crowdfunding Contract
;; Create and manage crowdfunding campaigns

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-campaign-not-found (err u101))
(define-constant err-campaign-ended (err u102))
(define-constant err-campaign-active (err u103))
(define-constant err-goal-not-met (err u104))
(define-constant err-already-claimed (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-not-contributor (err u107))
(define-constant err-goal-exceeded (err u108))

;; Platform fee: 3%
(define-constant platform-fee u300)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var campaign-nonce uint u0)
(define-data-var total-raised uint u0)
(define-data-var total-campaigns uint u0)

;; Campaigns
(define-map campaigns uint
  {
    creator: principal,
    title: (string-ascii 128),
    description: (string-utf8 1024),
    goal: uint,
    raised: uint,
    contributors-count: uint,
    start-block: uint,
    end-block: uint,
    claimed: bool,
    refunds-enabled: bool,
    flexible-funding: bool,
    category: (string-ascii 32)
  }
)

;; Contributions
(define-map contributions { campaign-id: uint, contributor: principal }
  {
    amount: uint,
    contributed-at: uint,
    refunded: bool
  }
)

;; Reward tiers
(define-map reward-tiers { campaign-id: uint, tier-id: uint }
  {
    name: (string-ascii 64),
    min-contribution: uint,
    description: (string-utf8 256),
    max-backers: uint,
    current-backers: uint
  }
)
(define-map tier-counts uint uint)

;; Backer rewards
(define-map backer-rewards { campaign-id: uint, backer: principal } uint)

;; Creator stats
(define-map creator-stats principal
  {
    campaigns-created: uint,
    total-raised: uint,
    successful-campaigns: uint
  }
)

;; Public Functions

(define-public (create-campaign
  (title (string-ascii 128))
  (description (string-utf8 1024))
  (goal uint)
  (duration uint)
  (flexible-funding bool)
  (category (string-ascii 32)))
  (let (
    (campaign-id (var-get campaign-nonce))
  )
    (asserts! (> goal u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    
    ;; Create campaign
    (map-set campaigns campaign-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        goal: goal,
        raised: u0,
        contributors-count: u0,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        claimed: false,
        refunds-enabled: false,
        flexible-funding: flexible-funding,
        category: category
      }
    )
    
    ;; Update creator stats
    (map-set creator-stats tx-sender
      (merge (get-creator-stats tx-sender)
        { campaigns-created: (+ (get campaigns-created (get-creator-stats tx-sender)) u1) }
      )
    )
    
    (var-set campaign-nonce (+ campaign-id u1))
    (var-set total-campaigns (+ (var-get total-campaigns) u1))
    
    (ok { campaign-id: campaign-id, end-block: (+ stacks-block-height duration) })
  )
)

(define-public (add-reward-tier
  (campaign-id uint)
  (name (string-ascii 64))
  (min-contribution uint)
  (description (string-utf8 256))
  (max-backers uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (let (
      (tier-id (default-to u0 (map-get? tier-counts campaign-id)))
    )
      (asserts! (is-eq (get creator campaign) tx-sender) err-owner-only)
      (asserts! (<= stacks-block-height (get end-block campaign)) err-campaign-ended)
      
      (map-set reward-tiers { campaign-id: campaign-id, tier-id: tier-id }
        {
          name: name,
          min-contribution: min-contribution,
          description: description,
          max-backers: max-backers,
          current-backers: u0
        }
      )
      (map-set tier-counts campaign-id (+ tier-id u1))
      
      (ok { tier-id: tier-id })
    )
    err-campaign-not-found
  )
)

(define-public (contribute (campaign-id uint) (amount uint) (tier-id (optional uint)))
  (match (map-get? campaigns campaign-id)
    campaign
    (let (
      (existing (get-contribution campaign-id tx-sender))
      (new-total (+ (get amount existing) amount))
    )
      (asserts! (<= stacks-block-height (get end-block campaign)) err-campaign-ended)
      (asserts! (> amount u0) err-invalid-amount)
      
      ;; Transfer contribution
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update contribution
      (map-set contributions { campaign-id: campaign-id, contributor: tx-sender }
        {
          amount: new-total,
          contributed-at: stacks-block-height,
          refunded: false
        }
      )
      
      ;; Update campaign
      (map-set campaigns campaign-id
        (merge campaign {
          raised: (+ (get raised campaign) amount),
          contributors-count: (if (is-eq (get amount existing) u0)
                               (+ (get contributors-count campaign) u1)
                               (get contributors-count campaign))
        })
      )
      
      ;; Handle reward tier
      (match tier-id
        tid
        (match (map-get? reward-tiers { campaign-id: campaign-id, tier-id: tid })
          tier
          (if (and 
                (>= new-total (get min-contribution tier))
                (< (get current-backers tier) (get max-backers tier)))
            (begin
              (map-set reward-tiers { campaign-id: campaign-id, tier-id: tid }
                (merge tier { current-backers: (+ (get current-backers tier) u1) })
              )
              (map-set backer-rewards { campaign-id: campaign-id, backer: tx-sender } tid)
            )
            true
          )
          true
        )
        true
      )
      
      (ok { campaign-id: campaign-id, contributed: amount, total: new-total })
    )
    err-campaign-not-found
  )
)

(define-public (claim-funds (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (let (
      (fee (calculate-fee (get raised campaign)))
      (creator-amount (- (get raised campaign) fee))
    )
      (asserts! (is-eq (get creator campaign) tx-sender) err-owner-only)
      (asserts! (> stacks-block-height (get end-block campaign)) err-campaign-active)
      (asserts! (not (get claimed campaign)) err-already-claimed)
      (asserts! (or 
        (get flexible-funding campaign)
        (>= (get raised campaign) (get goal campaign))
      ) err-goal-not-met)
      
      ;; Transfer funds to creator
      (try! (as-contract (stx-transfer? creator-amount tx-sender (get creator campaign))))
      
      ;; Update campaign
      (map-set campaigns campaign-id
        (merge campaign { claimed: true })
      )
      
      ;; Update stats
      (var-set total-raised (+ (var-get total-raised) (get raised campaign)))
      
      (map-set creator-stats (get creator campaign)
        (merge (get-creator-stats (get creator campaign))
          {
            total-raised: (+ (get total-raised (get-creator-stats (get creator campaign))) creator-amount),
            successful-campaigns: (+ (get successful-campaigns (get-creator-stats (get creator campaign))) u1)
          }
        )
      )
      
      (ok { claimed: creator-amount, fee: fee })
    )
    err-campaign-not-found
  )
)

(define-public (request-refund (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (let (
      (contribution (get-contribution campaign-id tx-sender))
    )
      (asserts! (> stacks-block-height (get end-block campaign)) err-campaign-active)
      (asserts! (not (get claimed campaign)) err-already-claimed)
      (asserts! (< (get raised campaign) (get goal campaign)) err-goal-exceeded)
      (asserts! (not (get flexible-funding campaign)) err-goal-exceeded)
      (asserts! (> (get amount contribution) u0) err-not-contributor)
      (asserts! (not (get refunded contribution)) err-already-claimed)
      
      ;; Refund contribution
      (try! (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender)))
      
      ;; Mark as refunded
      (map-set contributions { campaign-id: campaign-id, contributor: tx-sender }
        (merge contribution { refunded: true })
      )
      
      (ok { refunded: (get amount contribution) })
    )
    err-campaign-not-found
  )
)

(define-public (enable-refunds (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (begin
      (asserts! (is-eq (get creator campaign) tx-sender) err-owner-only)
      (asserts! (not (get claimed campaign)) err-already-claimed)
      
      (map-set campaigns campaign-id
        (merge campaign { refunds-enabled: true })
      )
      
      (ok { refunds-enabled: true })
    )
    err-campaign-not-found
  )
)

(define-public (extend-campaign (campaign-id uint) (additional-blocks uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (begin
      (asserts! (is-eq (get creator campaign) tx-sender) err-owner-only)
      (asserts! (<= stacks-block-height (get end-block campaign)) err-campaign-ended)
      
      (map-set campaigns campaign-id
        (merge campaign { end-block: (+ (get end-block campaign) additional-blocks) })
      )
      
      (ok { new-end-block: (+ (get end-block campaign) additional-blocks) })
    )
    err-campaign-not-found
  )
)

;; Read-only Functions

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id)
)

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (default-to 
    { amount: u0, contributed-at: u0, refunded: false }
    (map-get? contributions { campaign-id: campaign-id, contributor: contributor })
  )
)

(define-read-only (get-reward-tier (campaign-id uint) (tier-id uint))
  (map-get? reward-tiers { campaign-id: campaign-id, tier-id: tier-id })
)

(define-read-only (get-backer-reward (campaign-id uint) (backer principal))
  (map-get? backer-rewards { campaign-id: campaign-id, backer: backer })
)

(define-read-only (get-creator-stats (creator principal))
  (default-to 
    { campaigns-created: u0, total-raised: u0, successful-campaigns: u0 }
    (map-get? creator-stats creator)
  )
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount platform-fee) fee-denominator)
)

(define-read-only (get-campaign-status (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (if (get claimed campaign)
      "claimed"
      (if (<= stacks-block-height (get end-block campaign))
        (if (>= (get raised campaign) (get goal campaign))
          "funded"
          "active"
        )
        (if (>= (get raised campaign) (get goal campaign))
          "successful"
          (if (get flexible-funding campaign)
            "flexible-ended"
            "failed"
          )
        )
      )
    )
    "not-found"
  )
)

(define-read-only (get-campaign-progress (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
    (ok {
      raised: (get raised campaign),
      goal: (get goal campaign),
      percentage: (/ (* (get raised campaign) u10000) (get goal campaign)),
      contributors: (get contributors-count campaign),
      blocks-remaining: (if (> (get end-block campaign) stacks-block-height)
                          (- (get end-block campaign) stacks-block-height)
                          u0),
      is-funded: (>= (get raised campaign) (get goal campaign))
    })
    err-campaign-not-found
  )
)

(define-read-only (get-platform-stats)
  {
    total-campaigns: (var-get total-campaigns),
    total-raised: (var-get total-raised),
    campaign-nonce: (var-get campaign-nonce),
    platform-fee: platform-fee
  }
)

(define-read-only (get-tier-count (campaign-id uint))
  (default-to u0 (map-get? tier-counts campaign-id))
)

;; Admin Functions

(define-public (withdraw-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)
