;; Subscription Service Contract
;; Recurring payments and subscription management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-plan-not-found (err u101))
(define-constant err-subscription-not-found (err u102))
(define-constant err-already-subscribed (err u103))
(define-constant err-subscription-expired (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-not-subscriber (err u107))

;; Billing periods (in blocks)
(define-constant period-weekly u1008)
(define-constant period-monthly u4320)
(define-constant period-quarterly u12960)
(define-constant period-yearly u52560)

;; Data Variables
(define-data-var plan-nonce uint u0)
(define-data-var subscription-nonce uint u0)
(define-data-var total-revenue uint u0)

;; Subscription plans
(define-map plans uint
  {
    creator: principal,
    name: (string-ascii 64),
    description: (string-utf8 256),
    price: uint,
    billing-period: uint,
    max-subscribers: uint,
    current-subscribers: uint,
    features: (list 10 (string-ascii 64)),
    active: bool,
    created-at: uint
  }
)

;; Subscriptions
(define-map subscriptions uint
  {
    subscriber: principal,
    plan-id: uint,
    started-at: uint,
    current-period-start: uint,
    current-period-end: uint,
    total-paid: uint,
    payments-count: uint,
    auto-renew: bool,
    cancelled: bool,
    cancelled-at: (optional uint)
  }
)

;; User to subscription mapping
(define-map user-subscriptions { user: principal, plan-id: uint } uint)

;; Plan creator revenue
(define-map creator-revenue principal uint)

;; Subscriber stats
(define-map subscriber-stats principal
  {
    active-subscriptions: uint,
    total-spent: uint,
    subscription-count: uint
  }
)

;; Public Functions

(define-public (create-plan
  (name (string-ascii 64))
  (description (string-utf8 256))
  (price uint)
  (billing-period uint)
  (max-subscribers uint)
  (features (list 10 (string-ascii 64))))
  (let (
    (plan-id (var-get plan-nonce))
  )
    (asserts! (> price u0) err-invalid-amount)
    (asserts! (> billing-period u0) err-invalid-amount)
    
    ;; Create plan
    (map-set plans plan-id
      {
        creator: tx-sender,
        name: name,
        description: description,
        price: price,
        billing-period: billing-period,
        max-subscribers: max-subscribers,
        current-subscribers: u0,
        features: features,
        active: true,
        created-at: stacks-block-height
      }
    )
    
    (var-set plan-nonce (+ plan-id u1))
    
    (ok { plan-id: plan-id })
  )
)

(define-public (subscribe (plan-id uint) (auto-renew bool))
  (match (map-get? plans plan-id)
    plan
    (let (
      (subscription-id (var-get subscription-nonce))
      (period-end (+ stacks-block-height (get billing-period plan)))
    )
      (asserts! (get active plan) err-plan-not-found)
      (asserts! (< (get current-subscribers plan) (get max-subscribers plan)) err-plan-not-found)
      (asserts! (is-none (map-get? user-subscriptions { user: tx-sender, plan-id: plan-id })) err-already-subscribed)
      
      ;; Pay subscription fee
      (try! (stx-transfer? (get price plan) tx-sender (as-contract tx-sender)))
      
      ;; Create subscription
      (map-set subscriptions subscription-id
        {
          subscriber: tx-sender,
          plan-id: plan-id,
          started-at: stacks-block-height,
          current-period-start: stacks-block-height,
          current-period-end: period-end,
          total-paid: (get price plan),
          payments-count: u1,
          auto-renew: auto-renew,
          cancelled: false,
          cancelled-at: none
        }
      )
      
      ;; Map user to subscription
      (map-set user-subscriptions { user: tx-sender, plan-id: plan-id } subscription-id)
      
      ;; Update plan subscriber count
      (map-set plans plan-id
        (merge plan { current-subscribers: (+ (get current-subscribers plan) u1) })
      )
      
      ;; Update creator revenue
      (map-set creator-revenue (get creator plan)
        (+ (default-to u0 (map-get? creator-revenue (get creator plan))) (get price plan))
      )
      
      ;; Update subscriber stats
      (map-set subscriber-stats tx-sender
        (merge (get-subscriber-stats tx-sender)
          {
            active-subscriptions: (+ (get active-subscriptions (get-subscriber-stats tx-sender)) u1),
            total-spent: (+ (get total-spent (get-subscriber-stats tx-sender)) (get price plan)),
            subscription-count: (+ (get subscription-count (get-subscriber-stats tx-sender)) u1)
          }
        )
      )
      
      (var-set subscription-nonce (+ subscription-id u1))
      (var-set total-revenue (+ (var-get total-revenue) (get price plan)))
      
      (ok { subscription-id: subscription-id, expires-at: period-end })
    )
    err-plan-not-found
  )
)

(define-public (renew-subscription (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (match (map-get? plans (get plan-id subscription))
      plan
      (let (
        (new-period-start (if (> (get current-period-end subscription) stacks-block-height)
                            (get current-period-end subscription)
                            stacks-block-height))
        (new-period-end (+ new-period-start (get billing-period plan)))
      )
        (asserts! (is-eq (get subscriber subscription) tx-sender) err-not-subscriber)
        (asserts! (not (get cancelled subscription)) err-subscription-expired)
        (asserts! (get active plan) err-plan-not-found)
        
        ;; Pay renewal fee
        (try! (stx-transfer? (get price plan) tx-sender (as-contract tx-sender)))
        
        ;; Update subscription
        (map-set subscriptions subscription-id
          (merge subscription {
            current-period-start: new-period-start,
            current-period-end: new-period-end,
            total-paid: (+ (get total-paid subscription) (get price plan)),
            payments-count: (+ (get payments-count subscription) u1)
          })
        )
        
        ;; Update creator revenue
        (map-set creator-revenue (get creator plan)
          (+ (default-to u0 (map-get? creator-revenue (get creator plan))) (get price plan))
        )
        
        ;; Update subscriber stats
        (map-set subscriber-stats tx-sender
          (merge (get-subscriber-stats tx-sender)
            { total-spent: (+ (get total-spent (get-subscriber-stats tx-sender)) (get price plan)) }
          )
        )
        
        (var-set total-revenue (+ (var-get total-revenue) (get price plan)))
        
        (ok { subscription-id: subscription-id, new-expiry: new-period-end })
      )
      err-plan-not-found
    )
    err-subscription-not-found
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (match (map-get? plans (get plan-id subscription))
      plan
      (begin
        (asserts! (is-eq (get subscriber subscription) tx-sender) err-not-subscriber)
        (asserts! (not (get cancelled subscription)) err-subscription-expired)
        
        ;; Update subscription
        (map-set subscriptions subscription-id
          (merge subscription {
            cancelled: true,
            cancelled-at: (some stacks-block-height),
            auto-renew: false
          })
        )
        
        ;; Update plan subscriber count
        (map-set plans (get plan-id subscription)
          (merge plan { current-subscribers: (- (get current-subscribers plan) u1) })
        )
        
        ;; Update subscriber stats
        (map-set subscriber-stats tx-sender
          (merge (get-subscriber-stats tx-sender)
            { active-subscriptions: (- (get active-subscriptions (get-subscriber-stats tx-sender)) u1) }
          )
        )
        
        (ok { subscription-id: subscription-id, cancelled: true, active-until: (get current-period-end subscription) })
      )
      err-plan-not-found
    )
    err-subscription-not-found
  )
)

(define-public (toggle-auto-renew (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (begin
      (asserts! (is-eq (get subscriber subscription) tx-sender) err-not-subscriber)
      
      (map-set subscriptions subscription-id
        (merge subscription { auto-renew: (not (get auto-renew subscription)) })
      )
      
      (ok { auto-renew: (not (get auto-renew subscription)) })
    )
    err-subscription-not-found
  )
)

(define-public (withdraw-revenue)
  (let (
    (revenue (default-to u0 (map-get? creator-revenue tx-sender)))
  )
    (asserts! (> revenue u0) err-invalid-amount)
    
    ;; Transfer revenue
    (try! (as-contract (stx-transfer? revenue tx-sender tx-sender)))
    
    ;; Clear revenue
    (map-set creator-revenue tx-sender u0)
    
    (ok { withdrawn: revenue })
  )
)

(define-public (update-plan (plan-id uint) (new-price uint) (new-max uint))
  (match (map-get? plans plan-id)
    plan
    (begin
      (asserts! (is-eq (get creator plan) tx-sender) err-owner-only)
      
      (map-set plans plan-id
        (merge plan {
          price: new-price,
          max-subscribers: new-max
        })
      )
      
      (ok { plan-id: plan-id, updated: true })
    )
    err-plan-not-found
  )
)

(define-public (deactivate-plan (plan-id uint))
  (match (map-get? plans plan-id)
    plan
    (begin
      (asserts! (is-eq (get creator plan) tx-sender) err-owner-only)
      
      (map-set plans plan-id
        (merge plan { active: false })
      )
      
      (ok { plan-id: plan-id, deactivated: true })
    )
    err-plan-not-found
  )
)

;; Read-only Functions

(define-read-only (get-plan (plan-id uint))
  (map-get? plans plan-id)
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions subscription-id)
)

(define-read-only (get-user-subscription (user principal) (plan-id uint))
  (map-get? user-subscriptions { user: user, plan-id: plan-id })
)

(define-read-only (get-subscriber-stats (subscriber principal))
  (default-to 
    { active-subscriptions: u0, total-spent: u0, subscription-count: u0 }
    (map-get? subscriber-stats subscriber)
  )
)

(define-read-only (get-creator-revenue (creator principal))
  (default-to u0 (map-get? creator-revenue creator))
)

(define-read-only (is-subscription-active (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (and 
      (not (get cancelled subscription))
      (>= (get current-period-end subscription) stacks-block-height)
    )
    false
  )
)

(define-read-only (has-active-subscription (user principal) (plan-id uint))
  (match (map-get? user-subscriptions { user: user, plan-id: plan-id })
    subscription-id
    (is-subscription-active subscription-id)
    false
  )
)

(define-read-only (get-subscription-status (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (if (get cancelled subscription)
      (if (>= (get current-period-end subscription) stacks-block-height)
        "cancelled-active"
        "cancelled-expired"
      )
      (if (>= (get current-period-end subscription) stacks-block-height)
        "active"
        "expired"
      )
    )
    "not-found"
  )
)

(define-read-only (get-platform-stats)
  {
    total-plans: (var-get plan-nonce),
    total-subscriptions: (var-get subscription-nonce),
    total-revenue: (var-get total-revenue)
  }
)

(define-read-only (calculate-renewal-date (subscription-id uint))
  (match (map-get? subscriptions subscription-id)
    subscription
    (ok (get current-period-end subscription))
    err-subscription-not-found
  )
)

;; Admin Functions

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)
