;; Token Swap Contract
;; Atomic swaps between different tokens

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-swap-not-found (err u101))
(define-constant err-swap-expired (err u102))
(define-constant err-swap-completed (err u103))
(define-constant err-not-participant (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-insufficient-balance (err u106))
(define-constant err-already-accepted (err u107))
(define-constant err-swap-cancelled (err u108))

;; Platform fee: 0.3%
(define-constant platform-fee u30)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var swap-nonce uint u0)
(define-data-var total-swaps uint u0)
(define-data-var total-volume uint u0)

;; Swap orders
(define-map swaps uint
  {
    creator: principal,
    offer-token: principal,
    offer-amount: uint,
    want-token: principal,
    want-amount: uint,
    counterparty: (optional principal),
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20)
  }
)

;; User swap history
(define-map user-swaps principal
  {
    created: uint,
    completed: uint,
    cancelled: uint,
    volume: uint
  }
)

;; Supported tokens
(define-map supported-tokens principal bool)

;; Public Functions

(define-public (create-swap
  (offer-token principal)
  (offer-amount uint)
  (want-token principal)
  (want-amount uint)
  (duration uint)
  (counterparty (optional principal)))
  (let (
    (swap-id (var-get swap-nonce))
  )
    (asserts! (> offer-amount u0) err-invalid-amount)
    (asserts! (> want-amount u0) err-invalid-amount)
    
    ;; Lock offered tokens in contract
    (try! (stx-transfer? offer-amount tx-sender (as-contract tx-sender)))
    
    ;; Create swap
    (map-set swaps swap-id
      {
        creator: tx-sender,
        offer-token: offer-token,
        offer-amount: offer-amount,
        want-token: want-token,
        want-amount: want-amount,
        counterparty: counterparty,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        status: "open"
      }
    )
    
    ;; Update user stats
    (map-set user-swaps tx-sender
      (merge (get-user-stats tx-sender)
        { created: (+ (get created (get-user-stats tx-sender)) u1) }
      )
    )
    
    (var-set swap-nonce (+ swap-id u1))
    
    (ok { swap-id: swap-id, expires-at: (+ stacks-block-height duration) })
  )
)

(define-public (accept-swap (swap-id uint))
  (match (map-get? swaps swap-id)
    swap
    (let (
      (fee (calculate-fee (get want-amount swap)))
      (creator-receives (- (get want-amount swap) fee))
    )
      (asserts! (is-eq (get status swap) "open") err-swap-completed)
      (asserts! (<= stacks-block-height (get expires-at swap)) err-swap-expired)
      (asserts! (or 
        (is-none (get counterparty swap))
        (is-eq (some tx-sender) (get counterparty swap))
      ) err-not-participant)
      
      ;; Transfer want tokens from accepter to creator
      (try! (stx-transfer? creator-receives tx-sender (get creator swap)))
      (try! (stx-transfer? fee tx-sender contract-owner))
      
      ;; Transfer offer tokens from contract to accepter
      (try! (as-contract (stx-transfer? (get offer-amount swap) tx-sender tx-sender)))
      
      ;; Update swap status
      (map-set swaps swap-id
        (merge swap { status: "completed" })
      )
      
      ;; Update stats
      (var-set total-swaps (+ (var-get total-swaps) u1))
      (var-set total-volume (+ (var-get total-volume) (get offer-amount swap)))
      
      (map-set user-swaps (get creator swap)
        (merge (get-user-stats (get creator swap))
          { 
            completed: (+ (get completed (get-user-stats (get creator swap))) u1),
            volume: (+ (get volume (get-user-stats (get creator swap))) (get offer-amount swap))
          }
        )
      )
      
      (map-set user-swaps tx-sender
        (merge (get-user-stats tx-sender)
          { 
            completed: (+ (get completed (get-user-stats tx-sender)) u1),
            volume: (+ (get volume (get-user-stats tx-sender)) (get want-amount swap))
          }
        )
      )
      
      (ok { swap-id: swap-id, offer-received: (get offer-amount swap), want-paid: (get want-amount swap) })
    )
    err-swap-not-found
  )
)

(define-public (cancel-swap (swap-id uint))
  (match (map-get? swaps swap-id)
    swap
    (begin
      (asserts! (is-eq (get creator swap) tx-sender) err-not-participant)
      (asserts! (is-eq (get status swap) "open") err-swap-completed)
      
      ;; Return locked tokens to creator
      (try! (as-contract (stx-transfer? (get offer-amount swap) tx-sender (get creator swap))))
      
      ;; Update swap status
      (map-set swaps swap-id
        (merge swap { status: "cancelled" })
      )
      
      ;; Update user stats
      (map-set user-swaps tx-sender
        (merge (get-user-stats tx-sender)
          { cancelled: (+ (get cancelled (get-user-stats tx-sender)) u1) }
        )
      )
      
      (ok { swap-id: swap-id, refunded: (get offer-amount swap) })
    )
    err-swap-not-found
  )
)

(define-public (claim-expired-swap (swap-id uint))
  (match (map-get? swaps swap-id)
    swap
    (begin
      (asserts! (is-eq (get creator swap) tx-sender) err-not-participant)
      (asserts! (is-eq (get status swap) "open") err-swap-completed)
      (asserts! (> stacks-block-height (get expires-at swap)) err-swap-not-found)
      
      ;; Return locked tokens to creator
      (try! (as-contract (stx-transfer? (get offer-amount swap) tx-sender (get creator swap))))
      
      ;; Update swap status
      (map-set swaps swap-id
        (merge swap { status: "expired" })
      )
      
      (ok { swap-id: swap-id, refunded: (get offer-amount swap) })
    )
    err-swap-not-found
  )
)

(define-public (update-swap-counterparty (swap-id uint) (new-counterparty (optional principal)))
  (match (map-get? swaps swap-id)
    swap
    (begin
      (asserts! (is-eq (get creator swap) tx-sender) err-not-participant)
      (asserts! (is-eq (get status swap) "open") err-swap-completed)
      
      (map-set swaps swap-id
        (merge swap { counterparty: new-counterparty })
      )
      
      (ok { swap-id: swap-id, new-counterparty: new-counterparty })
    )
    err-swap-not-found
  )
)

;; Read-only Functions

(define-read-only (get-swap (swap-id uint))
  (map-get? swaps swap-id)
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { created: u0, completed: u0, cancelled: u0, volume: u0 }
    (map-get? user-swaps user)
  )
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount platform-fee) fee-denominator)
)

(define-read-only (get-swap-rate (swap-id uint))
  (match (map-get? swaps swap-id)
    swap
    (ok {
      offer-per-want: (/ (* (get offer-amount swap) u1000000) (get want-amount swap)),
      want-per-offer: (/ (* (get want-amount swap) u1000000) (get offer-amount swap))
    })
    err-swap-not-found
  )
)

(define-read-only (is-swap-active (swap-id uint))
  (match (map-get? swaps swap-id)
    swap
    (and 
      (is-eq (get status swap) "open")
      (<= stacks-block-height (get expires-at swap))
    )
    false
  )
)

(define-read-only (can-accept-swap (swap-id uint) (accepter principal))
  (match (map-get? swaps swap-id)
    swap
    (and
      (is-eq (get status swap) "open")
      (<= stacks-block-height (get expires-at swap))
      (or 
        (is-none (get counterparty swap))
        (is-eq (some accepter) (get counterparty swap))
      )
    )
    false
  )
)

(define-read-only (get-platform-stats)
  {
    total-swaps-created: (var-get swap-nonce),
    total-swaps-completed: (var-get total-swaps),
    total-volume: (var-get total-volume),
    platform-fee: platform-fee
  }
)

(define-read-only (is-token-supported (token principal))
  (default-to false (map-get? supported-tokens token))
)

;; Admin Functions

(define-public (add-supported-token (token principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set supported-tokens token true)
    (ok true)
  )
)

(define-public (remove-supported-token (token principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete supported-tokens token)
    (ok true)
  )
)
