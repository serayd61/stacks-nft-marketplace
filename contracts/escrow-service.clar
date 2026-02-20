;; Escrow Service Contract
;; Secure escrow for peer-to-peer transactions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-escrow-not-found (err u101))
(define-constant err-not-participant (err u102))
(define-constant err-invalid-state (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-deadline-passed (err u105))
(define-constant err-dispute-active (err u106))
(define-constant err-already-released (err u107))

;; Escrow states
(define-constant state-created u0)
(define-constant state-funded u1)
(define-constant state-delivered u2)
(define-constant state-disputed u3)
(define-constant state-released u4)
(define-constant state-refunded u5)
(define-constant state-cancelled u6)

;; Platform fee: 1%
(define-constant platform-fee u100)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var escrow-nonce uint u0)
(define-data-var total-volume uint u0)
(define-data-var total-fees uint u0)

;; Escrows
(define-map escrows uint
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    description: (string-utf8 256),
    state: uint,
    created-at: uint,
    deadline: uint,
    delivery-deadline: uint,
    arbiter: (optional principal),
    dispute-reason: (optional (string-utf8 256))
  }
)

;; Milestones for complex escrows
(define-map milestones { escrow-id: uint, milestone-id: uint }
  {
    description: (string-ascii 128),
    amount: uint,
    completed: bool,
    released: bool
  }
)
(define-map milestone-counts uint uint)

;; User stats
(define-map user-stats principal
  {
    escrows-as-buyer: uint,
    escrows-as-seller: uint,
    total-volume: uint,
    disputes: uint,
    reputation: uint
  }
)

;; Arbiters
(define-map arbiters principal
  {
    active: bool,
    cases-handled: uint,
    fee-percentage: uint
  }
)

;; Public Functions

(define-public (create-escrow
  (seller principal)
  (amount uint)
  (description (string-utf8 256))
  (delivery-deadline uint)
  (arbiter (optional principal)))
  (let (
    (escrow-id (var-get escrow-nonce))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq tx-sender seller)) err-not-participant)
    
    ;; Create escrow
    (map-set escrows escrow-id
      {
        buyer: tx-sender,
        seller: seller,
        amount: amount,
        description: description,
        state: state-created,
        created-at: stacks-block-height,
        deadline: (+ stacks-block-height u43200), ;; 30 days max
        delivery-deadline: (+ stacks-block-height delivery-deadline),
        arbiter: arbiter,
        dispute-reason: none
      }
    )
    
    ;; Update user stats
    (map-set user-stats tx-sender
      (merge (get-user-stats tx-sender)
        { escrows-as-buyer: (+ (get escrows-as-buyer (get-user-stats tx-sender)) u1) }
      )
    )
    (map-set user-stats seller
      (merge (get-user-stats seller)
        { escrows-as-seller: (+ (get escrows-as-seller (get-user-stats seller)) u1) }
      )
    )
    
    (var-set escrow-nonce (+ escrow-id u1))
    
    (ok { escrow-id: escrow-id })
  )
)

(define-public (fund-escrow (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (begin
      (asserts! (is-eq (get buyer escrow) tx-sender) err-not-participant)
      (asserts! (is-eq (get state escrow) state-created) err-invalid-state)
      
      ;; Transfer funds to contract
      (try! (stx-transfer? (get amount escrow) tx-sender (as-contract tx-sender)))
      
      ;; Update state
      (map-set escrows escrow-id
        (merge escrow { state: state-funded })
      )
      
      (ok { escrow-id: escrow-id, funded: true })
    )
    err-escrow-not-found
  )
)

(define-public (mark-delivered (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (begin
      (asserts! (is-eq (get seller escrow) tx-sender) err-not-participant)
      (asserts! (is-eq (get state escrow) state-funded) err-invalid-state)
      
      ;; Update state
      (map-set escrows escrow-id
        (merge escrow { state: state-delivered })
      )
      
      (ok { escrow-id: escrow-id, delivered: true })
    )
    err-escrow-not-found
  )
)

(define-public (confirm-delivery (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (let (
      (fee (calculate-fee (get amount escrow)))
      (seller-amount (- (get amount escrow) fee))
    )
      (asserts! (is-eq (get buyer escrow) tx-sender) err-not-participant)
      (asserts! (or (is-eq (get state escrow) state-funded) (is-eq (get state escrow) state-delivered)) err-invalid-state)
      
      ;; Transfer to seller
      (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller escrow))))
      
      ;; Update state
      (map-set escrows escrow-id
        (merge escrow { state: state-released })
      )
      
      ;; Update stats
      (var-set total-volume (+ (var-get total-volume) (get amount escrow)))
      (var-set total-fees (+ (var-get total-fees) fee))
      
      (map-set user-stats (get seller escrow)
        (merge (get-user-stats (get seller escrow))
          { 
            total-volume: (+ (get total-volume (get-user-stats (get seller escrow))) (get amount escrow)),
            reputation: (+ (get reputation (get-user-stats (get seller escrow))) u10)
          }
        )
      )
      
      (ok { escrow-id: escrow-id, released: seller-amount, fee: fee })
    )
    err-escrow-not-found
  )
)

(define-public (open-dispute (escrow-id uint) (reason (string-utf8 256)))
  (match (map-get? escrows escrow-id)
    escrow
    (begin
      (asserts! (or (is-eq (get buyer escrow) tx-sender) (is-eq (get seller escrow) tx-sender)) err-not-participant)
      (asserts! (or (is-eq (get state escrow) state-funded) (is-eq (get state escrow) state-delivered)) err-invalid-state)
      
      ;; Update state
      (map-set escrows escrow-id
        (merge escrow { 
          state: state-disputed,
          dispute-reason: (some reason)
        })
      )
      
      ;; Update stats
      (map-set user-stats tx-sender
        (merge (get-user-stats tx-sender)
          { disputes: (+ (get disputes (get-user-stats tx-sender)) u1) }
        )
      )
      
      (ok { escrow-id: escrow-id, disputed: true })
    )
    err-escrow-not-found
  )
)

(define-public (resolve-dispute (escrow-id uint) (release-to-seller bool) (seller-percentage uint))
  (match (map-get? escrows escrow-id)
    escrow
    (let (
      (arbiter-addr (unwrap! (get arbiter escrow) err-not-participant))
    )
      (asserts! (is-eq tx-sender arbiter-addr) err-not-participant)
      (asserts! (is-eq (get state escrow) state-disputed) err-invalid-state)
      
      (if release-to-seller
        ;; Release to seller (full or partial)
        (let (
          (seller-amount (/ (* (get amount escrow) seller-percentage) u10000))
          (buyer-refund (- (get amount escrow) seller-amount))
        )
          (if (> seller-amount u0)
            (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller escrow))))
            true
          )
          (if (> buyer-refund u0)
            (try! (as-contract (stx-transfer? buyer-refund tx-sender (get buyer escrow))))
            true
          )
          (map-set escrows escrow-id
            (merge escrow { state: state-released })
          )
          (ok { escrow-id: escrow-id, seller-received: seller-amount, buyer-refunded: buyer-refund })
        )
        ;; Full refund to buyer
        (begin
          (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
          (map-set escrows escrow-id
            (merge escrow { state: state-refunded })
          )
          (ok { escrow-id: escrow-id, seller-received: u0, buyer-refunded: (get amount escrow) })
        )
      )
    )
    err-escrow-not-found
  )
)

(define-public (request-refund (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (begin
      (asserts! (is-eq (get buyer escrow) tx-sender) err-not-participant)
      (asserts! (is-eq (get state escrow) state-funded) err-invalid-state)
      (asserts! (> stacks-block-height (get delivery-deadline escrow)) err-deadline-passed)
      
      ;; Refund buyer
      (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
      
      ;; Update state
      (map-set escrows escrow-id
        (merge escrow { state: state-refunded })
      )
      
      (ok { escrow-id: escrow-id, refunded: (get amount escrow) })
    )
    err-escrow-not-found
  )
)

(define-public (cancel-escrow (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (begin
      (asserts! (is-eq (get buyer escrow) tx-sender) err-not-participant)
      (asserts! (is-eq (get state escrow) state-created) err-invalid-state)
      
      ;; Update state
      (map-set escrows escrow-id
        (merge escrow { state: state-cancelled })
      )
      
      (ok { escrow-id: escrow-id, cancelled: true })
    )
    err-escrow-not-found
  )
)

;; Read-only Functions

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { escrows-as-buyer: u0, escrows-as-seller: u0, total-volume: u0, disputes: u0, reputation: u100 }
    (map-get? user-stats user)
  )
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount platform-fee) fee-denominator)
)

(define-read-only (get-escrow-state-name (state uint))
  (if (is-eq state state-created) "created"
    (if (is-eq state state-funded) "funded"
      (if (is-eq state state-delivered) "delivered"
        (if (is-eq state state-disputed) "disputed"
          (if (is-eq state state-released) "released"
            (if (is-eq state state-refunded) "refunded"
              (if (is-eq state state-cancelled) "cancelled"
                "unknown"
              )
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-platform-stats)
  {
    total-escrows: (var-get escrow-nonce),
    total-volume: (var-get total-volume),
    total-fees: (var-get total-fees),
    platform-fee: platform-fee
  }
)

(define-read-only (is-arbiter (account principal))
  (match (map-get? arbiters account)
    arbiter (get active arbiter)
    false
  )
)

;; Admin Functions

(define-public (add-arbiter (arbiter principal) (fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set arbiters arbiter
      { active: true, cases-handled: u0, fee-percentage: fee-percentage }
    )
    (ok true)
  )
)

(define-public (remove-arbiter (arbiter principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete arbiters arbiter)
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)
