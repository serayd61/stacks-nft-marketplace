;; DAO Treasury Contract
;; Manage DAO funds with governance-controlled spending

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-request-not-found (err u104))
(define-constant err-already-processed (err u105))
(define-constant err-spending-limit (err u106))
(define-constant err-cooldown-active (err u107))

;; Spending limits
(define-constant daily-limit u100000000000) ;; 100,000 STX daily
(define-constant single-tx-limit u10000000000) ;; 10,000 STX per transaction

;; Data Variables
(define-data-var total-balance uint u0)
(define-data-var total-spent uint u0)
(define-data-var request-nonce uint u0)
(define-data-var governance-contract (optional principal) none)
(define-data-var daily-spent uint u0)
(define-data-var last-reset-block uint u0)

;; Spending requests
(define-map spending-requests uint
  {
    requester: principal,
    recipient: principal,
    amount: uint,
    reason: (string-utf8 256),
    category: (string-ascii 32),
    created-at: uint,
    approved: bool,
    executed: bool,
    approvals: uint,
    rejections: uint
  }
)

;; Authorized spenders
(define-map authorized-spenders principal
  {
    spending-limit: uint,
    spent-today: uint,
    last-spend-block: uint,
    active: bool
  }
)

;; Budget allocations by category
(define-map budget-allocations (string-ascii 32)
  {
    allocated: uint,
    spent: uint,
    remaining: uint
  }
)

;; Signers for multi-sig
(define-map signers principal bool)
(define-data-var required-signatures uint u2)

;; Request approvals
(define-map request-approvals { request-id: uint, signer: principal } bool)

;; Transaction history
(define-map transactions uint
  {
    tx-type: (string-ascii 20),
    amount: uint,
    recipient: principal,
    category: (string-ascii 32),
    executed-at: uint,
    executed-by: principal
  }
)
(define-data-var tx-nonce uint u0)

;; Public Functions

(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set total-balance (+ (var-get total-balance) amount))
    
    ;; Record transaction
    (record-transaction "deposit" amount tx-sender "general")
    
    (ok { deposited: amount, new-balance: (var-get total-balance) })
  )
)

(define-public (create-spending-request 
  (recipient principal)
  (amount uint)
  (reason (string-utf8 256))
  (category (string-ascii 32)))
  (let (
    (request-id (var-get request-nonce))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount (var-get total-balance)) err-insufficient-funds)
    
    ;; Create request
    (map-set spending-requests request-id
      {
        requester: tx-sender,
        recipient: recipient,
        amount: amount,
        reason: reason,
        category: category,
        created-at: stacks-block-height,
        approved: false,
        executed: false,
        approvals: u0,
        rejections: u0
      }
    )
    
    (var-set request-nonce (+ request-id u1))
    
    (ok { request-id: request-id })
  )
)

(define-public (approve-request (request-id uint))
  (match (map-get? spending-requests request-id)
    request
    (begin
      (asserts! (is-signer tx-sender) err-not-authorized)
      (asserts! (not (get executed request)) err-already-processed)
      (asserts! (is-none (map-get? request-approvals { request-id: request-id, signer: tx-sender })) err-already-processed)
      
      ;; Record approval
      (map-set request-approvals { request-id: request-id, signer: tx-sender } true)
      
      ;; Update request
      (let (
        (new-approvals (+ (get approvals request) u1))
      )
        (map-set spending-requests request-id
          (merge request { approvals: new-approvals })
        )
        
        ;; Check if enough approvals
        (if (>= new-approvals (var-get required-signatures))
          (begin
            (map-set spending-requests request-id
              (merge request { approved: true, approvals: new-approvals })
            )
            (ok { request-id: request-id, approved: true, can-execute: true })
          )
          (ok { request-id: request-id, approved: false, can-execute: false })
        )
      )
    )
    err-request-not-found
  )
)

(define-public (reject-request (request-id uint))
  (match (map-get? spending-requests request-id)
    request
    (begin
      (asserts! (is-signer tx-sender) err-not-authorized)
      (asserts! (not (get executed request)) err-already-processed)
      
      (map-set spending-requests request-id
        (merge request { rejections: (+ (get rejections request) u1) })
      )
      
      (ok { request-id: request-id, rejected: true })
    )
    err-request-not-found
  )
)

(define-public (execute-request (request-id uint))
  (match (map-get? spending-requests request-id)
    request
    (begin
      (asserts! (get approved request) err-not-authorized)
      (asserts! (not (get executed request)) err-already-processed)
      (asserts! (<= (get amount request) (var-get total-balance)) err-insufficient-funds)
      
      ;; Reset daily limit if needed
      (reset-daily-limit-if-needed)
      
      ;; Check daily limit
      (asserts! (<= (+ (var-get daily-spent) (get amount request)) daily-limit) err-spending-limit)
      
      ;; Execute transfer
      (try! (as-contract (stx-transfer? (get amount request) tx-sender (get recipient request))))
      
      ;; Update request
      (map-set spending-requests request-id
        (merge request { executed: true })
      )
      
      ;; Update stats
      (var-set total-balance (- (var-get total-balance) (get amount request)))
      (var-set total-spent (+ (var-get total-spent) (get amount request)))
      (var-set daily-spent (+ (var-get daily-spent) (get amount request)))
      
      ;; Update budget allocation
      (update-budget-spent (get category request) (get amount request))
      
      ;; Record transaction
      (record-transaction "spending" (get amount request) (get recipient request) (get category request))
      
      (ok { 
        request-id: request-id, 
        executed: true, 
        amount: (get amount request),
        recipient: (get recipient request)
      })
    )
    err-request-not-found
  )
)

(define-public (quick-spend (recipient principal) (amount uint) (category (string-ascii 32)))
  (let (
    (spender-info (get-spender-info tx-sender))
  )
    (asserts! (get active spender-info) err-not-authorized)
    (asserts! (<= amount (get spending-limit spender-info)) err-spending-limit)
    (asserts! (<= amount single-tx-limit) err-spending-limit)
    (asserts! (<= amount (var-get total-balance)) err-insufficient-funds)
    
    ;; Reset daily limit if needed
    (reset-daily-limit-if-needed)
    
    ;; Check daily limit
    (asserts! (<= (+ (var-get daily-spent) amount) daily-limit) err-spending-limit)
    
    ;; Execute transfer
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update stats
    (var-set total-balance (- (var-get total-balance) amount))
    (var-set total-spent (+ (var-get total-spent) amount))
    (var-set daily-spent (+ (var-get daily-spent) amount))
    
    ;; Update spender stats
    (map-set authorized-spenders tx-sender
      (merge spender-info {
        spent-today: (+ (get spent-today spender-info) amount),
        last-spend-block: stacks-block-height
      })
    )
    
    ;; Record transaction
    (record-transaction "quick-spend" amount recipient category)
    
    (ok { spent: amount, recipient: recipient })
  )
)

(define-private (reset-daily-limit-if-needed)
  (if (> (- stacks-block-height (var-get last-reset-block)) u144) ;; ~1 day
    (begin
      (var-set daily-spent u0)
      (var-set last-reset-block stacks-block-height)
    )
    true
  )
)

(define-private (record-transaction (tx-type (string-ascii 20)) (amount uint) (recipient principal) (category (string-ascii 32)))
  (let (
    (tx-id (var-get tx-nonce))
  )
    (map-set transactions tx-id
      {
        tx-type: tx-type,
        amount: amount,
        recipient: recipient,
        category: category,
        executed-at: stacks-block-height,
        executed-by: tx-sender
      }
    )
    (var-set tx-nonce (+ tx-id u1))
  )
)

(define-private (update-budget-spent (category (string-ascii 32)) (amount uint))
  (match (map-get? budget-allocations category)
    budget
    (map-set budget-allocations category
      (merge budget {
        spent: (+ (get spent budget) amount),
        remaining: (- (get remaining budget) amount)
      })
    )
    true
  )
)

;; Read-only Functions

(define-read-only (get-spending-request (request-id uint))
  (map-get? spending-requests request-id)
)

(define-read-only (get-spender-info (spender principal))
  (default-to 
    { spending-limit: u0, spent-today: u0, last-spend-block: u0, active: false }
    (map-get? authorized-spenders spender)
  )
)

(define-read-only (get-budget-allocation (category (string-ascii 32)))
  (map-get? budget-allocations category)
)

(define-read-only (is-signer (account principal))
  (default-to false (map-get? signers account))
)

(define-read-only (get-treasury-stats)
  {
    total-balance: (var-get total-balance),
    total-spent: (var-get total-spent),
    daily-spent: (var-get daily-spent),
    daily-limit: daily-limit,
    daily-remaining: (- daily-limit (var-get daily-spent)),
    total-requests: (var-get request-nonce),
    required-signatures: (var-get required-signatures)
  }
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions tx-id)
)

(define-read-only (has-approved (request-id uint) (signer principal))
  (default-to false (map-get? request-approvals { request-id: request-id, signer: signer }))
)

;; Admin Functions

(define-public (add-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set signers signer true)
    (ok true)
  )
)

(define-public (remove-signer (signer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete signers signer)
    (ok true)
  )
)

(define-public (add-authorized-spender (spender principal) (limit uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-spenders spender
      { spending-limit: limit, spent-today: u0, last-spend-block: u0, active: true }
    )
    (ok true)
  )
)

(define-public (remove-authorized-spender (spender principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete authorized-spenders spender)
    (ok true)
  )
)

(define-public (set-budget-allocation (category (string-ascii 32)) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set budget-allocations category
      { allocated: amount, spent: u0, remaining: amount }
    )
    (ok true)
  )
)

(define-public (set-required-signatures (count uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set required-signatures count)
    (ok count)
  )
)

(define-public (set-governance-contract (governance principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set governance-contract (some governance))
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set total-balance (- (var-get total-balance) amount))
    (ok amount)
  )
)
