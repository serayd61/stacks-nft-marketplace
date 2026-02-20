;; Multi-Signature Wallet Contract
;; Secure wallet requiring multiple signatures for transactions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-signer (err u101))
(define-constant err-tx-not-found (err u102))
(define-constant err-already-signed (err u103))
(define-constant err-already-executed (err u104))
(define-constant err-insufficient-signatures (err u105))
(define-constant err-invalid-threshold (err u106))
(define-constant err-tx-expired (err u107))
(define-constant err-insufficient-funds (err u108))
(define-constant err-signer-exists (err u109))
(define-constant err-min-signers (err u110))

;; Transaction types
(define-constant tx-type-transfer u1)
(define-constant tx-type-add-signer u2)
(define-constant tx-type-remove-signer u3)
(define-constant tx-type-change-threshold u4)
(define-constant tx-type-contract-call u5)

;; Data Variables
(define-data-var tx-nonce uint u0)
(define-data-var required-signatures uint u2)
(define-data-var signer-count uint u0)
(define-data-var wallet-balance uint u0)
(define-data-var tx-expiry-blocks uint u10080) ;; ~7 days

;; Signers
(define-map signers principal bool)

;; Transactions
(define-map transactions uint
  {
    tx-type: uint,
    creator: principal,
    recipient: (optional principal),
    amount: (optional uint),
    data: (optional (buff 256)),
    target-contract: (optional principal),
    created-at: uint,
    expires-at: uint,
    executed: bool,
    cancelled: bool,
    signatures-count: uint
  }
)

;; Transaction signatures
(define-map tx-signatures { tx-id: uint, signer: principal } bool)

;; Transaction history
(define-map tx-history uint
  {
    executed-at: uint,
    executed-by: principal,
    success: bool
  }
)

;; Signer stats
(define-map signer-stats principal
  {
    txs-created: uint,
    txs-signed: uint,
    txs-executed: uint
  }
)

;; Initialize with contract owner as first signer
(map-set signers contract-owner true)

;; Public Functions

(define-public (deposit (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set wallet-balance (+ (var-get wallet-balance) amount))
    (ok { deposited: amount, new-balance: (var-get wallet-balance) })
  )
)

(define-public (propose-transfer (recipient principal) (amount uint))
  (let (
    (tx-id (var-get tx-nonce))
  )
    (asserts! (is-signer tx-sender) err-not-signer)
    (asserts! (<= amount (var-get wallet-balance)) err-insufficient-funds)
    
    ;; Create transaction
    (map-set transactions tx-id
      {
        tx-type: tx-type-transfer,
        creator: tx-sender,
        recipient: (some recipient),
        amount: (some amount),
        data: none,
        target-contract: none,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height (var-get tx-expiry-blocks)),
        executed: false,
        cancelled: false,
        signatures-count: u1
      }
    )
    
    ;; Creator auto-signs
    (map-set tx-signatures { tx-id: tx-id, signer: tx-sender } true)
    
    ;; Update stats
    (map-set signer-stats tx-sender
      (merge (get-signer-stats tx-sender)
        { 
          txs-created: (+ (get txs-created (get-signer-stats tx-sender)) u1),
          txs-signed: (+ (get txs-signed (get-signer-stats tx-sender)) u1)
        }
      )
    )
    
    (var-set tx-nonce (+ tx-id u1))
    
    (ok { tx-id: tx-id, signatures-needed: (var-get required-signatures) })
  )
)

(define-public (propose-add-signer (new-signer principal))
  (let (
    (tx-id (var-get tx-nonce))
  )
    (asserts! (is-signer tx-sender) err-not-signer)
    (asserts! (not (is-signer new-signer)) err-signer-exists)
    
    (map-set transactions tx-id
      {
        tx-type: tx-type-add-signer,
        creator: tx-sender,
        recipient: (some new-signer),
        amount: none,
        data: none,
        target-contract: none,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height (var-get tx-expiry-blocks)),
        executed: false,
        cancelled: false,
        signatures-count: u1
      }
    )
    
    (map-set tx-signatures { tx-id: tx-id, signer: tx-sender } true)
    (var-set tx-nonce (+ tx-id u1))
    
    (ok { tx-id: tx-id })
  )
)

(define-public (propose-remove-signer (signer-to-remove principal))
  (let (
    (tx-id (var-get tx-nonce))
  )
    (asserts! (is-signer tx-sender) err-not-signer)
    (asserts! (is-signer signer-to-remove) err-not-signer)
    (asserts! (> (var-get signer-count) (var-get required-signatures)) err-min-signers)
    
    (map-set transactions tx-id
      {
        tx-type: tx-type-remove-signer,
        creator: tx-sender,
        recipient: (some signer-to-remove),
        amount: none,
        data: none,
        target-contract: none,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height (var-get tx-expiry-blocks)),
        executed: false,
        cancelled: false,
        signatures-count: u1
      }
    )
    
    (map-set tx-signatures { tx-id: tx-id, signer: tx-sender } true)
    (var-set tx-nonce (+ tx-id u1))
    
    (ok { tx-id: tx-id })
  )
)

(define-public (propose-change-threshold (new-threshold uint))
  (let (
    (tx-id (var-get tx-nonce))
  )
    (asserts! (is-signer tx-sender) err-not-signer)
    (asserts! (> new-threshold u0) err-invalid-threshold)
    (asserts! (<= new-threshold (var-get signer-count)) err-invalid-threshold)
    
    (map-set transactions tx-id
      {
        tx-type: tx-type-change-threshold,
        creator: tx-sender,
        recipient: none,
        amount: (some new-threshold),
        data: none,
        target-contract: none,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height (var-get tx-expiry-blocks)),
        executed: false,
        cancelled: false,
        signatures-count: u1
      }
    )
    
    (map-set tx-signatures { tx-id: tx-id, signer: tx-sender } true)
    (var-set tx-nonce (+ tx-id u1))
    
    (ok { tx-id: tx-id })
  )
)

(define-public (sign-transaction (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
    (begin
      (asserts! (is-signer tx-sender) err-not-signer)
      (asserts! (not (get executed tx)) err-already-executed)
      (asserts! (not (get cancelled tx)) err-tx-not-found)
      (asserts! (<= stacks-block-height (get expires-at tx)) err-tx-expired)
      (asserts! (is-none (map-get? tx-signatures { tx-id: tx-id, signer: tx-sender })) err-already-signed)
      
      ;; Add signature
      (map-set tx-signatures { tx-id: tx-id, signer: tx-sender } true)
      
      ;; Update transaction
      (map-set transactions tx-id
        (merge tx { signatures-count: (+ (get signatures-count tx) u1) })
      )
      
      ;; Update stats
      (map-set signer-stats tx-sender
        (merge (get-signer-stats tx-sender)
          { txs-signed: (+ (get txs-signed (get-signer-stats tx-sender)) u1) }
        )
      )
      
      (ok { tx-id: tx-id, signatures: (+ (get signatures-count tx) u1), required: (var-get required-signatures) })
    )
    err-tx-not-found
  )
)

(define-public (execute-transaction (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
    (begin
      (asserts! (is-signer tx-sender) err-not-signer)
      (asserts! (not (get executed tx)) err-already-executed)
      (asserts! (not (get cancelled tx)) err-tx-not-found)
      (asserts! (<= stacks-block-height (get expires-at tx)) err-tx-expired)
      (asserts! (>= (get signatures-count tx) (var-get required-signatures)) err-insufficient-signatures)
      
      ;; Execute based on type
      (if (is-eq (get tx-type tx) tx-type-transfer)
        (execute-transfer tx-id tx)
        (if (is-eq (get tx-type tx) tx-type-add-signer)
          (execute-add-signer tx-id tx)
          (if (is-eq (get tx-type tx) tx-type-remove-signer)
            (execute-remove-signer tx-id tx)
            (if (is-eq (get tx-type tx) tx-type-change-threshold)
              (execute-change-threshold tx-id tx)
              err-tx-not-found
            )
          )
        )
      )
    )
    err-tx-not-found
  )
)

(define-private (execute-transfer (tx-id uint) (tx { tx-type: uint, creator: principal, recipient: (optional principal), amount: (optional uint), data: (optional (buff 256)), target-contract: (optional principal), created-at: uint, expires-at: uint, executed: bool, cancelled: bool, signatures-count: uint }))
  (let (
    (recipient (unwrap! (get recipient tx) err-tx-not-found))
    (amount (unwrap! (get amount tx) err-tx-not-found))
  )
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set wallet-balance (- (var-get wallet-balance) amount))
    (mark-executed tx-id)
    (ok { executed: true, tx-id: tx-id })
  )
)

(define-private (execute-add-signer (tx-id uint) (tx { tx-type: uint, creator: principal, recipient: (optional principal), amount: (optional uint), data: (optional (buff 256)), target-contract: (optional principal), created-at: uint, expires-at: uint, executed: bool, cancelled: bool, signatures-count: uint }))
  (let (
    (new-signer (unwrap! (get recipient tx) err-tx-not-found))
  )
    (map-set signers new-signer true)
    (var-set signer-count (+ (var-get signer-count) u1))
    (mark-executed tx-id)
    (ok { executed: true, tx-id: tx-id })
  )
)

(define-private (execute-remove-signer (tx-id uint) (tx { tx-type: uint, creator: principal, recipient: (optional principal), amount: (optional uint), data: (optional (buff 256)), target-contract: (optional principal), created-at: uint, expires-at: uint, executed: bool, cancelled: bool, signatures-count: uint }))
  (let (
    (signer-to-remove (unwrap! (get recipient tx) err-tx-not-found))
  )
    (map-delete signers signer-to-remove)
    (var-set signer-count (- (var-get signer-count) u1))
    (mark-executed tx-id)
    (ok { executed: true, tx-id: tx-id })
  )
)

(define-private (execute-change-threshold (tx-id uint) (tx { tx-type: uint, creator: principal, recipient: (optional principal), amount: (optional uint), data: (optional (buff 256)), target-contract: (optional principal), created-at: uint, expires-at: uint, executed: bool, cancelled: bool, signatures-count: uint }))
  (let (
    (new-threshold (unwrap! (get amount tx) err-tx-not-found))
  )
    (var-set required-signatures new-threshold)
    (mark-executed tx-id)
    (ok { executed: true, tx-id: tx-id })
  )
)

(define-private (mark-executed (tx-id uint))
  (begin
    (map-set transactions tx-id
      (merge (unwrap-panic (map-get? transactions tx-id)) { executed: true })
    )
    (map-set tx-history tx-id
      {
        executed-at: stacks-block-height,
        executed-by: tx-sender,
        success: true
      }
    )
    (map-set signer-stats tx-sender
      (merge (get-signer-stats tx-sender)
        { txs-executed: (+ (get txs-executed (get-signer-stats tx-sender)) u1) }
      )
    )
  )
)

(define-public (cancel-transaction (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
    (begin
      (asserts! (is-eq (get creator tx) tx-sender) err-not-signer)
      (asserts! (not (get executed tx)) err-already-executed)
      
      (map-set transactions tx-id
        (merge tx { cancelled: true })
      )
      
      (ok { tx-id: tx-id, cancelled: true })
    )
    err-tx-not-found
  )
)

;; Read-only Functions

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions tx-id)
)

(define-read-only (is-signer (account principal))
  (default-to false (map-get? signers account))
)

(define-read-only (has-signed (tx-id uint) (signer principal))
  (default-to false (map-get? tx-signatures { tx-id: tx-id, signer: signer }))
)

(define-read-only (get-signer-stats (signer principal))
  (default-to 
    { txs-created: u0, txs-signed: u0, txs-executed: u0 }
    (map-get? signer-stats signer)
  )
)

(define-read-only (get-tx-history (tx-id uint))
  (map-get? tx-history tx-id)
)

(define-read-only (get-wallet-info)
  {
    balance: (var-get wallet-balance),
    signer-count: (var-get signer-count),
    required-signatures: (var-get required-signatures),
    pending-txs: (var-get tx-nonce),
    tx-expiry-blocks: (var-get tx-expiry-blocks)
  }
)

(define-read-only (get-tx-status (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
    (if (get executed tx)
      "executed"
      (if (get cancelled tx)
        "cancelled"
        (if (> stacks-block-height (get expires-at tx))
          "expired"
          (if (>= (get signatures-count tx) (var-get required-signatures))
            "ready"
            "pending"
          )
        )
      )
    )
    "not-found"
  )
)

(define-read-only (can-execute (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
    (and
      (not (get executed tx))
      (not (get cancelled tx))
      (<= stacks-block-height (get expires-at tx))
      (>= (get signatures-count tx) (var-get required-signatures))
    )
    false
  )
)

;; Admin Functions (require multisig)

(define-public (set-tx-expiry (new-expiry uint))
  (begin
    (asserts! (is-signer tx-sender) err-not-signer)
    (var-set tx-expiry-blocks new-expiry)
    (ok new-expiry)
  )
)
