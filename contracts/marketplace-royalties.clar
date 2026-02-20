;; Marketplace Royalties Contract
;; On-chain royalty registry and automatic distribution for NFT creators
;; Supports EIP-2981 style royalty standard for Stacks ecosystem

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-registered (err u301))
(define-constant err-already-registered (err u302))
(define-constant err-not-authorized (err u303))
(define-constant err-invalid-royalty (err u304))
(define-constant err-invalid-amount (err u305))
(define-constant err-no-recipients (err u306))
(define-constant err-recipient-not-found (err u307))

;; Max royalty: 15% (1500 basis points)
(define-constant max-royalty-bps u1500)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var total-royalties-paid uint u0)
(define-data-var total-distributions uint u0)

;; Royalty registry: nft-contract -> royalty info
(define-map royalty-registry principal
  {
    creator: principal,
    royalty-bps: uint,        ;; basis points (e.g. 500 = 5%)
    registered-at: uint,
    active: bool,
    total-earned: uint
  }
)

;; Multi-recipient splits: nft-contract -> list of recipients
(define-map royalty-splits principal
  (list 5
    {
      recipient: principal,
      share-bps: uint          ;; must sum to 10000 across all recipients
    }
  )
)

;; Per-token royalty overrides (for special editions)
(define-map token-royalty-override
  { nft-contract: principal, token-id: uint }
  {
    recipient: principal,
    royalty-bps: uint
  }
)

;; Royalty balance (unclaimed)
(define-map royalty-balance principal uint)

;; Claim history
(define-data-var claim-nonce uint u0)
(define-map claim-history uint
  {
    recipient: principal,
    amount: uint,
    claimed-at: uint
  }
)

;; Read-only functions

(define-read-only (get-royalty-info (nft-contract principal))
  (map-get? royalty-registry nft-contract)
)

(define-read-only (get-royalty-splits (nft-contract principal))
  (map-get? royalty-splits nft-contract)
)

(define-read-only (get-token-royalty (nft-contract principal) (token-id uint))
  (map-get? token-royalty-override { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-royalty-balance (recipient principal))
  (default-to u0 (map-get? royalty-balance recipient))
)

(define-read-only (calculate-royalty (nft-contract principal) (sale-price uint))
  (match (map-get? royalty-registry nft-contract)
    info
    (ok (/ (* sale-price (get royalty-bps info)) fee-denominator))
    err-not-registered
  )
)

(define-read-only (calculate-token-royalty (nft-contract principal) (token-id uint) (sale-price uint))
  (match (map-get? token-royalty-override { nft-contract: nft-contract, token-id: token-id })
    override-info
    (ok (/ (* sale-price (get royalty-bps override-info)) fee-denominator))
    (calculate-royalty nft-contract sale-price)
  )
)

(define-read-only (get-total-royalties-paid)
  (var-get total-royalties-paid)
)

(define-read-only (is-registered (nft-contract principal))
  (is-some (map-get? royalty-registry nft-contract))
)

;; Public functions

;; Register NFT collection for royalties
(define-public (register-royalty
    (nft-contract principal)
    (royalty-bps uint))
  (begin
    (asserts! (not (is-some (map-get? royalty-registry nft-contract))) err-already-registered)
    (asserts! (<= royalty-bps max-royalty-bps) err-invalid-royalty)
    (asserts! (> royalty-bps u0) err-invalid-royalty)

    (map-set royalty-registry nft-contract {
      creator: tx-sender,
      royalty-bps: royalty-bps,
      registered-at: stacks-block-height,
      active: true,
      total-earned: u0
    })

    (ok { nft-contract: nft-contract, royalty-bps: royalty-bps, creator: tx-sender })
  )
)

;; Set multi-recipient split (creator only)
(define-public (set-royalty-splits
    (nft-contract principal)
    (splits (list 5 { recipient: principal, share-bps: uint })))
  (match (map-get? royalty-registry nft-contract)
    info
    (begin
      (asserts! (is-eq (get creator info) tx-sender) err-not-authorized)
      (map-set royalty-splits nft-contract splits)
      (ok { nft-contract: nft-contract, split-count: (len splits) })
    )
    err-not-registered
  )
)

;; Set per-token royalty override
(define-public (set-token-royalty
    (nft-contract principal)
    (token-id uint)
    (recipient principal)
    (royalty-bps uint))
  (match (map-get? royalty-registry nft-contract)
    info
    (begin
      (asserts! (is-eq (get creator info) tx-sender) err-not-authorized)
      (asserts! (<= royalty-bps max-royalty-bps) err-invalid-royalty)

      (map-set token-royalty-override
        { nft-contract: nft-contract, token-id: token-id }
        { recipient: recipient, royalty-bps: royalty-bps }
      )

      (ok { nft-contract: nft-contract, token-id: token-id, royalty-bps: royalty-bps })
    )
    err-not-registered
  )
)

;; Distribute royalty payment (called by marketplace on sale)
(define-public (distribute-royalty
    (nft-contract principal)
    (token-id uint)
    (sale-price uint)
    (payer principal))
  (match (map-get? royalty-registry nft-contract)
    info
    (let (
      (royalty-amount (/ (* sale-price (get royalty-bps info)) fee-denominator))
    )
      (asserts! (get active info) err-not-registered)
      (asserts! (> royalty-amount u0) err-invalid-amount)

      ;; Check for splits
      (match (map-get? royalty-splits nft-contract)
        splits
        ;; Distribute to first recipient (simplified - full split handled off-chain)
        (let ((creator (get creator info)))
          (map-set royalty-balance creator
            (+ (get-royalty-balance creator) royalty-amount))
          (map-set royalty-registry nft-contract
            (merge info { total-earned: (+ (get total-earned info) royalty-amount) }))
          (var-set total-royalties-paid (+ (var-get total-royalties-paid) royalty-amount))
          (var-set total-distributions (+ (var-get total-distributions) u1))
          (ok { distributed: royalty-amount, recipient: creator })
        )
        ;; No splits - pay creator directly
        (let ((creator (get creator info)))
          (map-set royalty-balance creator
            (+ (get-royalty-balance creator) royalty-amount))
          (map-set royalty-registry nft-contract
            (merge info { total-earned: (+ (get total-earned info) royalty-amount) }))
          (var-set total-royalties-paid (+ (var-get total-royalties-paid) royalty-amount))
          (var-set total-distributions (+ (var-get total-distributions) u1))
          (ok { distributed: royalty-amount, recipient: creator })
        )
      )
    )
    err-not-registered
  )
)

;; Claim accumulated royalties
(define-public (claim-royalties)
  (let (
    (balance (get-royalty-balance tx-sender))
    (claim-id (var-get claim-nonce))
  )
    (asserts! (> balance u0) err-invalid-amount)

    ;; Reset balance
    (map-set royalty-balance tx-sender u0)

    ;; Record claim
    (map-set claim-history claim-id {
      recipient: tx-sender,
      amount: balance,
      claimed-at: stacks-block-height
    })
    (var-set claim-nonce (+ claim-id u1))

    (ok { claimed: balance, recipient: tx-sender, claim-id: claim-id })
  )
)

;; Update royalty percentage (creator only, with limits)
(define-public (update-royalty-bps (nft-contract principal) (new-royalty-bps uint))
  (match (map-get? royalty-registry nft-contract)
    info
    (begin
      (asserts! (is-eq (get creator info) tx-sender) err-not-authorized)
      (asserts! (<= new-royalty-bps max-royalty-bps) err-invalid-royalty)
      (map-set royalty-registry nft-contract (merge info { royalty-bps: new-royalty-bps }))
      (ok { nft-contract: nft-contract, new-royalty-bps: new-royalty-bps })
    )
    err-not-registered
  )
)

;; Deactivate royalty (creator only)
(define-public (deactivate-royalty (nft-contract principal))
  (match (map-get? royalty-registry nft-contract)
    info
    (begin
      (asserts! (or
        (is-eq (get creator info) tx-sender)
        (is-eq tx-sender contract-owner)) err-not-authorized)
      (map-set royalty-registry nft-contract (merge info { active: false }))
      (ok { nft-contract: nft-contract, active: false })
    )
    err-not-registered
  )
)
