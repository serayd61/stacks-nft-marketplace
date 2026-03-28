;; Marketplace Escrow Contract
;; Holds STX funds safely during NFT purchase/auction settlement
;; Part of Stacks NFT Marketplace ecosystem

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-escrow-not-found (err u201))
(define-constant err-escrow-already-exists (err u202))
(define-constant err-insufficient-funds (err u203))
(define-constant err-not-authorized (err u204))
(define-constant err-escrow-locked (err u205))
(define-constant err-escrow-released (err u206))
(define-constant err-invalid-amount (err u207))
(define-constant err-dispute-active (err u208))

;; Escrow status constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-LOCKED u1)
(define-constant STATUS-RELEASED u2)
(define-constant STATUS-REFUNDED u3)
(define-constant STATUS-DISPUTED u4)

;; Data Variables
(define-data-var escrow-nonce uint u0)
(define-data-var total-locked uint u0)
(define-data-var total-released uint u0)
(define-data-var dispute-resolver principal contract-owner)

;; Escrow records
(define-map escrows uint
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    nft-contract: principal,
    token-id: uint,
    created-at: uint,
    expires-at: uint,
    status: uint,
    dispute-reason: (optional (string-ascii 200))
  }
)

;; Track escrows by buyer
(define-map buyer-escrows principal (list 50 uint))

;; Track escrows by seller
(define-map seller-escrows principal (list 50 uint))

;; Read-only functions

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

(define-read-only (get-escrow-status (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (ok (get status escrow))
    err-escrow-not-found
  )
)

(define-read-only (get-total-locked)
  (var-get total-locked)
)

(define-read-only (get-total-released)
  (var-get total-released)
)

(define-read-only (get-dispute-resolver)
  (var-get dispute-resolver)
)

(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (> stacks-block-height (get expires-at escrow))
    false
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Public functions

;; Create escrow - buyer locks funds
(define-public (create-escrow
    (seller principal)
    (amount uint)
    (nft-contract principal)
    (token-id uint)
    (duration uint))
  (let (
    (escrow-id (var-get escrow-nonce))
    (expires-at (+ stacks-block-height duration))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    (asserts! (not (is-eq tx-sender seller)) err-not-authorized)

    ;; Lock funds in contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Create escrow record
    (map-set escrows escrow-id {
      buyer: tx-sender,
      seller: seller,
      amount: amount,
      nft-contract: nft-contract,
      token-id: token-id,
      created-at: stacks-block-height,
      expires-at: expires-at,
      status: STATUS-LOCKED,
      dispute-reason: none
    })

    ;; Update nonce and stats
    (var-set escrow-nonce (+ escrow-id u1))
    (var-set total-locked (+ (var-get total-locked) amount))

    (ok { escrow-id: escrow-id, amount: amount, expires-at: expires-at })
  )
)

;; Release escrow to seller - called by buyer after receiving NFT
(define-public (release-escrow (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (let (
      (amount (get amount escrow))
      (seller (get seller escrow))
      (buyer (get buyer escrow))
    )
      (asserts! (is-eq tx-sender buyer) err-not-authorized)
      (asserts! (is-eq (get status escrow) STATUS-LOCKED) err-escrow-released)

      ;; Transfer to seller
      (try! (as-contract (stx-transfer? amount tx-sender seller)))

      ;; Update escrow status
      (map-set escrows escrow-id (merge escrow { status: STATUS-RELEASED }))
      (var-set total-released (+ (var-get total-released) amount))

      (ok { escrow-id: escrow-id, released-to: seller, amount: amount })
    )
    err-escrow-not-found
  )
)

;; Refund escrow to buyer - if expired or seller cancels
(define-public (refund-escrow (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow
    (let (
      (amount (get amount escrow))
      (buyer (get buyer escrow))
      (seller (get seller escrow))
    )
      ;; Only buyer, seller, or contract owner can refund
      (asserts! (or
        (is-eq tx-sender buyer)
        (is-eq tx-sender seller)
        (is-eq tx-sender contract-owner)) err-not-authorized)
      (asserts! (is-eq (get status escrow) STATUS-LOCKED) err-escrow-released)
      ;; Must be expired or seller initiated
      (asserts! (or
        (> stacks-block-height (get expires-at escrow))
        (is-eq tx-sender seller)
        (is-eq tx-sender contract-owner)) err-escrow-locked)

      ;; Refund to buyer
      (try! (as-contract (stx-transfer? amount tx-sender buyer)))

      ;; Update status
      (map-set escrows escrow-id (merge escrow { status: STATUS-REFUNDED }))

      (ok { escrow-id: escrow-id, refunded-to: buyer, amount: amount })
    )
    err-escrow-not-found
  )
)

;; Open dispute
(define-public (open-dispute (escrow-id uint) (reason (string-ascii 200)))
  (match (map-get? escrows escrow-id)
    escrow
    (let (
      (buyer (get buyer escrow))
      (seller (get seller escrow))
    )
      (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) err-not-authorized)
      (asserts! (is-eq (get status escrow) STATUS-LOCKED) err-escrow-released)

      (map-set escrows escrow-id (merge escrow {
        status: STATUS-DISPUTED,
        dispute-reason: (some reason)
      }))

      (ok { escrow-id: escrow-id, status: STATUS-DISPUTED })
    )
    err-escrow-not-found
  )
)

;; Resolve dispute - only dispute resolver
(define-public (resolve-dispute (escrow-id uint) (release-to-seller bool))
  (match (map-get? escrows escrow-id)
    escrow
    (let (
      (amount (get amount escrow))
      (buyer (get buyer escrow))
      (seller (get seller escrow))
      (resolver (var-get dispute-resolver))
    )
      (asserts! (is-eq tx-sender resolver) err-owner-only)
      (asserts! (is-eq (get status escrow) STATUS-DISPUTED) err-escrow-locked)

      (if release-to-seller
        (begin
          (try! (as-contract (stx-transfer? amount tx-sender seller)))
          (map-set escrows escrow-id (merge escrow { status: STATUS-RELEASED }))
          (var-set total-released (+ (var-get total-released) amount))
          (ok { escrow-id: escrow-id, resolved-to: seller, amount: amount })
        )
        (begin
          (try! (as-contract (stx-transfer? amount tx-sender buyer)))
          (map-set escrows escrow-id (merge escrow { status: STATUS-REFUNDED }))
          (ok { escrow-id: escrow-id, resolved-to: buyer, amount: amount })
        )
      )
    )
    err-escrow-not-found
  )
)

;; Update dispute resolver - owner only
(define-public (set-dispute-resolver (new-resolver principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set dispute-resolver new-resolver)
    (ok { new-resolver: new-resolver })
  )
)
