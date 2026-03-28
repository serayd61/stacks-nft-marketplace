;; Marketplace Offers Contract
;; Decentralized offer and counter-offer system for NFTs on Stacks
;; Buyers can make offers; sellers can accept, reject, or counter

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-offer-not-found (err u401))
(define-constant err-not-authorized (err u402))
(define-constant err-offer-expired (err u403))
(define-constant err-offer-inactive (err u404))
(define-constant err-invalid-amount (err u405))
(define-constant err-insufficient-funds (err u406))
(define-constant err-self-offer (err u407))
(define-constant err-already-responded (err u408))

;; Offer status
(define-constant STATUS-PENDING u0)
(define-constant STATUS-ACCEPTED u1)
(define-constant STATUS-REJECTED u2)
(define-constant STATUS-COUNTERED u3)
(define-constant STATUS-WITHDRAWN u4)
(define-constant STATUS-EXPIRED u5)

;; Data Variables
(define-data-var offer-nonce uint u0)
(define-data-var total-offers-made uint u0)
(define-data-var total-offers-accepted uint u0)
(define-data-var total-volume uint u0)

;; Offers map
(define-map offers uint
  {
    buyer: principal,
    seller: principal,
    nft-contract: principal,
    token-id: uint,
    offer-amount: uint,
    counter-amount: (optional uint),
    created-at: uint,
    expires-at: uint,
    status: uint,
    message: (optional (string-ascii 150))
  }
)

;; Index: nft -> list of offer IDs
(define-map nft-offers
  { nft-contract: principal, token-id: uint }
  (list 20 uint)
)

;; Index: buyer -> list of offer IDs
(define-map buyer-offer-ids principal (list 50 uint))

;; Index: seller -> list of offer IDs
(define-map seller-offer-ids principal (list 50 uint))

;; Read-only functions

(define-read-only (get-offer (offer-id uint))
  (map-get? offers offer-id)
)

(define-read-only (get-nft-offers (nft-contract principal) (token-id uint))
  (map-get? nft-offers { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-buyer-offers (buyer principal))
  (map-get? buyer-offer-ids buyer)
)

(define-read-only (get-seller-offers (seller principal))
  (map-get? seller-offer-ids seller)
)

(define-read-only (get-offer-status (offer-id uint))
  (match (map-get? offers offer-id)
    offer (ok (get status offer))
    err-offer-not-found
  )
)

(define-read-only (is-offer-valid (offer-id uint))
  (match (map-get? offers offer-id)
    offer
    (and
      (is-eq (get status offer) STATUS-PENDING)
      (<= stacks-block-height (get expires-at offer))
    )
    false
  )
)

(define-read-only (get-marketplace-stats)
  {
    total-offers: (var-get offer-nonce),
    offers-made: (var-get total-offers-made),
    offers-accepted: (var-get total-offers-accepted),
    total-volume: (var-get total-volume)
  }
)

;; Public functions

;; Make an offer on an NFT
(define-public (make-offer
    (seller principal)
    (nft-contract principal)
    (token-id uint)
    (offer-amount uint)
    (duration uint)
    (message (optional (string-ascii 150))))
  (let (
    (offer-id (var-get offer-nonce))
    (expires-at (+ stacks-block-height duration))
  )
    (asserts! (> offer-amount u0) err-invalid-amount)
    (asserts! (not (is-eq tx-sender seller)) err-self-offer)
    (asserts! (>= (stx-get-balance tx-sender) offer-amount) err-insufficient-funds)

    ;; Lock buyer funds in contract
    (try! (stx-transfer? offer-amount tx-sender (as-contract tx-sender)))

    ;; Create offer record
    (map-set offers offer-id {
      buyer: tx-sender,
      seller: seller,
      nft-contract: nft-contract,
      token-id: token-id,
      offer-amount: offer-amount,
      counter-amount: none,
      created-at: stacks-block-height,
      expires-at: expires-at,
      status: STATUS-PENDING,
      message: message
    })

    ;; Update indices
    (var-set offer-nonce (+ offer-id u1))
    (var-set total-offers-made (+ (var-get total-offers-made) u1))

    (ok { offer-id: offer-id, amount: offer-amount, expires-at: expires-at })
  )
)

;; Accept an offer (seller only)
(define-public (accept-offer (offer-id uint))
  (match (map-get? offers offer-id)
    offer
    (let (
      (buyer (get buyer offer))
      (seller (get seller offer))
      (amount (get offer-amount offer))
    )
      (asserts! (is-eq tx-sender seller) err-not-authorized)
      (asserts! (is-eq (get status offer) STATUS-PENDING) err-already-responded)
      (asserts! (<= stacks-block-height (get expires-at offer)) err-offer-expired)

      ;; Release funds to seller
      (try! (as-contract (stx-transfer? amount tx-sender seller)))

      ;; Update offer status
      (map-set offers offer-id (merge offer { status: STATUS-ACCEPTED }))

      ;; Update stats
      (var-set total-offers-accepted (+ (var-get total-offers-accepted) u1))
      (var-set total-volume (+ (var-get total-volume) amount))

      (ok { offer-id: offer-id, accepted-by: seller, amount: amount, buyer: buyer })
    )
    err-offer-not-found
  )
)

;; Reject an offer (seller only)
(define-public (reject-offer (offer-id uint))
  (match (map-get? offers offer-id)
    offer
    (let (
      (buyer (get buyer offer))
      (seller (get seller offer))
      (amount (get offer-amount offer))
    )
      (asserts! (is-eq tx-sender seller) err-not-authorized)
      (asserts! (is-eq (get status offer) STATUS-PENDING) err-already-responded)

      ;; Refund buyer
      (try! (as-contract (stx-transfer? amount tx-sender buyer)))

      ;; Update status
      (map-set offers offer-id (merge offer { status: STATUS-REJECTED }))

      (ok { offer-id: offer-id, rejected: true, refunded-to: buyer })
    )
    err-offer-not-found
  )
)

;; Counter offer (seller proposes different price)
(define-public (counter-offer (offer-id uint) (counter-amount uint))
  (match (map-get? offers offer-id)
    offer
    (let (
      (seller (get seller offer))
    )
      (asserts! (is-eq tx-sender seller) err-not-authorized)
      (asserts! (is-eq (get status offer) STATUS-PENDING) err-already-responded)
      (asserts! (> counter-amount u0) err-invalid-amount)
      (asserts! (<= stacks-block-height (get expires-at offer)) err-offer-expired)

      ;; Update with counter amount
      (map-set offers offer-id (merge offer {
        counter-amount: (some counter-amount),
        status: STATUS-COUNTERED
      }))

      (ok { offer-id: offer-id, counter-amount: counter-amount, original-amount: (get offer-amount offer) })
    )
    err-offer-not-found
  )
)

;; Accept counter offer (buyer only)
(define-public (accept-counter-offer (offer-id uint))
  (match (map-get? offers offer-id)
    offer
    (let (
      (buyer (get buyer offer))
      (seller (get seller offer))
      (original-amount (get offer-amount offer))
    )
      (asserts! (is-eq tx-sender buyer) err-not-authorized)
      (asserts! (is-eq (get status offer) STATUS-COUNTERED) err-offer-inactive)
      (asserts! (<= stacks-block-height (get expires-at offer)) err-offer-expired)

      (match (get counter-amount offer)
        counter-amount
        (let (
          (extra (if (> counter-amount original-amount)
            (- counter-amount original-amount)
            u0))
        )
          ;; Buyer pays extra if counter is higher
          (if (> extra u0)
            (try! (stx-transfer? extra tx-sender (as-contract tx-sender)))
            true
          )

          ;; Release total to seller
          (let ((total (if (> counter-amount original-amount) counter-amount original-amount)))
            (try! (as-contract (stx-transfer? total tx-sender seller)))

            (map-set offers offer-id (merge offer { status: STATUS-ACCEPTED }))
            (var-set total-offers-accepted (+ (var-get total-offers-accepted) u1))
            (var-set total-volume (+ (var-get total-volume) total))

            (ok { offer-id: offer-id, final-amount: total, seller: seller })
          )
        )
        err-offer-not-found
      )
    )
    err-offer-not-found
  )
)

;; Withdraw offer (buyer only, before acceptance)
(define-public (withdraw-offer (offer-id uint))
  (match (map-get? offers offer-id)
    offer
    (let (
      (buyer (get buyer offer))
      (amount (get offer-amount offer))
    )
      (asserts! (is-eq tx-sender buyer) err-not-authorized)
      (asserts! (or
        (is-eq (get status offer) STATUS-PENDING)
        (is-eq (get status offer) STATUS-COUNTERED)) err-already-responded)

      ;; Refund buyer
      (try! (as-contract (stx-transfer? amount tx-sender buyer)))

      (map-set offers offer-id (merge offer { status: STATUS-WITHDRAWN }))

      (ok { offer-id: offer-id, withdrawn: true, refunded: amount })
    )
    err-offer-not-found
  )
)

;; Cleanup expired offer (anyone can call to free up locked funds)
(define-public (cleanup-expired-offer (offer-id uint))
  (match (map-get? offers offer-id)
    offer
    (let (
      (buyer (get buyer offer))
      (amount (get offer-amount offer))
    )
      (asserts! (> stacks-block-height (get expires-at offer)) err-offer-inactive)
      (asserts! (is-eq (get status offer) STATUS-PENDING) err-already-responded)

      ;; Refund buyer
      (try! (as-contract (stx-transfer? amount tx-sender buyer)))

      (map-set offers offer-id (merge offer { status: STATUS-EXPIRED }))

      (ok { offer-id: offer-id, expired: true, refunded-to: buyer })
    )
    err-offer-not-found
  )
)
