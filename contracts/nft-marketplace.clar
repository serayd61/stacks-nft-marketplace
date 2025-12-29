;; Stacks NFT Marketplace
;; Buy, sell, and auction NFTs on Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant treasury 'SP2PEBKJ2W1ZDDF2QQ6Y4FXKZEDPT9J9R2NKD9WJB)
(define-constant err-owner-only (err u100))
(define-constant err-not-listing-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-listing-expired (err u104))
(define-constant err-invalid-price (err u105))
(define-constant err-auction-active (err u106))
(define-constant err-auction-ended (err u107))
(define-constant err-bid-too-low (err u108))
(define-constant err-not-highest-bidder (err u109))

;; Platform fee: 2.5% (250 basis points)
(define-constant platform-fee u250)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var listing-nonce uint u0)
(define-data-var auction-nonce uint u0)
(define-data-var total-volume uint u0)
(define-data-var total-sales uint u0)
(define-data-var total-fees uint u0)

;; Fixed Price Listings
(define-map listings uint
  {
    seller: principal,
    nft-contract: principal,
    token-id: uint,
    price: uint,
    listed-at: uint,
    expires-at: uint,
    active: bool
  }
)

;; Auctions
(define-map auctions uint
  {
    seller: principal,
    nft-contract: principal,
    token-id: uint,
    start-price: uint,
    reserve-price: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    start-block: uint,
    end-block: uint,
    active: bool
  }
)

;; Seller stats
(define-map seller-stats principal
  {
    total-sales: uint,
    total-volume: uint,
    total-listings: uint
  }
)

;; Buyer stats
(define-map buyer-stats principal
  {
    total-purchases: uint,
    total-spent: uint
  }
)

;; Read-only functions
(define-read-only (get-listing (listing-id uint))
  (map-get? listings listing-id)
)

(define-read-only (get-auction (auction-id uint))
  (map-get? auctions auction-id)
)

(define-read-only (get-seller-stats (seller principal))
  (default-to 
    { total-sales: u0, total-volume: u0, total-listings: u0 }
    (map-get? seller-stats seller)
  )
)

(define-read-only (get-buyer-stats (buyer principal))
  (default-to 
    { total-purchases: u0, total-spent: u0 }
    (map-get? buyer-stats buyer)
  )
)

(define-read-only (get-marketplace-stats)
  {
    total-volume: (var-get total-volume),
    total-sales: (var-get total-sales),
    total-fees: (var-get total-fees),
    total-listings: (var-get listing-nonce),
    total-auctions: (var-get auction-nonce)
  }
)

(define-read-only (calculate-fee (price uint))
  (/ (* price platform-fee) fee-denominator)
)

(define-read-only (is-listing-active (listing-id uint))
  (match (map-get? listings listing-id)
    listing (and (get active listing) (<= stacks-block-height (get expires-at listing)))
    false
  )
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? auctions auction-id)
    auction (and (get active auction) (<= stacks-block-height (get end-block auction)))
    false
  )
)

;; Public functions

;; List NFT for fixed price sale
(define-public (list-nft (nft-contract principal) (token-id uint) (price uint) (duration uint))
  (let (
    (listing-id (var-get listing-nonce))
  )
    (asserts! (> price u0) err-invalid-price)
    
    ;; Create listing
    (map-set listings listing-id {
      seller: tx-sender,
      nft-contract: nft-contract,
      token-id: token-id,
      price: price,
      listed-at: stacks-block-height,
      expires-at: (+ stacks-block-height duration),
      active: true
    })
    
    ;; Update stats
    (var-set listing-nonce (+ listing-id u1))
    (map-set seller-stats tx-sender
      (merge (get-seller-stats tx-sender)
        { total-listings: (+ (get total-listings (get-seller-stats tx-sender)) u1) }
      )
    )
    
    (ok { listing-id: listing-id, price: price, expires-at: (+ stacks-block-height duration) })
  )
)

;; Buy NFT at listed price
(define-public (buy-nft (listing-id uint))
  (match (map-get? listings listing-id)
    listing
    (let (
      (price (get price listing))
      (seller (get seller listing))
      (fee (calculate-fee price))
      (seller-amount (- price fee))
    )
      (asserts! (get active listing) err-listing-not-found)
      (asserts! (<= stacks-block-height (get expires-at listing)) err-listing-expired)
      (asserts! (>= (stx-get-balance tx-sender) price) err-insufficient-funds)
      
      ;; Transfer payment
      (try! (stx-transfer? seller-amount tx-sender seller))
      (try! (stx-transfer? fee tx-sender treasury))
      
      ;; Mark listing as inactive
      (map-set listings listing-id (merge listing { active: false }))
      
      ;; Update stats
      (var-set total-volume (+ (var-get total-volume) price))
      (var-set total-sales (+ (var-get total-sales) u1))
      (var-set total-fees (+ (var-get total-fees) fee))
      
      (map-set seller-stats seller
        (merge (get-seller-stats seller)
          { 
            total-sales: (+ (get total-sales (get-seller-stats seller)) u1),
            total-volume: (+ (get total-volume (get-seller-stats seller)) price)
          }
        )
      )
      
      (map-set buyer-stats tx-sender
        (merge (get-buyer-stats tx-sender)
          { 
            total-purchases: (+ (get total-purchases (get-buyer-stats tx-sender)) u1),
            total-spent: (+ (get total-spent (get-buyer-stats tx-sender)) price)
          }
        )
      )
      
      (ok { listing-id: listing-id, price: price, fee: fee, seller: seller })
    )
    err-listing-not-found
  )
)

;; Cancel listing
(define-public (cancel-listing (listing-id uint))
  (match (map-get? listings listing-id)
    listing
    (begin
      (asserts! (is-eq (get seller listing) tx-sender) err-not-listing-owner)
      (map-set listings listing-id (merge listing { active: false }))
      (ok { listing-id: listing-id })
    )
    err-listing-not-found
  )
)

;; Create auction
(define-public (create-auction (nft-contract principal) (token-id uint) (start-price uint) (reserve-price uint) (duration uint))
  (let (
    (auction-id (var-get auction-nonce))
  )
    (asserts! (> start-price u0) err-invalid-price)
    (asserts! (>= reserve-price start-price) err-invalid-price)
    
    ;; Create auction
    (map-set auctions auction-id {
      seller: tx-sender,
      nft-contract: nft-contract,
      token-id: token-id,
      start-price: start-price,
      reserve-price: reserve-price,
      current-bid: u0,
      highest-bidder: none,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height duration),
      active: true
    })
    
    (var-set auction-nonce (+ auction-id u1))
    
    (ok { auction-id: auction-id, end-block: (+ stacks-block-height duration) })
  )
)

;; Place bid on auction
(define-public (place-bid (auction-id uint) (bid-amount uint))
  (match (map-get? auctions auction-id)
    auction
    (let (
      (min-bid (if (> (get current-bid auction) u0) 
                 (+ (get current-bid auction) u1000000) ;; Min increment: 1 STX
                 (get start-price auction)))
    )
      (asserts! (get active auction) err-listing-not-found)
      (asserts! (<= stacks-block-height (get end-block auction)) err-auction-ended)
      (asserts! (>= bid-amount min-bid) err-bid-too-low)
      (asserts! (>= (stx-get-balance tx-sender) bid-amount) err-insufficient-funds)
      
      ;; Update auction
      (map-set auctions auction-id 
        (merge auction {
          current-bid: bid-amount,
          highest-bidder: (some tx-sender)
        })
      )
      
      (ok { auction-id: auction-id, bid: bid-amount })
    )
    err-listing-not-found
  )
)

;; End auction and settle
(define-public (end-auction (auction-id uint))
  (match (map-get? auctions auction-id)
    auction
    (begin
      (asserts! (get active auction) err-listing-not-found)
      (asserts! (> stacks-block-height (get end-block auction)) err-auction-active)
      
      (match (get highest-bidder auction)
        winner
        (let (
          (price (get current-bid auction))
          (fee (calculate-fee price))
          (seller-amount (- price fee))
          (seller (get seller auction))
        )
          ;; Check if reserve met
          (if (>= price (get reserve-price auction))
            (begin
              ;; Transfer payment
              (try! (stx-transfer? seller-amount winner seller))
              (try! (stx-transfer? fee winner treasury))
              
              ;; Mark auction complete
              (map-set auctions auction-id (merge auction { active: false }))
              
              ;; Update stats
              (var-set total-volume (+ (var-get total-volume) price))
              (var-set total-sales (+ (var-get total-sales) u1))
              (var-set total-fees (+ (var-get total-fees) fee))
              
              (ok { auction-id: auction-id, winner: winner, price: price, settled: true })
            )
            (begin
              ;; Reserve not met - cancel auction
              (map-set auctions auction-id (merge auction { active: false }))
              (ok { auction-id: auction-id, winner: winner, price: price, settled: false })
            )
          )
        )
        ;; No bids - just close
        (begin
          (map-set auctions auction-id (merge auction { active: false }))
          (ok { auction-id: auction-id, winner: tx-sender, price: u0, settled: false })
        )
      )
    )
    err-listing-not-found
  )
)

;; Update listing price
(define-public (update-price (listing-id uint) (new-price uint))
  (match (map-get? listings listing-id)
    listing
    (begin
      (asserts! (is-eq (get seller listing) tx-sender) err-not-listing-owner)
      (asserts! (> new-price u0) err-invalid-price)
      (map-set listings listing-id (merge listing { price: new-price }))
      (ok { listing-id: listing-id, new-price: new-price })
    )
    err-listing-not-found
  )
)

