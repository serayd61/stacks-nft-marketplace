;; NFT Rental Contract
;; Rent NFTs for a specified duration

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-owner (err u101))
(define-constant err-already-listed (err u102))
(define-constant err-not-listed (err u103))
(define-constant err-already-rented (err u104))
(define-constant err-not-rented (err u105))
(define-constant err-rental-active (err u106))
(define-constant err-rental-expired (err u107))
(define-constant err-insufficient-payment (err u108))
(define-constant err-invalid-duration (err u109))

;; Platform fee: 5%
(define-constant platform-fee u500)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var rental-nonce uint u0)
(define-data-var total-rentals uint u0)
(define-data-var total-volume uint u0)

;; Rental listings
(define-map rental-listings 
  { nft-contract: principal, token-id: uint }
  {
    owner: principal,
    price-per-block: uint,
    min-duration: uint,
    max-duration: uint,
    collateral: uint,
    listed-at: uint,
    active: bool
  }
)

;; Active rentals
(define-map active-rentals
  { nft-contract: principal, token-id: uint }
  {
    renter: principal,
    rental-id: uint,
    start-block: uint,
    end-block: uint,
    total-paid: uint,
    collateral-paid: uint
  }
)

;; User rental history
(define-map user-rentals principal
  {
    total-rented: uint,
    total-spent: uint,
    active-rentals: uint
  }
)

;; Owner rental stats
(define-map owner-stats principal
  {
    total-listings: uint,
    total-earned: uint,
    total-rentals: uint
  }
)

;; Public Functions

(define-public (list-for-rent 
  (nft-contract principal) 
  (token-id uint) 
  (price-per-block uint)
  (min-duration uint)
  (max-duration uint)
  (collateral uint))
  (begin
    (asserts! (> price-per-block u0) err-invalid-duration)
    (asserts! (> min-duration u0) err-invalid-duration)
    (asserts! (>= max-duration min-duration) err-invalid-duration)
    (asserts! (is-none (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })) err-already-listed)
    
    (map-set rental-listings 
      { nft-contract: nft-contract, token-id: token-id }
      {
        owner: tx-sender,
        price-per-block: price-per-block,
        min-duration: min-duration,
        max-duration: max-duration,
        collateral: collateral,
        listed-at: stacks-block-height,
        active: true
      }
    )
    
    ;; Update owner stats
    (map-set owner-stats tx-sender
      (merge (get-owner-stats tx-sender)
        { total-listings: (+ (get total-listings (get-owner-stats tx-sender)) u1) }
      )
    )
    
    (ok { nft-contract: nft-contract, token-id: token-id })
  )
)

(define-public (rent-nft (nft-contract principal) (token-id uint) (duration uint))
  (match (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
    listing
    (let (
      (rental-id (var-get rental-nonce))
      (total-price (* (get price-per-block listing) duration))
      (collateral (get collateral listing))
      (total-payment (+ total-price collateral))
      (fee (calculate-fee total-price))
      (owner-payment (- total-price fee))
    )
      (asserts! (get active listing) err-not-listed)
      (asserts! (is-none (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })) err-already-rented)
      (asserts! (>= duration (get min-duration listing)) err-invalid-duration)
      (asserts! (<= duration (get max-duration listing)) err-invalid-duration)
      
      ;; Transfer payment to owner
      (try! (stx-transfer? owner-payment tx-sender (get owner listing)))
      ;; Transfer fee to contract
      (try! (stx-transfer? fee tx-sender contract-owner))
      ;; Transfer collateral to contract (held in escrow)
      (if (> collateral u0)
        (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
        true
      )
      
      ;; Create rental record
      (map-set active-rentals
        { nft-contract: nft-contract, token-id: token-id }
        {
          renter: tx-sender,
          rental-id: rental-id,
          start-block: stacks-block-height,
          end-block: (+ stacks-block-height duration),
          total-paid: total-price,
          collateral-paid: collateral
        }
      )
      
      ;; Update stats
      (var-set rental-nonce (+ rental-id u1))
      (var-set total-rentals (+ (var-get total-rentals) u1))
      (var-set total-volume (+ (var-get total-volume) total-price))
      
      (map-set user-rentals tx-sender
        (merge (get-user-rentals tx-sender)
          {
            total-rented: (+ (get total-rented (get-user-rentals tx-sender)) u1),
            total-spent: (+ (get total-spent (get-user-rentals tx-sender)) total-price),
            active-rentals: (+ (get active-rentals (get-user-rentals tx-sender)) u1)
          }
        )
      )
      
      (map-set owner-stats (get owner listing)
        (merge (get-owner-stats (get owner listing))
          {
            total-earned: (+ (get total-earned (get-owner-stats (get owner listing))) owner-payment),
            total-rentals: (+ (get total-rentals (get-owner-stats (get owner listing))) u1)
          }
        )
      )
      
      (ok { rental-id: rental-id, end-block: (+ stacks-block-height duration), total-paid: total-payment })
    )
    err-not-listed
  )
)

(define-public (return-nft (nft-contract principal) (token-id uint))
  (match (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })
    rental
    (begin
      (asserts! (is-eq (get renter rental) tx-sender) err-not-owner)
      
      ;; Return collateral
      (if (> (get collateral-paid rental) u0)
        (try! (as-contract (stx-transfer? (get collateral-paid rental) tx-sender (get renter rental))))
        true
      )
      
      ;; Remove rental record
      (map-delete active-rentals { nft-contract: nft-contract, token-id: token-id })
      
      ;; Update user stats
      (map-set user-rentals tx-sender
        (merge (get-user-rentals tx-sender)
          { active-rentals: (- (get active-rentals (get-user-rentals tx-sender)) u1) }
        )
      )
      
      (ok { returned-at: stacks-block-height, collateral-returned: (get collateral-paid rental) })
    )
    err-not-rented
  )
)

(define-public (claim-collateral (nft-contract principal) (token-id uint))
  (match (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })
    rental
    (match (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
      listing
      (begin
        (asserts! (is-eq (get owner listing) tx-sender) err-not-owner)
        (asserts! (> stacks-block-height (get end-block rental)) err-rental-active)
        
        ;; Transfer collateral to owner (renter didn't return on time)
        (if (> (get collateral-paid rental) u0)
          (try! (as-contract (stx-transfer? (get collateral-paid rental) tx-sender tx-sender)))
          true
        )
        
        ;; Remove rental record
        (map-delete active-rentals { nft-contract: nft-contract, token-id: token-id })
        
        (ok { collateral-claimed: (get collateral-paid rental) })
      )
      err-not-listed
    )
    err-not-rented
  )
)

(define-public (cancel-listing (nft-contract principal) (token-id uint))
  (match (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
    listing
    (begin
      (asserts! (is-eq (get owner listing) tx-sender) err-not-owner)
      (asserts! (is-none (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })) err-rental-active)
      
      (map-set rental-listings 
        { nft-contract: nft-contract, token-id: token-id }
        (merge listing { active: false })
      )
      
      (ok true)
    )
    err-not-listed
  )
)

(define-public (update-listing 
  (nft-contract principal) 
  (token-id uint)
  (new-price uint)
  (new-min uint)
  (new-max uint)
  (new-collateral uint))
  (match (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
    listing
    (begin
      (asserts! (is-eq (get owner listing) tx-sender) err-not-owner)
      (asserts! (is-none (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })) err-rental-active)
      
      (map-set rental-listings 
        { nft-contract: nft-contract, token-id: token-id }
        (merge listing {
          price-per-block: new-price,
          min-duration: new-min,
          max-duration: new-max,
          collateral: new-collateral
        })
      )
      
      (ok true)
    )
    err-not-listed
  )
)

;; Read-only Functions

(define-read-only (get-listing (nft-contract principal) (token-id uint))
  (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-rental (nft-contract principal) (token-id uint))
  (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })
)

(define-read-only (get-user-rentals (user principal))
  (default-to 
    { total-rented: u0, total-spent: u0, active-rentals: u0 }
    (map-get? user-rentals user)
  )
)

(define-read-only (get-owner-stats (owner principal))
  (default-to 
    { total-listings: u0, total-earned: u0, total-rentals: u0 }
    (map-get? owner-stats owner)
  )
)

(define-read-only (calculate-rental-cost (nft-contract principal) (token-id uint) (duration uint))
  (match (map-get? rental-listings { nft-contract: nft-contract, token-id: token-id })
    listing
    (let (
      (rental-cost (* (get price-per-block listing) duration))
      (fee (calculate-fee rental-cost))
    )
      (ok {
        rental-cost: rental-cost,
        fee: fee,
        collateral: (get collateral listing),
        total: (+ rental-cost (get collateral listing))
      })
    )
    err-not-listed
  )
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount platform-fee) fee-denominator)
)

(define-read-only (is-rental-expired (nft-contract principal) (token-id uint))
  (match (map-get? active-rentals { nft-contract: nft-contract, token-id: token-id })
    rental (> stacks-block-height (get end-block rental))
    false
  )
)

(define-read-only (get-platform-stats)
  {
    total-rentals: (var-get total-rentals),
    total-volume: (var-get total-volume),
    platform-fee: platform-fee
  }
)
