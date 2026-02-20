;; NFT Fractional Ownership Contract
;; Fractionalize NFTs into fungible tokens

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-owner (err u101))
(define-constant err-already-fractionalized (err u102))
(define-constant err-not-fractionalized (err u103))
(define-constant err-insufficient-shares (err u104))
(define-constant err-buyout-active (err u105))
(define-constant err-buyout-not-active (err u106))
(define-constant err-invalid-amount (err u107))
(define-constant err-transfer-failed (err u108))
(define-constant err-already-claimed (err u109))

;; Fraction token settings
(define-constant default-fractions u10000) ;; 10,000 shares per NFT

;; Data Variables
(define-data-var vault-nonce uint u0)

;; Fractionalized NFT vaults
(define-map vaults uint
  {
    nft-contract: principal,
    token-id: uint,
    owner: principal,
    total-fractions: uint,
    fractions-sold: uint,
    price-per-fraction: uint,
    created-at: uint,
    buyout-price: uint,
    buyout-active: bool,
    buyout-initiator: (optional principal)
  }
)

;; User fraction balances per vault
(define-map fraction-balances { vault-id: uint, owner: principal } uint)

;; Buyout contributions
(define-map buyout-contributions { vault-id: uint, contributor: principal } uint)

;; Claimed proceeds after buyout
(define-map claimed-proceeds { vault-id: uint, owner: principal } bool)

;; Public Functions

(define-public (fractionalize-nft 
  (nft-contract principal) 
  (token-id uint) 
  (total-fractions uint)
  (price-per-fraction uint))
  (let (
    (vault-id (var-get vault-nonce))
  )
    (asserts! (> total-fractions u0) err-invalid-amount)
    (asserts! (> price-per-fraction u0) err-invalid-amount)
    
    ;; Create vault
    (map-set vaults vault-id
      {
        nft-contract: nft-contract,
        token-id: token-id,
        owner: tx-sender,
        total-fractions: total-fractions,
        fractions-sold: u0,
        price-per-fraction: price-per-fraction,
        created-at: stacks-block-height,
        buyout-price: (* total-fractions price-per-fraction u2), ;; 2x initial price
        buyout-active: false,
        buyout-initiator: none
      }
    )
    
    ;; Give owner all fractions initially
    (map-set fraction-balances { vault-id: vault-id, owner: tx-sender } total-fractions)
    
    (var-set vault-nonce (+ vault-id u1))
    
    (ok { vault-id: vault-id, total-fractions: total-fractions })
  )
)

(define-public (buy-fractions (vault-id uint) (amount uint))
  (match (map-get? vaults vault-id)
    vault
    (let (
      (total-cost (* amount (get price-per-fraction vault)))
      (owner-balance (get-fraction-balance vault-id (get owner vault)))
    )
      (asserts! (not (get buyout-active vault)) err-buyout-active)
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (<= amount owner-balance) err-insufficient-shares)
      
      ;; Transfer payment
      (try! (stx-transfer? total-cost tx-sender (get owner vault)))
      
      ;; Update balances
      (map-set fraction-balances 
        { vault-id: vault-id, owner: (get owner vault) }
        (- owner-balance amount)
      )
      (map-set fraction-balances 
        { vault-id: vault-id, owner: tx-sender }
        (+ (get-fraction-balance vault-id tx-sender) amount)
      )
      
      ;; Update vault
      (map-set vaults vault-id
        (merge vault { fractions-sold: (+ (get fractions-sold vault) amount) })
      )
      
      (ok { amount: amount, cost: total-cost })
    )
    err-not-fractionalized
  )
)

(define-public (transfer-fractions (vault-id uint) (recipient principal) (amount uint))
  (let (
    (sender-balance (get-fraction-balance vault-id tx-sender))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount sender-balance) err-insufficient-shares)
    
    ;; Update balances
    (map-set fraction-balances 
      { vault-id: vault-id, owner: tx-sender }
      (- sender-balance amount)
    )
    (map-set fraction-balances 
      { vault-id: vault-id, owner: recipient }
      (+ (get-fraction-balance vault-id recipient) amount)
    )
    
    (ok { transferred: amount, to: recipient })
  )
)

(define-public (initiate-buyout (vault-id uint))
  (match (map-get? vaults vault-id)
    vault
    (begin
      (asserts! (not (get buyout-active vault)) err-buyout-active)
      
      ;; Initiator must pay buyout price
      (try! (stx-transfer? (get buyout-price vault) tx-sender (as-contract tx-sender)))
      
      ;; Activate buyout
      (map-set vaults vault-id
        (merge vault { 
          buyout-active: true,
          buyout-initiator: (some tx-sender)
        })
      )
      
      ;; Record contribution
      (map-set buyout-contributions 
        { vault-id: vault-id, contributor: tx-sender }
        (get buyout-price vault)
      )
      
      (ok { vault-id: vault-id, buyout-price: (get buyout-price vault) })
    )
    err-not-fractionalized
  )
)

(define-public (claim-buyout-proceeds (vault-id uint))
  (match (map-get? vaults vault-id)
    vault
    (let (
      (user-fractions (get-fraction-balance vault-id tx-sender))
      (share-of-buyout (/ (* (get buyout-price vault) user-fractions) (get total-fractions vault)))
    )
      (asserts! (get buyout-active vault) err-buyout-not-active)
      (asserts! (> user-fractions u0) err-insufficient-shares)
      (asserts! (not (default-to false (map-get? claimed-proceeds { vault-id: vault-id, owner: tx-sender }))) err-already-claimed)
      
      ;; Transfer proceeds
      (try! (as-contract (stx-transfer? share-of-buyout tx-sender tx-sender)))
      
      ;; Mark as claimed
      (map-set claimed-proceeds { vault-id: vault-id, owner: tx-sender } true)
      
      ;; Clear user's fractions
      (map-set fraction-balances { vault-id: vault-id, owner: tx-sender } u0)
      
      (ok { claimed: share-of-buyout, fractions-redeemed: user-fractions })
    )
    err-not-fractionalized
  )
)

(define-public (update-fraction-price (vault-id uint) (new-price uint))
  (match (map-get? vaults vault-id)
    vault
    (begin
      (asserts! (is-eq (get owner vault) tx-sender) err-not-owner)
      (asserts! (not (get buyout-active vault)) err-buyout-active)
      (asserts! (> new-price u0) err-invalid-amount)
      
      (map-set vaults vault-id
        (merge vault { 
          price-per-fraction: new-price,
          buyout-price: (* (get total-fractions vault) new-price u2)
        })
      )
      
      (ok { new-price: new-price })
    )
    err-not-fractionalized
  )
)

(define-public (redeem-nft (vault-id uint))
  (match (map-get? vaults vault-id)
    vault
    (let (
      (user-fractions (get-fraction-balance vault-id tx-sender))
    )
      (asserts! (not (get buyout-active vault)) err-buyout-active)
      (asserts! (is-eq user-fractions (get total-fractions vault)) err-insufficient-shares)
      
      ;; User owns all fractions, can redeem NFT
      (map-set fraction-balances { vault-id: vault-id, owner: tx-sender } u0)
      
      ;; Mark vault as inactive (NFT redeemed)
      (map-delete vaults vault-id)
      
      (ok { redeemed: true, nft-contract: (get nft-contract vault), token-id: (get token-id vault) })
    )
    err-not-fractionalized
  )
)

;; Read-only Functions

(define-read-only (get-vault (vault-id uint))
  (map-get? vaults vault-id)
)

(define-read-only (get-fraction-balance (vault-id uint) (owner principal))
  (default-to u0 (map-get? fraction-balances { vault-id: vault-id, owner: owner }))
)

(define-read-only (get-fraction-value (vault-id uint) (amount uint))
  (match (map-get? vaults vault-id)
    vault (* amount (get price-per-fraction vault))
    u0
  )
)

(define-read-only (get-ownership-percentage (vault-id uint) (owner principal))
  (match (map-get? vaults vault-id)
    vault
    (let (
      (user-fractions (get-fraction-balance vault-id owner))
    )
      (/ (* user-fractions u10000) (get total-fractions vault)) ;; Returns basis points
    )
    u0
  )
)

(define-read-only (get-buyout-contribution (vault-id uint) (contributor principal))
  (default-to u0 (map-get? buyout-contributions { vault-id: vault-id, contributor: contributor }))
)

(define-read-only (has-claimed-proceeds (vault-id uint) (owner principal))
  (default-to false (map-get? claimed-proceeds { vault-id: vault-id, owner: owner }))
)

(define-read-only (calculate-buyout-share (vault-id uint) (owner principal))
  (match (map-get? vaults vault-id)
    vault
    (let (
      (user-fractions (get-fraction-balance vault-id owner))
    )
      (/ (* (get buyout-price vault) user-fractions) (get total-fractions vault))
    )
    u0
  )
)

(define-read-only (get-total-vaults)
  (var-get vault-nonce)
)
