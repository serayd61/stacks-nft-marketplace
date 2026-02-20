;; Token Airdrop Contract
;; Distribute tokens to multiple recipients with merkle proof verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-claimed (err u101))
(define-constant err-invalid-proof (err u102))
(define-constant err-airdrop-not-active (err u103))
(define-constant err-airdrop-expired (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-not-eligible (err u107))
(define-constant err-airdrop-not-found (err u108))

;; Data Variables
(define-data-var airdrop-nonce uint u0)
(define-data-var total-distributed uint u0)

;; Airdrop campaigns
(define-map airdrops uint
  {
    name: (string-ascii 64),
    token-contract: principal,
    total-amount: uint,
    claimed-amount: uint,
    amount-per-claim: uint,
    max-claims: uint,
    claims-count: uint,
    start-block: uint,
    end-block: uint,
    merkle-root: (optional (buff 32)),
    active: bool,
    creator: principal
  }
)

;; Claim tracking
(define-map claims { airdrop-id: uint, claimer: principal } bool)

;; Whitelist for non-merkle airdrops
(define-map whitelist { airdrop-id: uint, address: principal } uint)

;; Public Functions

(define-public (create-airdrop
  (name (string-ascii 64))
  (token-contract principal)
  (total-amount uint)
  (amount-per-claim uint)
  (max-claims uint)
  (duration uint)
  (merkle-root (optional (buff 32))))
  (let (
    (airdrop-id (var-get airdrop-nonce))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> total-amount u0) err-invalid-amount)
    (asserts! (> amount-per-claim u0) err-invalid-amount)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Create airdrop
    (map-set airdrops airdrop-id
      {
        name: name,
        token-contract: token-contract,
        total-amount: total-amount,
        claimed-amount: u0,
        amount-per-claim: amount-per-claim,
        max-claims: max-claims,
        claims-count: u0,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        merkle-root: merkle-root,
        active: true,
        creator: tx-sender
      }
    )
    
    (var-set airdrop-nonce (+ airdrop-id u1))
    
    (ok { airdrop-id: airdrop-id, end-block: (+ stacks-block-height duration) })
  )
)

(define-public (claim-airdrop (airdrop-id uint))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (let (
      (claim-amount (get-claim-amount airdrop-id tx-sender))
    )
      (asserts! (get active airdrop) err-airdrop-not-active)
      (asserts! (<= stacks-block-height (get end-block airdrop)) err-airdrop-expired)
      (asserts! (not (has-claimed airdrop-id tx-sender)) err-already-claimed)
      (asserts! (< (get claims-count airdrop) (get max-claims airdrop)) err-insufficient-funds)
      (asserts! (> claim-amount u0) err-not-eligible)
      (asserts! (<= (+ (get claimed-amount airdrop) claim-amount) (get total-amount airdrop)) err-insufficient-funds)
      
      ;; Transfer tokens
      (try! (as-contract (stx-transfer? claim-amount tx-sender tx-sender)))
      
      ;; Mark as claimed
      (map-set claims { airdrop-id: airdrop-id, claimer: tx-sender } true)
      
      ;; Update airdrop stats
      (map-set airdrops airdrop-id
        (merge airdrop {
          claimed-amount: (+ (get claimed-amount airdrop) claim-amount),
          claims-count: (+ (get claims-count airdrop) u1)
        })
      )
      
      (var-set total-distributed (+ (var-get total-distributed) claim-amount))
      
      (ok { claimed: claim-amount })
    )
    err-airdrop-not-found
  )
)

(define-public (add-to-whitelist (airdrop-id uint) (addresses (list 100 { address: principal, amount: uint })))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (fold add-whitelist-entry addresses airdrop-id)
    (ok true)
  )
)

(define-private (add-whitelist-entry (entry { address: principal, amount: uint }) (airdrop-id uint))
  (begin
    (map-set whitelist { airdrop-id: airdrop-id, address: (get address entry) } (get amount entry))
    airdrop-id
  )
)

(define-public (remove-from-whitelist (airdrop-id uint) (address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete whitelist { airdrop-id: airdrop-id, address: address })
    (ok true)
  )
)

(define-public (pause-airdrop (airdrop-id uint))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (map-set airdrops airdrop-id (merge airdrop { active: false }))
      (ok true)
    )
    err-airdrop-not-found
  )
)

(define-public (resume-airdrop (airdrop-id uint))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (map-set airdrops airdrop-id (merge airdrop { active: true }))
      (ok true)
    )
    err-airdrop-not-found
  )
)

(define-public (extend-airdrop (airdrop-id uint) (additional-blocks uint))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (map-set airdrops airdrop-id 
        (merge airdrop { end-block: (+ (get end-block airdrop) additional-blocks) })
      )
      (ok { new-end-block: (+ (get end-block airdrop) additional-blocks) })
    )
    err-airdrop-not-found
  )
)

(define-public (withdraw-unclaimed (airdrop-id uint))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (let (
      (unclaimed (- (get total-amount airdrop) (get claimed-amount airdrop)))
    )
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (> stacks-block-height (get end-block airdrop)) err-airdrop-not-active)
      (asserts! (> unclaimed u0) err-invalid-amount)
      
      ;; Transfer unclaimed tokens back to owner
      (try! (as-contract (stx-transfer? unclaimed tx-sender contract-owner)))
      
      ;; Deactivate airdrop
      (map-set airdrops airdrop-id (merge airdrop { active: false }))
      
      (ok { withdrawn: unclaimed })
    )
    err-airdrop-not-found
  )
)

;; Read-only Functions

(define-read-only (get-airdrop (airdrop-id uint))
  (map-get? airdrops airdrop-id)
)

(define-read-only (has-claimed (airdrop-id uint) (claimer principal))
  (default-to false (map-get? claims { airdrop-id: airdrop-id, claimer: claimer }))
)

(define-read-only (get-claim-amount (airdrop-id uint) (claimer principal))
  (match (map-get? whitelist { airdrop-id: airdrop-id, address: claimer })
    amount amount
    (match (map-get? airdrops airdrop-id)
      airdrop (get amount-per-claim airdrop)
      u0
    )
  )
)

(define-read-only (is-eligible (airdrop-id uint) (address principal))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (and
      (get active airdrop)
      (<= stacks-block-height (get end-block airdrop))
      (not (has-claimed airdrop-id address))
      (or 
        (is-some (map-get? whitelist { airdrop-id: airdrop-id, address: address }))
        (is-none (get merkle-root airdrop))
      )
    )
    false
  )
)

(define-read-only (get-airdrop-stats (airdrop-id uint))
  (match (map-get? airdrops airdrop-id)
    airdrop
    (ok {
      name: (get name airdrop),
      total-amount: (get total-amount airdrop),
      claimed-amount: (get claimed-amount airdrop),
      remaining: (- (get total-amount airdrop) (get claimed-amount airdrop)),
      claims-count: (get claims-count airdrop),
      max-claims: (get max-claims airdrop),
      active: (get active airdrop),
      expired: (> stacks-block-height (get end-block airdrop)),
      claim-percentage: (/ (* (get claimed-amount airdrop) u10000) (get total-amount airdrop))
    })
    err-airdrop-not-found
  )
)

(define-read-only (get-platform-stats)
  {
    total-airdrops: (var-get airdrop-nonce),
    total-distributed: (var-get total-distributed)
  }
)

(define-read-only (get-whitelist-amount (airdrop-id uint) (address principal))
  (map-get? whitelist { airdrop-id: airdrop-id, address: address })
)
