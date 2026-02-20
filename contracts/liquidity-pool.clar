;; Liquidity Pool Contract (AMM)
;; Automated Market Maker with constant product formula

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-liquidity (err u101))
(define-constant err-slippage-exceeded (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-pool-not-found (err u104))
(define-constant err-deadline-passed (err u105))
(define-constant err-zero-liquidity (err u106))
(define-constant err-pool-paused (err u107))

;; Fee settings
(define-constant swap-fee u30) ;; 0.3%
(define-constant protocol-fee u5) ;; 0.05% to protocol
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var pool-nonce uint u0)
(define-data-var total-protocol-fees uint u0)

;; Pool data
(define-map pools uint
  {
    token-a: principal,
    token-b: principal,
    reserve-a: uint,
    reserve-b: uint,
    total-shares: uint,
    created-at: uint,
    paused: bool
  }
)

;; Token pair to pool mapping
(define-map pair-to-pool { token-a: principal, token-b: principal } uint)

;; LP token balances
(define-map lp-balances { pool-id: uint, owner: principal } uint)

;; User stats
(define-map user-stats principal
  {
    total-swaps: uint,
    total-volume: uint,
    total-fees-paid: uint
  }
)

;; Public Functions

(define-public (create-pool (token-a principal) (token-b principal) (amount-a uint) (amount-b uint))
  (let (
    (pool-id (var-get pool-nonce))
    (initial-shares (sqrti (* amount-a amount-b)))
  )
    (asserts! (> amount-a u0) err-invalid-amount)
    (asserts! (> amount-b u0) err-invalid-amount)
    (asserts! (is-none (map-get? pair-to-pool { token-a: token-a, token-b: token-b })) err-pool-not-found)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount-a tx-sender (as-contract tx-sender)))
    (try! (stx-transfer? amount-b tx-sender (as-contract tx-sender)))
    
    ;; Create pool
    (map-set pools pool-id
      {
        token-a: token-a,
        token-b: token-b,
        reserve-a: amount-a,
        reserve-b: amount-b,
        total-shares: initial-shares,
        created-at: stacks-block-height,
        paused: false
      }
    )
    
    ;; Map pair to pool
    (map-set pair-to-pool { token-a: token-a, token-b: token-b } pool-id)
    (map-set pair-to-pool { token-a: token-b, token-b: token-a } pool-id)
    
    ;; Mint LP tokens
    (map-set lp-balances { pool-id: pool-id, owner: tx-sender } initial-shares)
    
    (var-set pool-nonce (+ pool-id u1))
    
    (ok { pool-id: pool-id, shares: initial-shares })
  )
)

(define-public (add-liquidity (pool-id uint) (amount-a uint) (amount-b uint) (min-shares uint))
  (match (map-get? pools pool-id)
    pool
    (let (
      (shares-a (/ (* amount-a (get total-shares pool)) (get reserve-a pool)))
      (shares-b (/ (* amount-b (get total-shares pool)) (get reserve-b pool)))
      (shares-minted (if (< shares-a shares-b) shares-a shares-b))
    )
      (asserts! (not (get paused pool)) err-pool-paused)
      (asserts! (> amount-a u0) err-invalid-amount)
      (asserts! (> amount-b u0) err-invalid-amount)
      (asserts! (>= shares-minted min-shares) err-slippage-exceeded)
      
      ;; Transfer tokens
      (try! (stx-transfer? amount-a tx-sender (as-contract tx-sender)))
      (try! (stx-transfer? amount-b tx-sender (as-contract tx-sender)))
      
      ;; Update pool
      (map-set pools pool-id
        (merge pool {
          reserve-a: (+ (get reserve-a pool) amount-a),
          reserve-b: (+ (get reserve-b pool) amount-b),
          total-shares: (+ (get total-shares pool) shares-minted)
        })
      )
      
      ;; Mint LP tokens
      (map-set lp-balances { pool-id: pool-id, owner: tx-sender }
        (+ (get-lp-balance pool-id tx-sender) shares-minted)
      )
      
      (ok { shares-minted: shares-minted, amount-a: amount-a, amount-b: amount-b })
    )
    err-pool-not-found
  )
)

(define-public (remove-liquidity (pool-id uint) (shares uint) (min-a uint) (min-b uint))
  (match (map-get? pools pool-id)
    pool
    (let (
      (user-shares (get-lp-balance pool-id tx-sender))
      (amount-a (/ (* shares (get reserve-a pool)) (get total-shares pool)))
      (amount-b (/ (* shares (get reserve-b pool)) (get total-shares pool)))
    )
      (asserts! (<= shares user-shares) err-insufficient-liquidity)
      (asserts! (>= amount-a min-a) err-slippage-exceeded)
      (asserts! (>= amount-b min-b) err-slippage-exceeded)
      
      ;; Transfer tokens back
      (try! (as-contract (stx-transfer? amount-a tx-sender tx-sender)))
      (try! (as-contract (stx-transfer? amount-b tx-sender tx-sender)))
      
      ;; Update pool
      (map-set pools pool-id
        (merge pool {
          reserve-a: (- (get reserve-a pool) amount-a),
          reserve-b: (- (get reserve-b pool) amount-b),
          total-shares: (- (get total-shares pool) shares)
        })
      )
      
      ;; Burn LP tokens
      (map-set lp-balances { pool-id: pool-id, owner: tx-sender }
        (- user-shares shares)
      )
      
      (ok { amount-a: amount-a, amount-b: amount-b, shares-burned: shares })
    )
    err-pool-not-found
  )
)

(define-public (swap-a-for-b (pool-id uint) (amount-in uint) (min-out uint) (deadline uint))
  (match (map-get? pools pool-id)
    pool
    (let (
      (fee (/ (* amount-in swap-fee) fee-denominator))
      (amount-in-after-fee (- amount-in fee))
      (amount-out (get-amount-out amount-in-after-fee (get reserve-a pool) (get reserve-b pool)))
      (protocol-fee-amount (/ (* fee protocol-fee) swap-fee))
    )
      (asserts! (not (get paused pool)) err-pool-paused)
      (asserts! (<= stacks-block-height deadline) err-deadline-passed)
      (asserts! (> amount-in u0) err-invalid-amount)
      (asserts! (>= amount-out min-out) err-slippage-exceeded)
      (asserts! (< amount-out (get reserve-b pool)) err-insufficient-liquidity)
      
      ;; Transfer token A in
      (try! (stx-transfer? amount-in tx-sender (as-contract tx-sender)))
      
      ;; Transfer token B out
      (try! (as-contract (stx-transfer? amount-out tx-sender tx-sender)))
      
      ;; Update pool reserves
      (map-set pools pool-id
        (merge pool {
          reserve-a: (+ (get reserve-a pool) amount-in-after-fee),
          reserve-b: (- (get reserve-b pool) amount-out)
        })
      )
      
      ;; Update stats
      (var-set total-protocol-fees (+ (var-get total-protocol-fees) protocol-fee-amount))
      
      (map-set user-stats tx-sender
        (merge (get-user-stats tx-sender)
          {
            total-swaps: (+ (get total-swaps (get-user-stats tx-sender)) u1),
            total-volume: (+ (get total-volume (get-user-stats tx-sender)) amount-in),
            total-fees-paid: (+ (get total-fees-paid (get-user-stats tx-sender)) fee)
          }
        )
      )
      
      (ok { amount-in: amount-in, amount-out: amount-out, fee: fee })
    )
    err-pool-not-found
  )
)

(define-public (swap-b-for-a (pool-id uint) (amount-in uint) (min-out uint) (deadline uint))
  (match (map-get? pools pool-id)
    pool
    (let (
      (fee (/ (* amount-in swap-fee) fee-denominator))
      (amount-in-after-fee (- amount-in fee))
      (amount-out (get-amount-out amount-in-after-fee (get reserve-b pool) (get reserve-a pool)))
      (protocol-fee-amount (/ (* fee protocol-fee) swap-fee))
    )
      (asserts! (not (get paused pool)) err-pool-paused)
      (asserts! (<= stacks-block-height deadline) err-deadline-passed)
      (asserts! (> amount-in u0) err-invalid-amount)
      (asserts! (>= amount-out min-out) err-slippage-exceeded)
      (asserts! (< amount-out (get reserve-a pool)) err-insufficient-liquidity)
      
      ;; Transfer token B in
      (try! (stx-transfer? amount-in tx-sender (as-contract tx-sender)))
      
      ;; Transfer token A out
      (try! (as-contract (stx-transfer? amount-out tx-sender tx-sender)))
      
      ;; Update pool reserves
      (map-set pools pool-id
        (merge pool {
          reserve-b: (+ (get reserve-b pool) amount-in-after-fee),
          reserve-a: (- (get reserve-a pool) amount-out)
        })
      )
      
      ;; Update stats
      (var-set total-protocol-fees (+ (var-get total-protocol-fees) protocol-fee-amount))
      
      (ok { amount-in: amount-in, amount-out: amount-out, fee: fee })
    )
    err-pool-not-found
  )
)

;; Read-only Functions

(define-read-only (get-pool (pool-id uint))
  (map-get? pools pool-id)
)

(define-read-only (get-pool-by-pair (token-a principal) (token-b principal))
  (map-get? pair-to-pool { token-a: token-a, token-b: token-b })
)

(define-read-only (get-lp-balance (pool-id uint) (owner principal))
  (default-to u0 (map-get? lp-balances { pool-id: pool-id, owner: owner }))
)

(define-read-only (get-amount-out (amount-in uint) (reserve-in uint) (reserve-out uint))
  (let (
    (numerator (* amount-in reserve-out))
    (denominator (+ reserve-in amount-in))
  )
    (/ numerator denominator)
  )
)

(define-read-only (get-amount-in (amount-out uint) (reserve-in uint) (reserve-out uint))
  (let (
    (numerator (* reserve-in amount-out))
    (denominator (- reserve-out amount-out))
  )
    (+ (/ numerator denominator) u1)
  )
)

(define-read-only (quote-swap-a-for-b (pool-id uint) (amount-in uint))
  (match (map-get? pools pool-id)
    pool
    (let (
      (fee (/ (* amount-in swap-fee) fee-denominator))
      (amount-in-after-fee (- amount-in fee))
      (amount-out (get-amount-out amount-in-after-fee (get reserve-a pool) (get reserve-b pool)))
    )
      (ok { amount-out: amount-out, fee: fee, price-impact: (calculate-price-impact amount-in (get reserve-a pool)) })
    )
    err-pool-not-found
  )
)

(define-read-only (quote-swap-b-for-a (pool-id uint) (amount-in uint))
  (match (map-get? pools pool-id)
    pool
    (let (
      (fee (/ (* amount-in swap-fee) fee-denominator))
      (amount-in-after-fee (- amount-in fee))
      (amount-out (get-amount-out amount-in-after-fee (get reserve-b pool) (get reserve-a pool)))
    )
      (ok { amount-out: amount-out, fee: fee, price-impact: (calculate-price-impact amount-in (get reserve-b pool)) })
    )
    err-pool-not-found
  )
)

(define-read-only (calculate-price-impact (amount-in uint) (reserve uint))
  (/ (* amount-in fee-denominator) reserve)
)

(define-read-only (get-pool-price (pool-id uint))
  (match (map-get? pools pool-id)
    pool
    (ok {
      price-a-in-b: (/ (* (get reserve-b pool) u1000000) (get reserve-a pool)),
      price-b-in-a: (/ (* (get reserve-a pool) u1000000) (get reserve-b pool))
    })
    err-pool-not-found
  )
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { total-swaps: u0, total-volume: u0, total-fees-paid: u0 }
    (map-get? user-stats user)
  )
)

(define-read-only (get-platform-stats)
  {
    total-pools: (var-get pool-nonce),
    total-protocol-fees: (var-get total-protocol-fees),
    swap-fee: swap-fee,
    protocol-fee: protocol-fee
  }
)

;; Helper function for square root (integer)
(define-read-only (sqrti (n uint))
  (if (<= n u1)
    n
    (let (
      (x (/ (+ n u1) u2))
    )
      (sqrti-iter n x)
    )
  )
)

(define-private (sqrti-iter (n uint) (x uint))
  (let (
    (x-new (/ (+ x (/ n x)) u2))
  )
    (if (>= x-new x)
      x
      (sqrti-iter n x-new)
    )
  )
)

;; Admin Functions

(define-public (toggle-pool-pause (pool-id uint))
  (match (map-get? pools pool-id)
    pool
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (map-set pools pool-id (merge pool { paused: (not (get paused pool)) }))
      (ok (not (get paused pool)))
    )
    err-pool-not-found
  )
)

(define-public (withdraw-protocol-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get total-protocol-fees)) err-insufficient-liquidity)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set total-protocol-fees (- (var-get total-protocol-fees) amount))
    (ok amount)
  )
)
