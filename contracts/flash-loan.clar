;; Flash Loan Contract
;; Uncollateralized loans that must be repaid within the same transaction

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-liquidity (err u101))
(define-constant err-loan-not-repaid (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-pool-paused (err u104))
(define-constant err-callback-failed (err u105))
(define-constant err-reentrancy (err u106))

;; Flash loan fee: 0.09%
(define-constant flash-fee u9)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var total-liquidity uint u0)
(define-data-var total-loans uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var pool-paused bool false)
(define-data-var loan-in-progress bool false)
(define-data-var protocol-fee-share uint u1000) ;; 10% of fees to protocol

;; Liquidity providers
(define-map liquidity-providers principal
  {
    deposited: uint,
    shares: uint,
    deposited-at: uint
  }
)

;; Total shares for proportional fee distribution
(define-data-var total-shares uint u0)

;; Flash loan stats
(define-map loan-stats principal
  {
    total-borrowed: uint,
    total-fees-paid: uint,
    loan-count: uint
  }
)

;; Approved callback contracts
(define-map approved-callbacks principal bool)

;; Public Functions

(define-public (deposit-liquidity (amount uint))
  (let (
    (current-liquidity (var-get total-liquidity))
    (current-shares (var-get total-shares))
    (new-shares (if (is-eq current-shares u0)
                  amount
                  (/ (* amount current-shares) current-liquidity)))
    (provider-info (get-provider-info tx-sender))
  )
    (asserts! (not (var-get pool-paused)) err-pool-paused)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update provider info
    (map-set liquidity-providers tx-sender
      {
        deposited: (+ (get deposited provider-info) amount),
        shares: (+ (get shares provider-info) new-shares),
        deposited-at: stacks-block-height
      }
    )
    
    (var-set total-liquidity (+ current-liquidity amount))
    (var-set total-shares (+ current-shares new-shares))
    
    (ok { deposited: amount, shares: new-shares })
  )
)

(define-public (withdraw-liquidity (shares uint))
  (let (
    (provider-info (get-provider-info tx-sender))
    (current-liquidity (var-get total-liquidity))
    (current-shares (var-get total-shares))
    (withdraw-amount (/ (* shares current-liquidity) current-shares))
  )
    (asserts! (not (var-get loan-in-progress)) err-reentrancy)
    (asserts! (<= shares (get shares provider-info)) err-insufficient-liquidity)
    (asserts! (<= withdraw-amount current-liquidity) err-insufficient-liquidity)
    
    ;; Transfer tokens to provider
    (try! (as-contract (stx-transfer? withdraw-amount tx-sender tx-sender)))
    
    ;; Update provider info
    (map-set liquidity-providers tx-sender
      {
        deposited: (- (get deposited provider-info) withdraw-amount),
        shares: (- (get shares provider-info) shares),
        deposited-at: (get deposited-at provider-info)
      }
    )
    
    (var-set total-liquidity (- current-liquidity withdraw-amount))
    (var-set total-shares (- current-shares shares))
    
    (ok { withdrawn: withdraw-amount, shares-burned: shares })
  )
)

(define-public (flash-loan (amount uint) (callback-contract principal))
  (let (
    (fee (calculate-flash-fee amount))
    (total-repayment (+ amount fee))
    (balance-before (stx-get-balance (as-contract tx-sender)))
  )
    (asserts! (not (var-get pool-paused)) err-pool-paused)
    (asserts! (not (var-get loan-in-progress)) err-reentrancy)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount (var-get total-liquidity)) err-insufficient-liquidity)
    (asserts! (is-callback-approved callback-contract) err-callback-failed)
    
    ;; Set reentrancy guard
    (var-set loan-in-progress true)
    
    ;; Transfer loan amount to borrower
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Execute callback (borrower uses the funds)
    ;; Note: In real implementation, this would call the callback contract
    ;; For now, we simulate the callback returning
    
    ;; Check repayment
    (let (
      (balance-after (stx-get-balance (as-contract tx-sender)))
    )
      ;; Borrower must have sent back amount + fee
      (asserts! (>= balance-after (+ balance-before fee)) err-loan-not-repaid)
      
      ;; Distribute fees
      (let (
        (protocol-share (/ (* fee (var-get protocol-fee-share)) fee-denominator))
        (lp-share (- fee protocol-share))
      )
        ;; Add LP share to liquidity pool
        (var-set total-liquidity (+ (var-get total-liquidity) lp-share))
        (var-set total-fees-collected (+ (var-get total-fees-collected) fee))
      )
      
      ;; Update stats
      (var-set total-loans (+ (var-get total-loans) u1))
      
      (map-set loan-stats tx-sender
        (merge (get-loan-stats tx-sender)
          {
            total-borrowed: (+ (get total-borrowed (get-loan-stats tx-sender)) amount),
            total-fees-paid: (+ (get total-fees-paid (get-loan-stats tx-sender)) fee),
            loan-count: (+ (get loan-count (get-loan-stats tx-sender)) u1)
          }
        )
      )
      
      ;; Reset reentrancy guard
      (var-set loan-in-progress false)
      
      (ok { borrowed: amount, fee: fee, repaid: total-repayment })
    )
  )
)

(define-public (repay-flash-loan (amount uint))
  (begin
    (asserts! (var-get loan-in-progress) err-loan-not-repaid)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok { repaid: amount })
  )
)

;; Read-only Functions

(define-read-only (get-provider-info (provider principal))
  (default-to 
    { deposited: u0, shares: u0, deposited-at: u0 }
    (map-get? liquidity-providers provider)
  )
)

(define-read-only (get-loan-stats (borrower principal))
  (default-to 
    { total-borrowed: u0, total-fees-paid: u0, loan-count: u0 }
    (map-get? loan-stats borrower)
  )
)

(define-read-only (calculate-flash-fee (amount uint))
  (/ (* amount flash-fee) fee-denominator)
)

(define-read-only (get-max-flash-loan)
  (var-get total-liquidity)
)

(define-read-only (get-provider-value (provider principal))
  (let (
    (provider-info (get-provider-info provider))
    (current-liquidity (var-get total-liquidity))
    (current-shares (var-get total-shares))
  )
    (if (is-eq current-shares u0)
      u0
      (/ (* (get shares provider-info) current-liquidity) current-shares)
    )
  )
)

(define-read-only (get-provider-earnings (provider principal))
  (let (
    (provider-info (get-provider-info provider))
    (current-value (get-provider-value provider))
  )
    (if (> current-value (get deposited provider-info))
      (- current-value (get deposited provider-info))
      u0
    )
  )
)

(define-read-only (is-callback-approved (callback principal))
  (default-to false (map-get? approved-callbacks callback))
)

(define-read-only (get-pool-stats)
  {
    total-liquidity: (var-get total-liquidity),
    total-shares: (var-get total-shares),
    total-loans: (var-get total-loans),
    total-fees-collected: (var-get total-fees-collected),
    flash-fee: flash-fee,
    pool-paused: (var-get pool-paused),
    loan-in-progress: (var-get loan-in-progress)
  }
)

(define-read-only (quote-flash-loan (amount uint))
  (let (
    (fee (calculate-flash-fee amount))
  )
    {
      amount: amount,
      fee: fee,
      total-repayment: (+ amount fee),
      available: (<= amount (var-get total-liquidity))
    }
  )
)

;; Admin Functions

(define-public (approve-callback (callback principal) (approved bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set approved-callbacks callback approved)
    (ok true)
  )
)

(define-public (toggle-pool-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set pool-paused (not (var-get pool-paused)))
    (ok (var-get pool-paused))
  )
)

(define-public (set-protocol-fee-share (new-share uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-share fee-denominator) err-invalid-amount)
    (var-set protocol-fee-share new-share)
    (ok new-share)
  )
)

(define-public (withdraw-protocol-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get loan-in-progress)) err-reentrancy)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set total-liquidity (- (var-get total-liquidity) amount))
    (ok amount)
  )
)
