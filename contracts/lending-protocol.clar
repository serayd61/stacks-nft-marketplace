;; Lending Protocol Contract
;; Deposit collateral and borrow against it

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-insufficient-liquidity (err u102))
(define-constant err-loan-not-found (err u103))
(define-constant err-loan-healthy (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-already-has-loan (err u106))
(define-constant err-protocol-paused (err u107))
(define-constant err-max-ltv-exceeded (err u108))

;; Protocol parameters
(define-constant max-ltv u7500) ;; 75% max loan-to-value
(define-constant liquidation-threshold u8500) ;; 85% liquidation threshold
(define-constant liquidation-bonus u500) ;; 5% bonus for liquidators
(define-constant base-rate u200) ;; 2% base interest rate
(define-constant rate-slope u1000) ;; Interest rate slope
(define-constant denominator u10000)

;; Data Variables
(define-data-var total-deposits uint u0)
(define-data-var total-borrows uint u0)
(define-data-var protocol-paused bool false)
(define-data-var reserve-factor uint u1000) ;; 10% to reserves
(define-data-var total-reserves uint u0)
(define-data-var loan-nonce uint u0)

;; User deposits
(define-map deposits principal
  {
    amount: uint,
    deposited-at: uint,
    last-update: uint
  }
)

;; Active loans
(define-map loans uint
  {
    borrower: principal,
    collateral: uint,
    borrowed: uint,
    interest-accrued: uint,
    created-at: uint,
    last-update: uint,
    active: bool
  }
)

;; User to loan mapping
(define-map user-loans principal uint)

;; Public Functions

(define-public (deposit (amount uint))
  (begin
    (asserts! (not (var-get protocol-paused)) err-protocol-paused)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update deposit record
    (map-set deposits tx-sender
      {
        amount: (+ (get-deposit-amount tx-sender) amount),
        deposited-at: stacks-block-height,
        last-update: stacks-block-height
      }
    )
    
    (var-set total-deposits (+ (var-get total-deposits) amount))
    
    (ok { deposited: amount, total: (get-deposit-amount tx-sender) })
  )
)

(define-public (withdraw (amount uint))
  (let (
    (user-deposit (get-deposit-amount tx-sender))
    (available (get-available-liquidity))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount user-deposit) err-insufficient-collateral)
    (asserts! (<= amount available) err-insufficient-liquidity)
    
    ;; Transfer tokens to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update deposit record
    (map-set deposits tx-sender
      {
        amount: (- user-deposit amount),
        deposited-at: stacks-block-height,
        last-update: stacks-block-height
      }
    )
    
    (var-set total-deposits (- (var-get total-deposits) amount))
    
    (ok { withdrawn: amount, remaining: (- user-deposit amount) })
  )
)

(define-public (borrow (collateral-amount uint) (borrow-amount uint))
  (let (
    (loan-id (var-get loan-nonce))
    (max-borrow (/ (* collateral-amount max-ltv) denominator))
  )
    (asserts! (not (var-get protocol-paused)) err-protocol-paused)
    (asserts! (> collateral-amount u0) err-invalid-amount)
    (asserts! (> borrow-amount u0) err-invalid-amount)
    (asserts! (<= borrow-amount max-borrow) err-max-ltv-exceeded)
    (asserts! (<= borrow-amount (get-available-liquidity)) err-insufficient-liquidity)
    (asserts! (is-none (map-get? user-loans tx-sender)) err-already-has-loan)
    
    ;; Transfer collateral to contract
    (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
    
    ;; Transfer borrowed amount to user
    (try! (as-contract (stx-transfer? borrow-amount tx-sender tx-sender)))
    
    ;; Create loan
    (map-set loans loan-id
      {
        borrower: tx-sender,
        collateral: collateral-amount,
        borrowed: borrow-amount,
        interest-accrued: u0,
        created-at: stacks-block-height,
        last-update: stacks-block-height,
        active: true
      }
    )
    
    (map-set user-loans tx-sender loan-id)
    
    (var-set loan-nonce (+ loan-id u1))
    (var-set total-borrows (+ (var-get total-borrows) borrow-amount))
    
    (ok { loan-id: loan-id, collateral: collateral-amount, borrowed: borrow-amount })
  )
)

(define-public (repay (loan-id uint) (amount uint))
  (match (map-get? loans loan-id)
    loan
    (let (
      (total-owed (+ (get borrowed loan) (calculate-interest loan-id)))
      (repay-amount (if (> amount total-owed) total-owed amount))
      (remaining (- total-owed repay-amount))
    )
      (asserts! (is-eq (get borrower loan) tx-sender) err-loan-not-found)
      (asserts! (get active loan) err-loan-not-found)
      (asserts! (> amount u0) err-invalid-amount)
      
      ;; Transfer repayment to contract
      (try! (stx-transfer? repay-amount tx-sender (as-contract tx-sender)))
      
      (if (is-eq remaining u0)
        ;; Full repayment - return collateral
        (begin
          (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
          (map-set loans loan-id (merge loan { active: false, borrowed: u0, interest-accrued: u0 }))
          (map-delete user-loans tx-sender)
          (var-set total-borrows (- (var-get total-borrows) (get borrowed loan)))
          (ok { repaid: repay-amount, remaining: u0, collateral-returned: (get collateral loan) })
        )
        ;; Partial repayment
        (begin
          (map-set loans loan-id 
            (merge loan { 
              borrowed: remaining,
              interest-accrued: u0,
              last-update: stacks-block-height
            })
          )
          (ok { repaid: repay-amount, remaining: remaining, collateral-returned: u0 })
        )
      )
    )
    err-loan-not-found
  )
)

(define-public (liquidate (loan-id uint))
  (match (map-get? loans loan-id)
    loan
    (let (
      (total-owed (+ (get borrowed loan) (calculate-interest loan-id)))
      (health-factor (calculate-health-factor loan-id))
      (liquidation-amount (/ (* (get collateral loan) (+ denominator liquidation-bonus)) denominator))
    )
      (asserts! (get active loan) err-loan-not-found)
      (asserts! (< health-factor denominator) err-loan-healthy)
      
      ;; Liquidator pays the debt
      (try! (stx-transfer? total-owed tx-sender (as-contract tx-sender)))
      
      ;; Liquidator receives collateral + bonus
      (try! (as-contract (stx-transfer? liquidation-amount tx-sender tx-sender)))
      
      ;; Close loan
      (map-set loans loan-id (merge loan { active: false, borrowed: u0 }))
      (map-delete user-loans (get borrower loan))
      
      (var-set total-borrows (- (var-get total-borrows) (get borrowed loan)))
      
      (ok { 
        liquidated: true, 
        debt-paid: total-owed, 
        collateral-received: liquidation-amount,
        borrower: (get borrower loan)
      })
    )
    err-loan-not-found
  )
)

(define-public (add-collateral (loan-id uint) (amount uint))
  (match (map-get? loans loan-id)
    loan
    (begin
      (asserts! (is-eq (get borrower loan) tx-sender) err-loan-not-found)
      (asserts! (get active loan) err-loan-not-found)
      (asserts! (> amount u0) err-invalid-amount)
      
      ;; Transfer additional collateral
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update loan
      (map-set loans loan-id
        (merge loan { collateral: (+ (get collateral loan) amount) })
      )
      
      (ok { new-collateral: (+ (get collateral loan) amount) })
    )
    err-loan-not-found
  )
)

;; Read-only Functions

(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id)
)

(define-read-only (get-user-loan-id (user principal))
  (map-get? user-loans user)
)

(define-read-only (get-deposit-amount (user principal))
  (default-to u0 (get amount (map-get? deposits user)))
)

(define-read-only (get-available-liquidity)
  (- (var-get total-deposits) (var-get total-borrows))
)

(define-read-only (calculate-interest (loan-id uint))
  (match (map-get? loans loan-id)
    loan
    (let (
      (blocks-elapsed (- stacks-block-height (get last-update loan)))
      (rate (get-current-rate))
      (interest (/ (* (get borrowed loan) rate blocks-elapsed) (* denominator u144000))) ;; ~1 day in blocks
    )
      interest
    )
    u0
  )
)

(define-read-only (get-current-rate)
  (let (
    (utilization (get-utilization-rate))
  )
    (+ base-rate (/ (* utilization rate-slope) denominator))
  )
)

(define-read-only (get-utilization-rate)
  (if (is-eq (var-get total-deposits) u0)
    u0
    (/ (* (var-get total-borrows) denominator) (var-get total-deposits))
  )
)

(define-read-only (calculate-health-factor (loan-id uint))
  (match (map-get? loans loan-id)
    loan
    (let (
      (total-owed (+ (get borrowed loan) (calculate-interest loan-id)))
      (collateral-value (/ (* (get collateral loan) liquidation-threshold) denominator))
    )
      (if (is-eq total-owed u0)
        u999999
        (/ (* collateral-value denominator) total-owed)
      )
    )
    u0
  )
)

(define-read-only (get-max-borrow (collateral uint))
  (/ (* collateral max-ltv) denominator)
)

(define-read-only (get-protocol-stats)
  {
    total-deposits: (var-get total-deposits),
    total-borrows: (var-get total-borrows),
    available-liquidity: (get-available-liquidity),
    utilization-rate: (get-utilization-rate),
    current-rate: (get-current-rate),
    total-reserves: (var-get total-reserves),
    total-loans: (var-get loan-nonce),
    protocol-paused: (var-get protocol-paused)
  }
)

;; Admin Functions

(define-public (toggle-protocol-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set protocol-paused (not (var-get protocol-paused)))
    (ok (var-get protocol-paused))
  )
)

(define-public (set-reserve-factor (new-factor uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set reserve-factor new-factor)
    (ok new-factor)
  )
)

(define-public (withdraw-reserves (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get total-reserves)) err-insufficient-liquidity)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set total-reserves (- (var-get total-reserves) amount))
    (ok amount)
  )
)
