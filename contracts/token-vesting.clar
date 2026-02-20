;; Token Vesting Contract
;; Linear and cliff vesting schedules for token distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-beneficiary (err u101))
(define-constant err-schedule-not-found (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-cliff-not-reached (err u104))
(define-constant err-nothing-to-claim (err u105))
(define-constant err-invalid-params (err u106))
(define-constant err-schedule-revoked (err u107))
(define-constant err-insufficient-funds (err u108))

;; Data Variables
(define-data-var schedule-nonce uint u0)
(define-data-var total-vested uint u0)
(define-data-var total-claimed uint u0)

;; Vesting schedules
(define-map vesting-schedules uint
  {
    beneficiary: principal,
    token-contract: principal,
    total-amount: uint,
    claimed-amount: uint,
    start-block: uint,
    cliff-block: uint,
    end-block: uint,
    revocable: bool,
    revoked: bool,
    created-at: uint
  }
)

;; Beneficiary to schedule mapping
(define-map beneficiary-schedules principal (list 20 uint))

;; Admin list for creating schedules
(define-map schedule-admins principal bool)

;; Public Functions

(define-public (create-vesting-schedule
  (beneficiary principal)
  (token-contract principal)
  (total-amount uint)
  (cliff-duration uint)
  (vesting-duration uint)
  (revocable bool))
  (let (
    (schedule-id (var-get schedule-nonce))
    (start-block stacks-block-height)
    (cliff-block (+ start-block cliff-duration))
    (end-block (+ start-block vesting-duration))
  )
    (asserts! (or (is-eq tx-sender contract-owner) (is-admin tx-sender)) err-owner-only)
    (asserts! (> total-amount u0) err-invalid-params)
    (asserts! (> vesting-duration cliff-duration) err-invalid-params)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Create schedule
    (map-set vesting-schedules schedule-id
      {
        beneficiary: beneficiary,
        token-contract: token-contract,
        total-amount: total-amount,
        claimed-amount: u0,
        start-block: start-block,
        cliff-block: cliff-block,
        end-block: end-block,
        revocable: revocable,
        revoked: false,
        created-at: stacks-block-height
      }
    )
    
    ;; Add to beneficiary's schedules
    (map-set beneficiary-schedules beneficiary
      (unwrap! (as-max-len? 
        (append (get-beneficiary-schedule-ids beneficiary) schedule-id) 
        u20) err-invalid-params)
    )
    
    (var-set schedule-nonce (+ schedule-id u1))
    (var-set total-vested (+ (var-get total-vested) total-amount))
    
    (ok { schedule-id: schedule-id, cliff-block: cliff-block, end-block: end-block })
  )
)

(define-public (claim-vested-tokens (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
    (let (
      (claimable (calculate-claimable schedule-id))
    )
      (asserts! (is-eq (get beneficiary schedule) tx-sender) err-not-beneficiary)
      (asserts! (not (get revoked schedule)) err-schedule-revoked)
      (asserts! (>= stacks-block-height (get cliff-block schedule)) err-cliff-not-reached)
      (asserts! (> claimable u0) err-nothing-to-claim)
      
      ;; Transfer claimable tokens
      (try! (as-contract (stx-transfer? claimable tx-sender tx-sender)))
      
      ;; Update schedule
      (map-set vesting-schedules schedule-id
        (merge schedule { claimed-amount: (+ (get claimed-amount schedule) claimable) })
      )
      
      (var-set total-claimed (+ (var-get total-claimed) claimable))
      
      (ok { claimed: claimable, total-claimed: (+ (get claimed-amount schedule) claimable) })
    )
    err-schedule-not-found
  )
)

(define-public (revoke-schedule (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
    (let (
      (vested (calculate-vested-amount schedule-id))
      (unvested (- (get total-amount schedule) vested))
    )
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (get revocable schedule) err-invalid-params)
      (asserts! (not (get revoked schedule)) err-schedule-revoked)
      
      ;; Return unvested tokens to owner
      (if (> unvested u0)
        (try! (as-contract (stx-transfer? unvested tx-sender contract-owner)))
        true
      )
      
      ;; Mark as revoked
      (map-set vesting-schedules schedule-id
        (merge schedule { revoked: true })
      )
      
      (ok { revoked: true, unvested-returned: unvested })
    )
    err-schedule-not-found
  )
)

(define-public (transfer-beneficiary (schedule-id uint) (new-beneficiary principal))
  (match (map-get? vesting-schedules schedule-id)
    schedule
    (begin
      (asserts! (is-eq (get beneficiary schedule) tx-sender) err-not-beneficiary)
      (asserts! (not (get revoked schedule)) err-schedule-revoked)
      
      ;; Update beneficiary
      (map-set vesting-schedules schedule-id
        (merge schedule { beneficiary: new-beneficiary })
      )
      
      (ok { new-beneficiary: new-beneficiary })
    )
    err-schedule-not-found
  )
)

;; Read-only Functions

(define-read-only (get-schedule (schedule-id uint))
  (map-get? vesting-schedules schedule-id)
)

(define-read-only (get-beneficiary-schedule-ids (beneficiary principal))
  (default-to (list) (map-get? beneficiary-schedules beneficiary))
)

(define-read-only (calculate-vested-amount (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
    (if (get revoked schedule)
      (get claimed-amount schedule)
      (if (< stacks-block-height (get cliff-block schedule))
        u0
        (if (>= stacks-block-height (get end-block schedule))
          (get total-amount schedule)
          (let (
            (elapsed (- stacks-block-height (get start-block schedule)))
            (total-duration (- (get end-block schedule) (get start-block schedule)))
          )
            (/ (* (get total-amount schedule) elapsed) total-duration)
          )
        )
      )
    )
    u0
  )
)

(define-read-only (calculate-claimable (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
    (let (
      (vested (calculate-vested-amount schedule-id))
    )
      (- vested (get claimed-amount schedule))
    )
    u0
  )
)

(define-read-only (get-vesting-progress (schedule-id uint))
  (match (map-get? vesting-schedules schedule-id)
    schedule
    (let (
      (vested (calculate-vested-amount schedule-id))
      (claimable (calculate-claimable schedule-id))
    )
      (ok {
        total-amount: (get total-amount schedule),
        vested-amount: vested,
        claimed-amount: (get claimed-amount schedule),
        claimable-amount: claimable,
        remaining: (- (get total-amount schedule) vested),
        vested-percentage: (/ (* vested u10000) (get total-amount schedule)),
        cliff-reached: (>= stacks-block-height (get cliff-block schedule)),
        fully-vested: (>= stacks-block-height (get end-block schedule)),
        revoked: (get revoked schedule)
      })
    )
    err-schedule-not-found
  )
)

(define-read-only (get-platform-stats)
  {
    total-schedules: (var-get schedule-nonce),
    total-vested: (var-get total-vested),
    total-claimed: (var-get total-claimed),
    total-locked: (- (var-get total-vested) (var-get total-claimed))
  }
)

(define-read-only (is-admin (account principal))
  (default-to false (map-get? schedule-admins account))
)

;; Admin Functions

(define-public (add-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set schedule-admins admin true)
    (ok true)
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete schedule-admins admin)
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)
  )
)
