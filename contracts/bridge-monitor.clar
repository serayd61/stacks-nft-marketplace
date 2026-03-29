;; Stacks Bridge Monitor Contract
;; On-chain registry and monitoring for sBTC bridge transactions
;; Tracks peg-in/peg-out events and bridge health metrics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u500))
(define-constant err-not-found (err u501))
(define-constant err-not-authorized (err u502))
(define-constant err-invalid-amount (err u503))
(define-constant err-already-exists (err u504))
(define-constant err-invalid-status (err u505))
(define-constant err-threshold-not-met (err u506))

;; Bridge event types
(define-constant EVENT-PEG-IN u0)       ;; BTC -> sBTC
(define-constant EVENT-PEG-OUT u1)      ;; sBTC -> BTC
(define-constant EVENT-FAILED u2)
(define-constant EVENT-PENDING u3)
(define-constant EVENT-CONFIRMED u4)

;; Alert levels
(define-constant ALERT-NONE u0)
(define-constant ALERT-LOW u1)
(define-constant ALERT-MEDIUM u2)
(define-constant ALERT-HIGH u3)
(define-constant ALERT-CRITICAL u4)

;; Data Variables
(define-data-var event-nonce uint u0)
(define-data-var alert-nonce uint u0)
(define-data-var total-peg-in-volume uint u0)
(define-data-var total-peg-out-volume uint u0)
(define-data-var total-failed-events uint u0)
(define-data-var bridge-active bool true)
(define-data-var current-alert-level uint ALERT-NONE)
(define-data-var last-health-check uint u0)

;; Authorized reporters (oracles that can submit bridge events)
(define-map authorized-reporters principal bool)

;; Bridge events
(define-map bridge-events uint
  {
    event-type: uint,
    submitter: principal,
    btc-txid: (string-ascii 64),
    stacks-txid: (optional (string-ascii 64)),
    amount-sats: uint,
    amount-micro-sbtc: uint,
    from-address: (string-ascii 62),
    to-principal: (optional principal),
    status: uint,
    submitted-at: uint,
    confirmed-at: (optional uint),
    confirmation-blocks: uint,
    notes: (optional (string-ascii 200))
  }
)

;; Alert log
(define-map alerts uint
  {
    level: uint,
    message: (string-ascii 200),
    raised-at: uint,
    raised-by: principal,
    resolved: bool,
    resolved-at: (optional uint)
  }
)

;; Bridge health metrics (rolling 144-block / ~1 day window)
(define-map health-snapshots uint
  {
    block-height: uint,
    peg-in-count: uint,
    peg-out-count: uint,
    failed-count: uint,
    avg-confirmation-blocks: uint,
    total-volume-sats: uint,
    alert-level: uint
  }
)
(define-data-var snapshot-nonce uint u0)

;; Cumulative stats
(define-map reporter-stats principal
  {
    events-submitted: uint,
    last-submission: uint
  }
)

;; Read-only functions

(define-read-only (get-event (event-id uint))
  (map-get? bridge-events event-id)
)

(define-read-only (get-alert (alert-id uint))
  (map-get? alerts alert-id)
)

(define-read-only (get-bridge-stats)
  {
    total-events: (var-get event-nonce),
    total-peg-in-volume: (var-get total-peg-in-volume),
    total-peg-out-volume: (var-get total-peg-out-volume),
    total-failed: (var-get total-failed-events),
    bridge-active: (var-get bridge-active),
    current-alert-level: (var-get current-alert-level),
    last-health-check: (var-get last-health-check)
  }
)

(define-read-only (get-current-alert-level)
  (var-get current-alert-level)
)

(define-read-only (is-bridge-healthy)
  (and
    (var-get bridge-active)
    (<= (var-get current-alert-level) ALERT-LOW)
  )
)

(define-read-only (is-authorized-reporter (reporter principal))
  (default-to false (map-get? authorized-reporters reporter))
)

(define-read-only (get-reporter-stats (reporter principal))
  (default-to
    { events-submitted: u0, last-submission: u0 }
    (map-get? reporter-stats reporter)
  )
)

(define-read-only (get-latest-snapshot)
  (let ((nonce (var-get snapshot-nonce)))
    (if (> nonce u0)
      (map-get? health-snapshots (- nonce u1))
      none
    )
  )
)

;; Public functions

;; Add authorized reporter (owner only)
(define-public (add-reporter (reporter principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-reporters reporter true)
    (ok { reporter: reporter, authorized: true })
  )
)

;; Remove reporter
(define-public (remove-reporter (reporter principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-reporters reporter false)
    (ok { reporter: reporter, authorized: false })
  )
)

;; Submit bridge event (authorized reporters only)
(define-public (submit-bridge-event
    (event-type uint)
    (btc-txid (string-ascii 64))
    (amount-sats uint)
    (amount-micro-sbtc uint)
    (from-address (string-ascii 62))
    (to-principal (optional principal))
    (notes (optional (string-ascii 200))))
  (let (
    (event-id (var-get event-nonce))
  )
    (asserts! (is-authorized-reporter tx-sender) err-not-authorized)
    (asserts! (> amount-sats u0) err-invalid-amount)
    (asserts! (or (is-eq event-type EVENT-PEG-IN) (is-eq event-type EVENT-PEG-OUT)) err-invalid-status)

    (map-set bridge-events event-id {
      event-type: event-type,
      submitter: tx-sender,
      btc-txid: btc-txid,
      stacks-txid: none,
      amount-sats: amount-sats,
      amount-micro-sbtc: amount-micro-sbtc,
      from-address: from-address,
      to-principal: to-principal,
      status: EVENT-PENDING,
      submitted-at: stacks-block-height,
      confirmed-at: none,
      confirmation-blocks: u0,
      notes: notes
    })

    ;; Update volume stats
    (if (is-eq event-type EVENT-PEG-IN)
      (var-set total-peg-in-volume (+ (var-get total-peg-in-volume) amount-sats))
      (var-set total-peg-out-volume (+ (var-get total-peg-out-volume) amount-sats))
    )

    ;; Update reporter stats
    (let ((stats (get-reporter-stats tx-sender)))
      (map-set reporter-stats tx-sender {
        events-submitted: (+ (get events-submitted stats) u1),
        last-submission: stacks-block-height
      })
    )

    (var-set event-nonce (+ event-id u1))

    (ok { event-id: event-id, status: EVENT-PENDING })
  )
)

;; Confirm bridge event
(define-public (confirm-event
    (event-id uint)
    (stacks-txid (string-ascii 64))
    (confirmation-blocks uint))
  (match (map-get? bridge-events event-id)
    event
    (begin
      (asserts! (is-authorized-reporter tx-sender) err-not-authorized)
      (asserts! (is-eq (get status event) EVENT-PENDING) err-invalid-status)

      (map-set bridge-events event-id (merge event {
        stacks-txid: (some stacks-txid),
        status: EVENT-CONFIRMED,
        confirmed-at: (some stacks-block-height),
        confirmation-blocks: confirmation-blocks
      }))

      (ok { event-id: event-id, status: EVENT-CONFIRMED, confirmation-blocks: confirmation-blocks })
    )
    err-not-found
  )
)

;; Mark event as failed
(define-public (mark-event-failed (event-id uint) (reason (string-ascii 200)))
  (match (map-get? bridge-events event-id)
    event
    (begin
      (asserts! (is-authorized-reporter tx-sender) err-not-authorized)
      (asserts! (is-eq (get status event) EVENT-PENDING) err-invalid-status)

      (map-set bridge-events event-id (merge event {
        status: EVENT-FAILED,
        notes: (some reason)
      }))

      (var-set total-failed-events (+ (var-get total-failed-events) u1))

      (ok { event-id: event-id, status: EVENT-FAILED })
    )
    err-not-found
  )
)

;; Raise alert
(define-public (raise-alert (level uint) (message (string-ascii 200)))
  (let ((alert-id (var-get alert-nonce)))
    (asserts! (is-authorized-reporter tx-sender) err-not-authorized)
    (asserts! (and (>= level ALERT-LOW) (<= level ALERT-CRITICAL)) err-invalid-status)

    (map-set alerts alert-id {
      level: level,
      message: message,
      raised-at: stacks-block-height,
      raised-by: tx-sender,
      resolved: false,
      resolved-at: none
    })

    (var-set alert-nonce (+ alert-id u1))

    ;; Update current alert level if higher
    (if (> level (var-get current-alert-level))
      (var-set current-alert-level level)
      false
    )

    (ok { alert-id: alert-id, level: level })
  )
)

;; Resolve alert (owner only)
(define-public (resolve-alert (alert-id uint))
  (match (map-get? alerts alert-id)
    alert
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)

      (map-set alerts alert-id (merge alert {
        resolved: true,
        resolved-at: (some stacks-block-height)
      }))

      ;; Reset alert level
      (var-set current-alert-level ALERT-NONE)

      (ok { alert-id: alert-id, resolved: true })
    )
    err-not-found
  )
)

;; Record health snapshot
(define-public (record-health-snapshot
    (peg-in-count uint)
    (peg-out-count uint)
    (failed-count uint)
    (avg-confirmation-blocks uint)
    (total-volume-sats uint))
  (let ((snapshot-id (var-get snapshot-nonce)))
    (asserts! (is-authorized-reporter tx-sender) err-not-authorized)

    (map-set health-snapshots snapshot-id {
      block-height: stacks-block-height,
      peg-in-count: peg-in-count,
      peg-out-count: peg-out-count,
      failed-count: failed-count,
      avg-confirmation-blocks: avg-confirmation-blocks,
      total-volume-sats: total-volume-sats,
      alert-level: (var-get current-alert-level)
    })

    (var-set snapshot-nonce (+ snapshot-id u1))
    (var-set last-health-check stacks-block-height)

    (ok { snapshot-id: snapshot-id, block-height: stacks-block-height })
  )
)

;; Toggle bridge active status (owner only)
(define-public (set-bridge-active (active bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set bridge-active active)
    (if (not active)
      (var-set current-alert-level ALERT-CRITICAL)
      (var-set current-alert-level ALERT-NONE)
    )
    (ok { bridge-active: active })
  )
)
