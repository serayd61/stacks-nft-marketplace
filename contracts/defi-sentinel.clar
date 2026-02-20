;; Stacks DeFi Sentinel Contract
;; On-chain monitoring, risk scoring, and alerts for Stacks DeFi protocols
;; Tracks TVL changes, price deviations, and liquidity anomalies

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u600))
(define-constant err-not-found (err u601))
(define-constant err-not-authorized (err u602))
(define-constant err-invalid-value (err u603))
(define-constant err-protocol-inactive (err u604))
(define-constant err-already-registered (err u605))
(define-constant err-threshold-breach (err u606))

;; Risk levels
(define-constant RISK-LOW u1)
(define-constant RISK-MEDIUM u2)
(define-constant RISK-HIGH u3)
(define-constant RISK-CRITICAL u4)

;; Protocol types
(define-constant PROTO-DEX u0)
(define-constant PROTO-LENDING u1)
(define-constant PROTO-YIELD u2)
(define-constant PROTO-BRIDGE u3)
(define-constant PROTO-STAKING u4)

;; Data Variables
(define-data-var sentinel-active bool true)
(define-data-var protocol-count uint u0)
(define-data-var incident-count uint u0)
(define-data-var snapshot-count uint u0)
(define-data-var global-risk-score uint RISK-LOW)
(define-data-var last-update uint u0)

;; Authorized oracles/watchers
(define-map authorized-watchers principal bool)

;; Registered DeFi protocols
(define-map protocols principal
  {
    name: (string-ascii 50),
    protocol-type: uint,
    registered-at: uint,
    active: bool,
    risk-score: uint,
    tvl-micro-stx: uint,
    last-tvl-update: uint,
    incident-count: uint,
    watcher: principal
  }
)

;; TVL snapshots for each protocol
(define-map tvl-snapshots
  { protocol: principal, snapshot-id: uint }
  {
    tvl-micro-stx: uint,
    block-height: uint,
    recorded-by: principal,
    price-stx-usd: uint,         ;; price * 1000000 for precision
    tvl-usd-estimate: uint
  }
)

;; Per-protocol snapshot counter
(define-map protocol-snapshot-count principal uint)

;; Incident registry
(define-map incidents uint
  {
    protocol: principal,
    severity: uint,
    incident-type: (string-ascii 50),
    description: (string-ascii 200),
    detected-at: uint,
    detected-by: principal,
    tvl-before: uint,
    tvl-after: uint,
    resolved: bool,
    resolved-at: (optional uint),
    post-mortem: (optional (string-ascii 200))
  }
)

;; Risk thresholds per protocol
(define-map risk-thresholds principal
  {
    max-tvl-drop-bps: uint,       ;; Max acceptable TVL drop in basis points
    max-price-deviation-bps: uint, ;; Max price deviation
    min-liquidity-micro-stx: uint, ;; Minimum liquidity threshold
    alert-cooldown-blocks: uint    ;; Blocks between repeated alerts
  }
)

;; Last alert per protocol (for cooldown)
(define-map last-alert-block principal uint)

;; Read-only functions

(define-read-only (get-protocol (protocol principal))
  (map-get? protocols protocol)
)

(define-read-only (get-incident (incident-id uint))
  (map-get? incidents incident-id)
)

(define-read-only (get-tvl-snapshot (protocol principal) (snapshot-id uint))
  (map-get? tvl-snapshots { protocol: protocol, snapshot-id: snapshot-id })
)

(define-read-only (get-risk-thresholds (protocol principal))
  (map-get? risk-thresholds protocol)
)

(define-read-only (get-global-risk-score)
  (var-get global-risk-score)
)

(define-read-only (get-sentinel-stats)
  {
    active: (var-get sentinel-active),
    protocol-count: (var-get protocol-count),
    incident-count: (var-get incident-count),
    global-risk: (var-get global-risk-score),
    last-update: (var-get last-update)
  }
)

(define-read-only (is-authorized-watcher (watcher principal))
  (default-to false (map-get? authorized-watchers watcher))
)

(define-read-only (calculate-tvl-change-bps (old-tvl uint) (new-tvl uint))
  (if (> old-tvl u0)
    (if (>= new-tvl old-tvl)
      (ok (/ (* (- new-tvl old-tvl) u10000) old-tvl))      ;; increase
      (ok (/ (* (- old-tvl new-tvl) u10000) old-tvl))      ;; decrease (positive value)
    )
    (ok u0)
  )
)

(define-read-only (get-protocol-snapshot-count (protocol principal))
  (default-to u0 (map-get? protocol-snapshot-count protocol))
)

(define-read-only (is-in-alert-cooldown (protocol principal) (cooldown-blocks uint))
  (match (map-get? last-alert-block protocol)
    last-block (< (- stacks-block-height last-block) cooldown-blocks)
    false
  )
)

;; Public functions

;; Add authorized watcher
(define-public (add-watcher (watcher principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-watchers watcher true)
    (ok { watcher: watcher, authorized: true })
  )
)

;; Register a DeFi protocol for monitoring
(define-public (register-protocol
    (protocol principal)
    (name (string-ascii 50))
    (protocol-type uint)
    (initial-tvl uint)
    (max-tvl-drop-bps uint)
    (min-liquidity uint))
  (begin
    (asserts! (is-authorized-watcher tx-sender) err-not-authorized)
    (asserts! (is-none (map-get? protocols protocol)) err-already-registered)
    (asserts! (<= protocol-type PROTO-STAKING) err-invalid-value)

    (map-set protocols protocol {
      name: name,
      protocol-type: protocol-type,
      registered-at: stacks-block-height,
      active: true,
      risk-score: RISK-LOW,
      tvl-micro-stx: initial-tvl,
      last-tvl-update: stacks-block-height,
      incident-count: u0,
      watcher: tx-sender
    })

    ;; Set default thresholds
    (map-set risk-thresholds protocol {
      max-tvl-drop-bps: max-tvl-drop-bps,
      max-price-deviation-bps: u2000,   ;; 20% default
      min-liquidity-micro-stx: min-liquidity,
      alert-cooldown-blocks: u144        ;; ~1 day default
    })

    (var-set protocol-count (+ (var-get protocol-count) u1))

    (ok { protocol: protocol, name: name, initial-tvl: initial-tvl })
  )
)

;; Record TVL snapshot and detect anomalies
(define-public (record-tvl-snapshot
    (protocol principal)
    (new-tvl uint)
    (price-stx-usd uint))
  (match (map-get? protocols protocol)
    proto
    (let (
      (old-tvl (get tvl-micro-stx proto))
      (snapshot-id (get-protocol-snapshot-count protocol))
      (tvl-usd-estimate (/ (* new-tvl price-stx-usd) u1000000))
    )
      (asserts! (is-authorized-watcher tx-sender) err-not-authorized)
      (asserts! (get active proto) err-protocol-inactive)

      ;; Record snapshot
      (map-set tvl-snapshots
        { protocol: protocol, snapshot-id: snapshot-id }
        {
          tvl-micro-stx: new-tvl,
          block-height: stacks-block-height,
          recorded-by: tx-sender,
          price-stx-usd: price-stx-usd,
          tvl-usd-estimate: tvl-usd-estimate
        }
      )

      ;; Update protocol TVL
      (map-set protocols protocol (merge proto {
        tvl-micro-stx: new-tvl,
        last-tvl-update: stacks-block-height
      }))

      (map-set protocol-snapshot-count protocol (+ snapshot-id u1))
      (var-set snapshot-count (+ (var-get snapshot-count) u1))
      (var-set last-update stacks-block-height)

      ;; Check for TVL anomaly
      (match (map-get? risk-thresholds protocol)
        thresholds
        (let (
          (drop-bps (if (> old-tvl new-tvl)
            (/ (* (- old-tvl new-tvl) u10000) (if (> old-tvl u0) old-tvl u1))
            u0))
        )
          (if (> drop-bps (get max-tvl-drop-bps thresholds))
            (ok { snapshot-id: snapshot-id, tvl: new-tvl, anomaly-detected: true, drop-bps: drop-bps })
            (ok { snapshot-id: snapshot-id, tvl: new-tvl, anomaly-detected: false, drop-bps: drop-bps })
          )
        )
        (ok { snapshot-id: snapshot-id, tvl: new-tvl, anomaly-detected: false, drop-bps: u0 })
      )
    )
    err-not-found
  )
)

;; Report incident
(define-public (report-incident
    (protocol principal)
    (severity uint)
    (incident-type (string-ascii 50))
    (description (string-ascii 200))
    (tvl-before uint)
    (tvl-after uint))
  (let (
    (incident-id (var-get incident-count))
  )
    (asserts! (is-authorized-watcher tx-sender) err-not-authorized)
    (asserts! (is-some (map-get? protocols protocol)) err-not-found)
    (asserts! (and (>= severity RISK-LOW) (<= severity RISK-CRITICAL)) err-invalid-value)

    (map-set incidents incident-id {
      protocol: protocol,
      severity: severity,
      incident-type: incident-type,
      description: description,
      detected-at: stacks-block-height,
      detected-by: tx-sender,
      tvl-before: tvl-before,
      tvl-after: tvl-after,
      resolved: false,
      resolved-at: none,
      post-mortem: none
    })

    ;; Update protocol risk score
    (match (map-get? protocols protocol)
      proto
      (map-set protocols protocol (merge proto {
        risk-score: severity,
        incident-count: (+ (get incident-count proto) u1)
      }))
      false
    )

    ;; Update global risk score if higher
    (if (> severity (var-get global-risk-score))
      (var-set global-risk-score severity)
      false
    )

    (map-set last-alert-block protocol stacks-block-height)
    (var-set incident-count (+ incident-id u1))

    (ok { incident-id: incident-id, severity: severity, protocol: protocol })
  )
)

;; Resolve incident with post-mortem
(define-public (resolve-incident
    (incident-id uint)
    (post-mortem (optional (string-ascii 200))))
  (match (map-get? incidents incident-id)
    incident
    (begin
      (asserts! (is-authorized-watcher tx-sender) err-not-authorized)
      (asserts! (not (get resolved incident)) err-invalid-value)

      (map-set incidents incident-id (merge incident {
        resolved: true,
        resolved-at: (some stacks-block-height),
        post-mortem: post-mortem
      }))

      ;; Recalculate global risk
      (var-set global-risk-score RISK-LOW)

      (ok { incident-id: incident-id, resolved: true })
    )
    err-not-found
  )
)

;; Update risk thresholds
(define-public (update-thresholds
    (protocol principal)
    (max-tvl-drop-bps uint)
    (max-price-deviation-bps uint)
    (min-liquidity uint)
    (cooldown-blocks uint))
  (begin
    (asserts! (is-authorized-watcher tx-sender) err-not-authorized)
    (asserts! (is-some (map-get? protocols protocol)) err-not-found)

    (map-set risk-thresholds protocol {
      max-tvl-drop-bps: max-tvl-drop-bps,
      max-price-deviation-bps: max-price-deviation-bps,
      min-liquidity-micro-stx: min-liquidity,
      alert-cooldown-blocks: cooldown-blocks
    })

    (ok { protocol: protocol, thresholds-updated: true })
  )
)

;; Deactivate protocol monitoring
(define-public (deactivate-protocol (protocol principal))
  (match (map-get? protocols protocol)
    proto
    (begin
      (asserts! (or
        (is-eq tx-sender contract-owner)
        (is-eq tx-sender (get watcher proto))) err-not-authorized)

      (map-set protocols protocol (merge proto { active: false }))
      (ok { protocol: protocol, active: false })
    )
    err-not-found
  )
)

;; Toggle sentinel active
(define-public (set-sentinel-active (active bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set sentinel-active active)
    (ok { sentinel-active: active })
  )
)
