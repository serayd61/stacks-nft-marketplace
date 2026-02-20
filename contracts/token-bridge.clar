;; Token Bridge Contract
;; Bridge tokens between Stacks and other chains (with oracle verification)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-oracle (err u101))
(define-constant err-already-processed (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-bridge-paused (err u104))
(define-constant err-invalid-destination (err u105))
(define-constant err-insufficient-liquidity (err u106))
(define-constant err-invalid-proof (err u107))
(define-constant err-request-not-found (err u108))
(define-constant err-min-amount (err u109))

;; Bridge settings
(define-constant min-bridge-amount u1000000) ;; 1 STX minimum
(define-constant bridge-fee u100) ;; 1% fee
(define-constant fee-denominator u10000)

;; Supported chains
(define-constant chain-ethereum u1)
(define-constant chain-bitcoin u2)
(define-constant chain-polygon u3)
(define-constant chain-arbitrum u4)

;; Data Variables
(define-data-var bridge-nonce uint u0)
(define-data-var bridge-paused bool false)
(define-data-var total-bridged-out uint u0)
(define-data-var total-bridged-in uint u0)
(define-data-var liquidity-pool uint u0)

;; Bridge requests (outgoing)
(define-map bridge-requests uint
  {
    sender: principal,
    destination-chain: uint,
    destination-address: (buff 64),
    amount: uint,
    fee: uint,
    created-at: uint,
    status: (string-ascii 20),
    tx-hash: (optional (buff 32))
  }
)

;; Processed incoming transactions (to prevent replay)
(define-map processed-txs (buff 32) bool)

;; Oracle addresses
(define-map oracles principal bool)

;; Chain configurations
(define-map chain-configs uint
  {
    name: (string-ascii 32),
    enabled: bool,
    min-confirmations: uint,
    bridge-contract: (buff 64)
  }
)

;; User bridge history
(define-map user-bridge-stats principal
  {
    total-out: uint,
    total-in: uint,
    request-count: uint
  }
)

;; Initialize chain configs
(map-set chain-configs chain-ethereum { name: "Ethereum", enabled: true, min-confirmations: u12, bridge-contract: 0x })
(map-set chain-configs chain-bitcoin { name: "Bitcoin", enabled: true, min-confirmations: u6, bridge-contract: 0x })
(map-set chain-configs chain-polygon { name: "Polygon", enabled: true, min-confirmations: u128, bridge-contract: 0x })
(map-set chain-configs chain-arbitrum { name: "Arbitrum", enabled: true, min-confirmations: u64, bridge-contract: 0x })

;; Public Functions

(define-public (bridge-out 
  (amount uint) 
  (destination-chain uint) 
  (destination-address (buff 64)))
  (let (
    (request-id (var-get bridge-nonce))
    (fee (calculate-bridge-fee amount))
    (net-amount (- amount fee))
  )
    (asserts! (not (var-get bridge-paused)) err-bridge-paused)
    (asserts! (>= amount min-bridge-amount) err-min-amount)
    (asserts! (is-chain-enabled destination-chain) err-invalid-destination)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Create bridge request
    (map-set bridge-requests request-id
      {
        sender: tx-sender,
        destination-chain: destination-chain,
        destination-address: destination-address,
        amount: net-amount,
        fee: fee,
        created-at: stacks-block-height,
        status: "pending",
        tx-hash: none
      }
    )
    
    ;; Update stats
    (var-set bridge-nonce (+ request-id u1))
    (var-set total-bridged-out (+ (var-get total-bridged-out) net-amount))
    
    (map-set user-bridge-stats tx-sender
      (merge (get-user-stats tx-sender)
        {
          total-out: (+ (get total-out (get-user-stats tx-sender)) net-amount),
          request-count: (+ (get request-count (get-user-stats tx-sender)) u1)
        }
      )
    )
    
    (ok { request-id: request-id, amount: net-amount, fee: fee })
  )
)

(define-public (complete-bridge-out (request-id uint) (tx-hash (buff 32)))
  (match (map-get? bridge-requests request-id)
    request
    (begin
      (asserts! (is-oracle tx-sender) err-not-oracle)
      (asserts! (is-eq (get status request) "pending") err-already-processed)
      
      ;; Update request status
      (map-set bridge-requests request-id
        (merge request { 
          status: "completed",
          tx-hash: (some tx-hash)
        })
      )
      
      (ok { request-id: request-id, status: "completed" })
    )
    err-request-not-found
  )
)

(define-public (bridge-in 
  (recipient principal)
  (amount uint)
  (source-chain uint)
  (source-tx-hash (buff 32)))
  (begin
    (asserts! (is-oracle tx-sender) err-not-oracle)
    (asserts! (not (var-get bridge-paused)) err-bridge-paused)
    (asserts! (not (is-tx-processed source-tx-hash)) err-already-processed)
    (asserts! (>= (var-get liquidity-pool) amount) err-insufficient-liquidity)
    
    ;; Mark transaction as processed
    (map-set processed-txs source-tx-hash true)
    
    ;; Transfer tokens to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update stats
    (var-set total-bridged-in (+ (var-get total-bridged-in) amount))
    (var-set liquidity-pool (- (var-get liquidity-pool) amount))
    
    (map-set user-bridge-stats recipient
      (merge (get-user-stats recipient)
        { total-in: (+ (get total-in (get-user-stats recipient)) amount) }
      )
    )
    
    (ok { recipient: recipient, amount: amount, source-chain: source-chain })
  )
)

(define-public (add-liquidity (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set liquidity-pool (+ (var-get liquidity-pool) amount))
    (ok { new-liquidity: (var-get liquidity-pool) })
  )
)

(define-public (cancel-bridge-request (request-id uint))
  (match (map-get? bridge-requests request-id)
    request
    (begin
      (asserts! (is-eq (get sender request) tx-sender) err-owner-only)
      (asserts! (is-eq (get status request) "pending") err-already-processed)
      
      ;; Refund tokens (minus a small cancellation fee)
      (let (
        (refund-amount (+ (get amount request) (/ (get fee request) u2)))
      )
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get sender request))))
      )
      
      ;; Update status
      (map-set bridge-requests request-id
        (merge request { status: "cancelled" })
      )
      
      (ok { request-id: request-id, status: "cancelled" })
    )
    err-request-not-found
  )
)

;; Read-only Functions

(define-read-only (get-bridge-request (request-id uint))
  (map-get? bridge-requests request-id)
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { total-out: u0, total-in: u0, request-count: u0 }
    (map-get? user-bridge-stats user)
  )
)

(define-read-only (calculate-bridge-fee (amount uint))
  (/ (* amount bridge-fee) fee-denominator)
)

(define-read-only (is-tx-processed (tx-hash (buff 32)))
  (default-to false (map-get? processed-txs tx-hash))
)

(define-read-only (is-oracle (account principal))
  (default-to false (map-get? oracles account))
)

(define-read-only (is-chain-enabled (target-chain uint))
  (match (map-get? chain-configs target-chain)
    config (get enabled config)
    false
  )
)

(define-read-only (get-chain-config (target-chain uint))
  (map-get? chain-configs target-chain)
)

(define-read-only (get-bridge-stats)
  {
    total-requests: (var-get bridge-nonce),
    total-bridged-out: (var-get total-bridged-out),
    total-bridged-in: (var-get total-bridged-in),
    liquidity-pool: (var-get liquidity-pool),
    bridge-paused: (var-get bridge-paused),
    bridge-fee: bridge-fee,
    min-amount: min-bridge-amount
  }
)

(define-read-only (estimate-bridge-out (amount uint))
  (let (
    (fee (calculate-bridge-fee amount))
  )
    {
      gross-amount: amount,
      fee: fee,
      net-amount: (- amount fee)
    }
  )
)

;; Admin Functions

(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set oracles oracle true)
    (ok true)
  )
)

(define-public (remove-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete oracles oracle)
    (ok true)
  )
)

(define-public (toggle-bridge-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set bridge-paused (not (var-get bridge-paused)))
    (ok (var-get bridge-paused))
  )
)

(define-public (update-chain-config 
  (target-chain uint) 
  (name (string-ascii 32))
  (enabled bool)
  (min-confirmations uint)
  (bridge-contract (buff 64)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set chain-configs target-chain {
      name: name,
      enabled: enabled,
      min-confirmations: min-confirmations,
      bridge-contract: bridge-contract
    })
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set liquidity-pool (- (var-get liquidity-pool) amount))
    (ok true)
  )
)
