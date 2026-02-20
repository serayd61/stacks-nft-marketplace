;; SIP-010 Fungible Token Contract
;; Standard fungible token with minting, burning, and transfer
;; Implements SIP-010 interface

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-mint-limit (err u104))
(define-constant err-burn-failed (err u105))
(define-constant err-transfer-failed (err u106))
(define-constant err-not-authorized (err u107))

;; Token settings
(define-constant token-name "Stacks Token")
(define-constant token-symbol "STKN")
(define-constant token-decimals u6)
(define-constant max-supply u1000000000000000) ;; 1 billion tokens

;; Define the fungible token
(define-fungible-token stacks-token max-supply)

;; Data Variables
(define-data-var total-supply uint u0)
(define-data-var mint-enabled bool true)
(define-data-var transfer-enabled bool true)

;; Authorized minters
(define-map authorized-minters principal bool)

;; Token metadata URI
(define-data-var token-uri (optional (string-utf8 256)) (some u"https://api.stackstoken.io/metadata"))

;; SIP-010 Functions

(define-read-only (get-name)
  (ok token-name)
)

(define-read-only (get-symbol)
  (ok token-symbol)
)

(define-read-only (get-decimals)
  (ok token-decimals)
)

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance stacks-token account))
)

(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

(define-read-only (get-token-uri)
  (ok (var-get token-uri))
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (var-get transfer-enabled) err-transfer-failed)
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount (ft-get-balance stacks-token sender)) err-insufficient-balance)
    (try! (ft-transfer? stacks-token amount sender recipient))
    (match memo to-print (print to-print) 0x)
    (ok true)
  )
)

;; Minting Functions

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (var-get mint-enabled) err-mint-limit)
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-minter tx-sender)) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= (+ (var-get total-supply) amount) max-supply) err-mint-limit)
    (try! (ft-mint? stacks-token amount recipient))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok amount)
  )
)

(define-public (mint-fixed (recipient principal))
  (let (
    (amount u1000000000) ;; 1000 tokens
  )
    (asserts! (var-get mint-enabled) err-mint-limit)
    (asserts! (<= (+ (var-get total-supply) amount) max-supply) err-mint-limit)
    (try! (ft-mint? stacks-token amount recipient))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok amount)
  )
)

;; Burning Functions

(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount (ft-get-balance stacks-token tx-sender)) err-insufficient-balance)
    (try! (ft-burn? stacks-token amount tx-sender))
    (var-set total-supply (- (var-get total-supply) amount))
    (ok amount)
  )
)

(define-public (burn-from (amount uint) (owner principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount (ft-get-balance stacks-token owner)) err-insufficient-balance)
    (try! (ft-burn? stacks-token amount owner))
    (var-set total-supply (- (var-get total-supply) amount))
    (ok amount)
  )
)

;; Admin Functions

(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set token-uri new-uri)
    (ok true)
  )
)

(define-public (toggle-minting)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set mint-enabled (not (var-get mint-enabled)))
    (ok (var-get mint-enabled))
  )
)

(define-public (toggle-transfers)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set transfer-enabled (not (var-get transfer-enabled)))
    (ok (var-get transfer-enabled))
  )
)

(define-public (add-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-minters minter true)
    (ok true)
  )
)

(define-public (remove-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete authorized-minters minter)
    (ok true)
  )
)

;; Read-only Functions

(define-read-only (is-authorized-minter (account principal))
  (default-to false (map-get? authorized-minters account))
)

(define-read-only (get-token-info)
  {
    name: token-name,
    symbol: token-symbol,
    decimals: token-decimals,
    total-supply: (var-get total-supply),
    max-supply: max-supply,
    mint-enabled: (var-get mint-enabled),
    transfer-enabled: (var-get transfer-enabled)
  }
)

(define-read-only (get-max-supply)
  (ok max-supply)
)

(define-read-only (is-mint-enabled)
  (ok (var-get mint-enabled))
)

(define-read-only (is-transfer-enabled)
  (ok (var-get transfer-enabled))
)
