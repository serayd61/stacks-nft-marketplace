;; NFT Collection Contract
;; SIP-009 compliant NFT with minting, burning, and metadata
;; Implements SIP-009 interface

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-not-found (err u102))
(define-constant err-mint-limit-reached (err u103))
(define-constant err-already-minted (err u104))
(define-constant err-invalid-uri (err u105))
(define-constant err-transfer-failed (err u106))
(define-constant err-not-approved (err u107))

;; Collection settings
(define-constant collection-name "Stacks Collection")
(define-constant collection-symbol "STKC")
(define-constant max-supply u10000)
(define-constant mint-price u50000000) ;; 50 STX

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var base-uri (string-ascii 256) "https://api.stackscollection.io/metadata/")
(define-data-var mint-paused bool false)
(define-data-var total-minted uint u0)

;; NFT Definition
(define-non-fungible-token stacks-nft uint)

;; Token metadata
(define-map token-uris uint (string-ascii 256))
(define-map token-owners uint principal)
(define-map owner-balances principal uint)

;; Approval maps
(define-map token-approvals uint principal)
(define-map operator-approvals { owner: principal, operator: principal } bool)

;; Royalty settings
(define-map royalty-info uint { creator: principal, percentage: uint })
(define-constant royalty-denominator u10000)

;; SIP-009 Functions

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
  (ok (map-get? token-uris token-id))
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? stacks-nft token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (asserts! (is-some (nft-get-owner? stacks-nft token-id)) err-token-not-found)
    (try! (nft-transfer? stacks-nft token-id sender recipient))
    (map-set token-owners token-id recipient)
    (map-set owner-balances sender (- (default-to u0 (map-get? owner-balances sender)) u1))
    (map-set owner-balances recipient (+ (default-to u0 (map-get? owner-balances recipient)) u1))
    (ok true)
  )
)

;; Minting Functions

(define-public (mint (recipient principal))
  (let (
    (token-id (+ (var-get last-token-id) u1))
  )
    (asserts! (not (var-get mint-paused)) err-owner-only)
    (asserts! (<= token-id max-supply) err-mint-limit-reached)
    ;; Only charge if not contract owner
    (if (not (is-eq tx-sender contract-owner))
      (try! (stx-transfer? mint-price tx-sender contract-owner))
      true
    )
    (try! (nft-mint? stacks-nft token-id recipient))
    (var-set last-token-id token-id)
    (var-set total-minted (+ (var-get total-minted) u1))
    (map-set token-owners token-id recipient)
    (map-set owner-balances recipient (+ (default-to u0 (map-get? owner-balances recipient)) u1))
    (map-set token-uris token-id (concat (var-get base-uri) ""))
    (map-set royalty-info token-id { creator: tx-sender, percentage: u500 }) ;; 5% royalty
    (ok token-id)
  )
)

(define-public (mint-batch (recipient principal) (count uint))
  (let (
    (start-id (var-get last-token-id))
  )
    (asserts! (not (var-get mint-paused)) err-owner-only)
    (asserts! (<= (+ start-id count) max-supply) err-mint-limit-reached)
    (try! (stx-transfer? (* mint-price count) tx-sender contract-owner))
    (fold mint-single (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) { recipient: recipient, minted: u0, max: count })
    (ok (var-get last-token-id))
  )
)

(define-private (mint-single (n uint) (state { recipient: principal, minted: uint, max: uint }))
  (if (< (get minted state) (get max state))
    (let (
      (token-id (+ (var-get last-token-id) u1))
    )
      (match (nft-mint? stacks-nft token-id (get recipient state))
        success
        (begin
          (var-set last-token-id token-id)
          (var-set total-minted (+ (var-get total-minted) u1))
          (map-set token-owners token-id (get recipient state))
          (map-set owner-balances (get recipient state) 
            (+ (default-to u0 (map-get? owner-balances (get recipient state))) u1))
          { recipient: (get recipient state), minted: (+ (get minted state) u1), max: (get max state) }
        )
        error state
      )
    )
    state
  )
)

;; Burning

(define-public (burn (token-id uint))
  (let (
    (owner (unwrap! (nft-get-owner? stacks-nft token-id) err-token-not-found))
  )
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (try! (nft-burn? stacks-nft token-id owner))
    (map-delete token-owners token-id)
    (map-delete token-uris token-id)
    (map-set owner-balances owner (- (default-to u0 (map-get? owner-balances owner)) u1))
    (ok true)
  )
)

;; Approval Functions

(define-public (set-approved (token-id uint) (approved principal) (is-approved bool))
  (let (
    (owner (unwrap! (nft-get-owner? stacks-nft token-id) err-token-not-found))
  )
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (if is-approved
      (map-set token-approvals token-id approved)
      (map-delete token-approvals token-id)
    )
    (ok true)
  )
)

(define-public (set-approval-for-all (operator principal) (approved bool))
  (begin
    (map-set operator-approvals { owner: tx-sender, operator: operator } approved)
    (ok true)
  )
)

(define-read-only (get-approved (token-id uint))
  (ok (map-get? token-approvals token-id))
)

(define-read-only (is-approved-for-all (owner principal) (operator principal))
  (ok (default-to false (map-get? operator-approvals { owner: owner, operator: operator })))
)

;; Read-only Functions

(define-read-only (get-balance (owner principal))
  (ok (default-to u0 (map-get? owner-balances owner)))
)

(define-read-only (get-collection-info)
  {
    name: collection-name,
    symbol: collection-symbol,
    max-supply: max-supply,
    total-minted: (var-get total-minted),
    mint-price: mint-price,
    mint-paused: (var-get mint-paused),
    base-uri: (var-get base-uri)
  }
)

(define-read-only (get-royalty-info (token-id uint))
  (map-get? royalty-info token-id)
)

(define-read-only (calculate-royalty (token-id uint) (sale-price uint))
  (match (map-get? royalty-info token-id)
    info (/ (* sale-price (get percentage info)) royalty-denominator)
    u0
  )
)

;; Admin Functions

(define-public (set-base-uri (new-uri (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set base-uri new-uri)
    (ok true)
  )
)

(define-public (set-token-uri (token-id uint) (uri (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set token-uris token-id uri)
    (ok true)
  )
)

(define-public (toggle-mint-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set mint-paused (not (var-get mint-paused)))
    (ok (var-get mint-paused))
  )
)

(define-public (withdraw-stx (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)
  )
)
