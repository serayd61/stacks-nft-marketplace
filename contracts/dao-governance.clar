;; DAO Governance Contract
;; Decentralized governance with proposal creation and voting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-proposal-not-found (err u102))
(define-constant err-voting-ended (err u103))
(define-constant err-voting-active (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-insufficient-tokens (err u106))
(define-constant err-quorum-not-met (err u107))
(define-constant err-proposal-not-passed (err u108))
(define-constant err-already-executed (err u109))
(define-constant err-execution-delay (err u110))

;; Governance parameters
(define-constant min-proposal-threshold u1000000000) ;; 1000 tokens to create proposal
(define-constant voting-period u10080) ;; ~7 days in blocks
(define-constant execution-delay u1440) ;; ~1 day delay after voting ends
(define-constant quorum-percentage u400) ;; 4% quorum required

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var total-voting-power uint u0)
(define-data-var governance-token principal tx-sender)

;; Proposals
(define-map proposals uint
  {
    proposer: principal,
    title: (string-ascii 128),
    description: (string-utf8 1024),
    proposal-type: (string-ascii 32),
    target-contract: (optional principal),
    call-data: (optional (buff 256)),
    start-block: uint,
    end-block: uint,
    for-votes: uint,
    against-votes: uint,
    abstain-votes: uint,
    executed: bool,
    cancelled: bool
  }
)

;; Votes tracking
(define-map votes { proposal-id: uint, voter: principal }
  {
    vote-type: (string-ascii 10),
    weight: uint,
    voted-at: uint
  }
)

;; Member voting power (delegated or direct)
(define-map voting-power principal uint)

;; Delegation
(define-map delegations principal principal)

;; Member stats
(define-map member-stats principal
  {
    proposals-created: uint,
    votes-cast: uint,
    proposals-executed: uint
  }
)

;; Public Functions

(define-public (create-proposal 
  (title (string-ascii 128))
  (description (string-utf8 1024))
  (proposal-type (string-ascii 32))
  (target-contract (optional principal))
  (call-data (optional (buff 256))))
  (let (
    (proposal-id (var-get proposal-nonce))
    (proposer-power (get-voting-power tx-sender))
  )
    (asserts! (>= proposer-power min-proposal-threshold) err-insufficient-tokens)
    
    ;; Create proposal
    (map-set proposals proposal-id
      {
        proposer: tx-sender,
        title: title,
        description: description,
        proposal-type: proposal-type,
        target-contract: target-contract,
        call-data: call-data,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height voting-period),
        for-votes: u0,
        against-votes: u0,
        abstain-votes: u0,
        executed: false,
        cancelled: false
      }
    )
    
    ;; Update stats
    (var-set proposal-nonce (+ proposal-id u1))
    
    (map-set member-stats tx-sender
      (merge (get-member-stats tx-sender)
        { proposals-created: (+ (get proposals-created (get-member-stats tx-sender)) u1) }
      )
    )
    
    (ok { proposal-id: proposal-id, end-block: (+ stacks-block-height voting-period) })
  )
)

(define-public (cast-vote (proposal-id uint) (vote-type (string-ascii 10)))
  (match (map-get? proposals proposal-id)
    proposal
    (let (
      (voter-power (get-voting-power tx-sender))
    )
      (asserts! (not (get cancelled proposal)) err-proposal-not-found)
      (asserts! (<= stacks-block-height (get end-block proposal)) err-voting-ended)
      (asserts! (>= stacks-block-height (get start-block proposal)) err-voting-active)
      (asserts! (> voter-power u0) err-insufficient-tokens)
      (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) err-already-voted)
      
      ;; Record vote
      (map-set votes { proposal-id: proposal-id, voter: tx-sender }
        {
          vote-type: vote-type,
          weight: voter-power,
          voted-at: stacks-block-height
        }
      )
      
      ;; Update proposal vote counts
      (map-set proposals proposal-id
        (merge proposal
          (if (is-eq vote-type "for")
            { for-votes: (+ (get for-votes proposal) voter-power) }
            (if (is-eq vote-type "against")
              { against-votes: (+ (get against-votes proposal) voter-power) }
              { abstain-votes: (+ (get abstain-votes proposal) voter-power) }
            )
          )
        )
      )
      
      ;; Update member stats
      (map-set member-stats tx-sender
        (merge (get-member-stats tx-sender)
          { votes-cast: (+ (get votes-cast (get-member-stats tx-sender)) u1) }
        )
      )
      
      (ok { proposal-id: proposal-id, vote-type: vote-type, weight: voter-power })
    )
    err-proposal-not-found
  )
)

(define-public (execute-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let (
      (total-votes (+ (+ (get for-votes proposal) (get against-votes proposal)) (get abstain-votes proposal)))
      (quorum-votes (/ (* (var-get total-voting-power) quorum-percentage) u10000))
    )
      (asserts! (not (get executed proposal)) err-already-executed)
      (asserts! (not (get cancelled proposal)) err-proposal-not-found)
      (asserts! (> stacks-block-height (get end-block proposal)) err-voting-active)
      (asserts! (>= stacks-block-height (+ (get end-block proposal) execution-delay)) err-execution-delay)
      (asserts! (>= total-votes quorum-votes) err-quorum-not-met)
      (asserts! (> (get for-votes proposal) (get against-votes proposal)) err-proposal-not-passed)
      
      ;; Mark as executed
      (map-set proposals proposal-id
        (merge proposal { executed: true })
      )
      
      ;; Update proposer stats
      (map-set member-stats (get proposer proposal)
        (merge (get-member-stats (get proposer proposal))
          { proposals-executed: (+ (get proposals-executed (get-member-stats (get proposer proposal))) u1) }
        )
      )
      
      (ok { proposal-id: proposal-id, executed: true })
    )
    err-proposal-not-found
  )
)

(define-public (cancel-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (begin
      (asserts! (or 
        (is-eq tx-sender (get proposer proposal))
        (is-eq tx-sender contract-owner)
      ) err-owner-only)
      (asserts! (not (get executed proposal)) err-already-executed)
      (asserts! (<= stacks-block-height (get end-block proposal)) err-voting-ended)
      
      (map-set proposals proposal-id
        (merge proposal { cancelled: true })
      )
      
      (ok { proposal-id: proposal-id, cancelled: true })
    )
    err-proposal-not-found
  )
)

(define-public (delegate (delegatee principal))
  (let (
    (current-power (get-voting-power tx-sender))
  )
    ;; Remove power from current delegate if any
    (match (map-get? delegations tx-sender)
      current-delegate
      (map-set voting-power current-delegate 
        (- (default-to u0 (map-get? voting-power current-delegate)) current-power))
      true
    )
    
    ;; Set new delegation
    (map-set delegations tx-sender delegatee)
    
    ;; Add power to new delegate
    (map-set voting-power delegatee 
      (+ (default-to u0 (map-get? voting-power delegatee)) current-power))
    
    (ok { delegated-to: delegatee, power: current-power })
  )
)

(define-public (undelegate)
  (let (
    (current-power (get-voting-power tx-sender))
  )
    (match (map-get? delegations tx-sender)
      current-delegate
      (begin
        ;; Remove power from delegate
        (map-set voting-power current-delegate 
          (- (default-to u0 (map-get? voting-power current-delegate)) current-power))
        ;; Remove delegation
        (map-delete delegations tx-sender)
        ;; Restore own power
        (map-set voting-power tx-sender current-power)
        (ok { undelegated: true })
      )
      (ok { undelegated: false })
    )
  )
)

(define-public (register-voting-power (amount uint))
  (begin
    (map-set voting-power tx-sender (+ (get-voting-power tx-sender) amount))
    (var-set total-voting-power (+ (var-get total-voting-power) amount))
    (ok { new-power: (get-voting-power tx-sender) })
  )
)

;; Read-only Functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (account principal))
  (default-to u0 (map-get? voting-power account))
)

(define-read-only (get-delegate (account principal))
  (map-get? delegations account)
)

(define-read-only (get-member-stats (member principal))
  (default-to 
    { proposals-created: u0, votes-cast: u0, proposals-executed: u0 }
    (map-get? member-stats member)
  )
)

(define-read-only (get-proposal-state (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (if (get cancelled proposal)
      "cancelled"
      (if (get executed proposal)
        "executed"
        (if (< stacks-block-height (get start-block proposal))
          "pending"
          (if (<= stacks-block-height (get end-block proposal))
            "active"
            (if (< stacks-block-height (+ (get end-block proposal) execution-delay))
              "queued"
              (let (
                (total-votes (+ (+ (get for-votes proposal) (get against-votes proposal)) (get abstain-votes proposal)))
                (quorum-votes (/ (* (var-get total-voting-power) quorum-percentage) u10000))
              )
                (if (< total-votes quorum-votes)
                  "defeated"
                  (if (> (get for-votes proposal) (get against-votes proposal))
                    "succeeded"
                    "defeated"
                  )
                )
              )
            )
          )
        )
      )
    )
    "not-found"
  )
)

(define-read-only (get-proposal-votes (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (ok {
      for-votes: (get for-votes proposal),
      against-votes: (get against-votes proposal),
      abstain-votes: (get abstain-votes proposal),
      total-votes: (+ (+ (get for-votes proposal) (get against-votes proposal)) (get abstain-votes proposal)),
      quorum-required: (/ (* (var-get total-voting-power) quorum-percentage) u10000)
    })
    err-proposal-not-found
  )
)

(define-read-only (get-governance-stats)
  {
    total-proposals: (var-get proposal-nonce),
    total-voting-power: (var-get total-voting-power),
    voting-period: voting-period,
    execution-delay: execution-delay,
    quorum-percentage: quorum-percentage,
    proposal-threshold: min-proposal-threshold
  }
)

(define-read-only (can-create-proposal (account principal))
  (>= (get-voting-power account) min-proposal-threshold)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

;; Admin Functions

(define-public (set-governance-token (token principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set governance-token token)
    (ok true)
  )
)
