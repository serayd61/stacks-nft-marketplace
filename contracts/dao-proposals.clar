;; DAO Proposals Contract
;; Advanced proposal management with templates and execution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-proposal-not-found (err u102))
(define-constant err-invalid-state (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-voting-closed (err u105))
(define-constant err-quorum-not-met (err u106))
(define-constant err-execution-failed (err u107))
(define-constant err-invalid-template (err u108))

;; Proposal states
(define-constant state-draft u0)
(define-constant state-pending u1)
(define-constant state-active u2)
(define-constant state-passed u3)
(define-constant state-rejected u4)
(define-constant state-executed u5)
(define-constant state-cancelled u6)

;; Proposal types
(define-constant type-general u1)
(define-constant type-funding u2)
(define-constant type-parameter u3)
(define-constant type-membership u4)
(define-constant type-emergency u5)

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var quorum-threshold uint u1000) ;; 10% in basis points
(define-data-var approval-threshold uint u5000) ;; 50% in basis points

;; Proposal templates
(define-map proposal-templates uint
  {
    name: (string-ascii 64),
    description: (string-utf8 256),
    required-fields: (list 10 (string-ascii 32)),
    voting-period: uint,
    execution-delay: uint,
    quorum-override: (optional uint)
  }
)

;; Initialize templates
(map-set proposal-templates type-general { name: "General Proposal", description: u"Standard governance proposal", required-fields: (list "title" "description"), voting-period: u10080, execution-delay: u1440, quorum-override: none })
(map-set proposal-templates type-funding { name: "Funding Request", description: u"Request treasury funds", required-fields: (list "title" "description" "amount" "recipient"), voting-period: u10080, execution-delay: u2880, quorum-override: (some u1500) })
(map-set proposal-templates type-parameter { name: "Parameter Change", description: u"Modify protocol parameters", required-fields: (list "title" "description" "parameter" "new-value"), voting-period: u7200, execution-delay: u4320, quorum-override: (some u2000) })
(map-set proposal-templates type-emergency { name: "Emergency Action", description: u"Urgent action required", required-fields: (list "title" "description" "action"), voting-period: u1440, execution-delay: u144, quorum-override: (some u500) })

;; Proposals
(define-map proposals uint
  {
    proposer: principal,
    proposal-type: uint,
    title: (string-ascii 128),
    description: (string-utf8 2048),
    state: uint,
    created-at: uint,
    voting-starts: uint,
    voting-ends: uint,
    execution-time: uint,
    for-votes: uint,
    against-votes: uint,
    abstain-votes: uint,
    total-voters: uint,
    executed-at: (optional uint),
    execution-result: (optional bool)
  }
)

;; Proposal parameters (for funding/parameter proposals)
(define-map proposal-params uint
  {
    amount: (optional uint),
    recipient: (optional principal),
    parameter-name: (optional (string-ascii 64)),
    parameter-value: (optional uint),
    target-contract: (optional principal)
  }
)

;; Votes
(define-map votes { proposal-id: uint, voter: principal }
  {
    vote: uint, ;; 1=for, 2=against, 3=abstain
    weight: uint,
    voted-at: uint,
    reason: (optional (string-utf8 256))
  }
)

;; Proposal discussions
(define-map discussions { proposal-id: uint, comment-id: uint }
  {
    author: principal,
    content: (string-utf8 512),
    created-at: uint,
    parent-id: (optional uint)
  }
)
(define-map discussion-counts uint uint)

;; Member voting power
(define-map voting-power principal uint)

;; Public Functions

(define-public (create-proposal
  (proposal-type uint)
  (title (string-ascii 128))
  (description (string-utf8 2048)))
  (let (
    (proposal-id (var-get proposal-nonce))
    (template (unwrap! (map-get? proposal-templates proposal-type) err-invalid-template))
    (voting-starts (+ stacks-block-height u144)) ;; 1 day delay
    (voting-ends (+ voting-starts (get voting-period template)))
    (execution-time (+ voting-ends (get execution-delay template)))
  )
    (asserts! (> (get-voter-power tx-sender) u0) err-not-member)
    
    ;; Create proposal
    (map-set proposals proposal-id
      {
        proposer: tx-sender,
        proposal-type: proposal-type,
        title: title,
        description: description,
        state: state-pending,
        created-at: stacks-block-height,
        voting-starts: voting-starts,
        voting-ends: voting-ends,
        execution-time: execution-time,
        for-votes: u0,
        against-votes: u0,
        abstain-votes: u0,
        total-voters: u0,
        executed-at: none,
        execution-result: none
      }
    )
    
    (var-set proposal-nonce (+ proposal-id u1))
    
    (ok { proposal-id: proposal-id, voting-starts: voting-starts, voting-ends: voting-ends })
  )
)

(define-public (create-funding-proposal
  (title (string-ascii 128))
  (description (string-utf8 2048))
  (amount uint)
  (recipient principal))
  (let (
    (result (try! (create-proposal type-funding title description)))
    (proposal-id (get proposal-id result))
  )
    ;; Add funding parameters
    (map-set proposal-params proposal-id
      {
        amount: (some amount),
        recipient: (some recipient),
        parameter-name: none,
        parameter-value: none,
        target-contract: none
      }
    )
    
    (ok result)
  )
)

(define-public (create-parameter-proposal
  (title (string-ascii 128))
  (description (string-utf8 2048))
  (param-name (string-ascii 64))
  (param-value uint)
  (target principal))
  (let (
    (result (try! (create-proposal type-parameter title description)))
    (proposal-id (get proposal-id result))
  )
    ;; Add parameter change details
    (map-set proposal-params proposal-id
      {
        amount: none,
        recipient: none,
        parameter-name: (some param-name),
        parameter-value: (some param-value),
        target-contract: (some target)
      }
    )
    
    (ok result)
  )
)

(define-public (vote (proposal-id uint) (vote-choice uint) (reason (optional (string-utf8 256))))
  (match (map-get? proposals proposal-id)
    proposal
    (let (
      (voter-weight (get-voter-power tx-sender))
    )
      (asserts! (is-eq (get state proposal) state-pending) err-invalid-state)
      (asserts! (>= stacks-block-height (get voting-starts proposal)) err-voting-closed)
      (asserts! (<= stacks-block-height (get voting-ends proposal)) err-voting-closed)
      (asserts! (> voter-weight u0) err-not-member)
      (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) err-already-voted)
      (asserts! (and (>= vote-choice u1) (<= vote-choice u3)) err-invalid-state)
      
      ;; Record vote
      (map-set votes { proposal-id: proposal-id, voter: tx-sender }
        {
          vote: vote-choice,
          weight: voter-weight,
          voted-at: stacks-block-height,
          reason: reason
        }
      )
      
      ;; Update proposal vote counts
      (map-set proposals proposal-id
        (merge proposal
          {
            for-votes: (if (is-eq vote-choice u1) (+ (get for-votes proposal) voter-weight) (get for-votes proposal)),
            against-votes: (if (is-eq vote-choice u2) (+ (get against-votes proposal) voter-weight) (get against-votes proposal)),
            abstain-votes: (if (is-eq vote-choice u3) (+ (get abstain-votes proposal) voter-weight) (get abstain-votes proposal)),
            total-voters: (+ (get total-voters proposal) u1)
          }
        )
      )
      
      ;; Check if voting should transition to active
      (if (and 
            (is-eq (get state proposal) state-pending)
            (>= stacks-block-height (get voting-starts proposal)))
        (map-set proposals proposal-id
          (merge proposal { state: state-active })
        )
        true
      )
      
      (ok { proposal-id: proposal-id, vote: vote-choice, weight: voter-weight })
    )
    err-proposal-not-found
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let (
      (total-votes (+ (+ (get for-votes proposal) (get against-votes proposal)) (get abstain-votes proposal)))
      (template (unwrap! (map-get? proposal-templates (get proposal-type proposal)) err-invalid-template))
      (quorum (default-to (var-get quorum-threshold) (get quorum-override template)))
      (quorum-met (>= (* total-votes u10000) (* (get-total-voting-power) quorum)))
      (approval-met (> (* (get for-votes proposal) u10000) (* (+ (get for-votes proposal) (get against-votes proposal)) (var-get approval-threshold))))
    )
      (asserts! (> stacks-block-height (get voting-ends proposal)) err-voting-closed)
      (asserts! (or (is-eq (get state proposal) state-pending) (is-eq (get state proposal) state-active)) err-invalid-state)
      
      (if (and quorum-met approval-met)
        (begin
          (map-set proposals proposal-id
            (merge proposal { state: state-passed })
          )
          (ok { proposal-id: proposal-id, state: "passed" })
        )
        (begin
          (map-set proposals proposal-id
            (merge proposal { state: state-rejected })
          )
          (ok { proposal-id: proposal-id, state: "rejected" })
        )
      )
    )
    err-proposal-not-found
  )
)

(define-public (execute-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (begin
      (asserts! (is-eq (get state proposal) state-passed) err-invalid-state)
      (asserts! (>= stacks-block-height (get execution-time proposal)) err-voting-closed)
      
      ;; Mark as executed
      (map-set proposals proposal-id
        (merge proposal {
          state: state-executed,
          executed-at: (some stacks-block-height),
          execution-result: (some true)
        })
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
      (asserts! (or (is-eq tx-sender (get proposer proposal)) (is-eq tx-sender contract-owner)) err-owner-only)
      (asserts! (or (is-eq (get state proposal) state-draft) (is-eq (get state proposal) state-pending)) err-invalid-state)
      
      (map-set proposals proposal-id
        (merge proposal { state: state-cancelled })
      )
      
      (ok { proposal-id: proposal-id, cancelled: true })
    )
    err-proposal-not-found
  )
)

(define-public (add-discussion (proposal-id uint) (content (string-utf8 512)) (parent-id (optional uint)))
  (match (map-get? proposals proposal-id)
    proposal
    (let (
      (comment-id (default-to u0 (map-get? discussion-counts proposal-id)))
    )
      (map-set discussions { proposal-id: proposal-id, comment-id: comment-id }
        {
          author: tx-sender,
          content: content,
          created-at: stacks-block-height,
          parent-id: parent-id
        }
      )
      (map-set discussion-counts proposal-id (+ comment-id u1))
      
      (ok { comment-id: comment-id })
    )
    err-proposal-not-found
  )
)

(define-public (register-voting-power (amount uint))
  (begin
    (map-set voting-power tx-sender (+ (get-voter-power tx-sender) amount))
    (ok { new-power: (get-voter-power tx-sender) })
  )
)

;; Read-only Functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-params (proposal-id uint))
  (map-get? proposal-params proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voter-power (voter principal))
  (default-to u0 (map-get? voting-power voter))
)

(define-read-only (get-total-voting-power)
  u1000000 ;; Placeholder - should aggregate all voting power
)

(define-read-only (get-proposal-template (template-type uint))
  (map-get? proposal-templates template-type)
)

(define-read-only (get-proposal-state-name (state uint))
  (if (is-eq state state-draft) "draft"
    (if (is-eq state state-pending) "pending"
      (if (is-eq state state-active) "active"
        (if (is-eq state state-passed) "passed"
          (if (is-eq state state-rejected) "rejected"
            (if (is-eq state state-executed) "executed"
              (if (is-eq state state-cancelled) "cancelled"
                "unknown"
              )
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-proposal-summary (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let (
      (total-votes (+ (+ (get for-votes proposal) (get against-votes proposal)) (get abstain-votes proposal)))
    )
      (ok {
        title: (get title proposal),
        state: (get-proposal-state-name (get state proposal)),
        for-votes: (get for-votes proposal),
        against-votes: (get against-votes proposal),
        abstain-votes: (get abstain-votes proposal),
        total-votes: total-votes,
        total-voters: (get total-voters proposal),
        approval-rate: (if (> (+ (get for-votes proposal) (get against-votes proposal)) u0)
                         (/ (* (get for-votes proposal) u10000) (+ (get for-votes proposal) (get against-votes proposal)))
                         u0),
        voting-ends: (get voting-ends proposal),
        can-execute: (and 
                       (is-eq (get state proposal) state-passed)
                       (>= stacks-block-height (get execution-time proposal)))
      })
    )
    err-proposal-not-found
  )
)

(define-read-only (get-discussion (proposal-id uint) (comment-id uint))
  (map-get? discussions { proposal-id: proposal-id, comment-id: comment-id })
)

(define-read-only (get-discussion-count (proposal-id uint))
  (default-to u0 (map-get? discussion-counts proposal-id))
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (get-governance-params)
  {
    quorum-threshold: (var-get quorum-threshold),
    approval-threshold: (var-get approval-threshold),
    total-proposals: (var-get proposal-nonce)
  }
)

;; Admin Functions

(define-public (set-quorum-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set quorum-threshold new-threshold)
    (ok new-threshold)
  )
)

(define-public (set-approval-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set approval-threshold new-threshold)
    (ok new-threshold)
  )
)

(define-public (update-template (template-type uint) (name (string-ascii 64)) (description (string-utf8 256)) (voting-period uint) (execution-delay uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set proposal-templates template-type
      {
        name: name,
        description: description,
        required-fields: (list "title" "description"),
        voting-period: voting-period,
        execution-delay: execution-delay,
        quorum-override: none
      }
    )
    (ok true)
  )
)
