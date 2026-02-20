;; DAO Voting Contract
;; Flexible voting mechanisms for DAO decisions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-poll-not-found (err u101))
(define-constant err-voting-ended (err u102))
(define-constant err-voting-not-started (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-invalid-option (err u105))
(define-constant err-not-eligible (err u106))
(define-constant err-poll-cancelled (err u107))

;; Vote types
(define-constant vote-type-single u1)
(define-constant vote-type-multiple u2)
(define-constant vote-type-ranked u3)
(define-constant vote-type-quadratic u4)

;; Data Variables
(define-data-var poll-nonce uint u0)
(define-data-var total-polls uint u0)

;; Polls
(define-map polls uint
  {
    creator: principal,
    title: (string-ascii 128),
    description: (string-utf8 512),
    vote-type: uint,
    options: (list 10 (string-ascii 64)),
    start-block: uint,
    end-block: uint,
    min-voting-power: uint,
    total-votes: uint,
    cancelled: bool,
    finalized: bool
  }
)

;; Vote counts per option
(define-map vote-counts { poll-id: uint, option-index: uint } uint)

;; User votes
(define-map user-votes { poll-id: uint, voter: principal }
  {
    options-selected: (list 10 uint),
    weight: uint,
    voted-at: uint
  }
)

;; Voting power (can be linked to token holdings)
(define-map voting-power principal uint)

;; Poll results
(define-map poll-results uint
  {
    winning-option: uint,
    winning-votes: uint,
    total-participation: uint,
    finalized-at: uint
  }
)

;; Voter stats
(define-map voter-stats principal
  {
    polls-participated: uint,
    total-voting-power-used: uint
  }
)

;; Public Functions

(define-public (create-poll
  (title (string-ascii 128))
  (description (string-utf8 512))
  (vote-type uint)
  (options (list 10 (string-ascii 64)))
  (duration uint)
  (min-voting-power uint))
  (let (
    (poll-id (var-get poll-nonce))
  )
    (asserts! (> (len options) u1) err-invalid-option)
    (asserts! (<= vote-type u4) err-invalid-option)
    
    ;; Create poll
    (map-set polls poll-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        vote-type: vote-type,
        options: options,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        min-voting-power: min-voting-power,
        total-votes: u0,
        cancelled: false,
        finalized: false
      }
    )
    
    ;; Initialize vote counts
    (fold init-vote-count options { poll-id: poll-id, index: u0 })
    
    (var-set poll-nonce (+ poll-id u1))
    (var-set total-polls (+ (var-get total-polls) u1))
    
    (ok { poll-id: poll-id, end-block: (+ stacks-block-height duration) })
  )
)

(define-private (init-vote-count (option (string-ascii 64)) (state { poll-id: uint, index: uint }))
  (begin
    (map-set vote-counts { poll-id: (get poll-id state), option-index: (get index state) } u0)
    { poll-id: (get poll-id state), index: (+ (get index state) u1) }
  )
)

(define-public (cast-vote (poll-id uint) (selected-options (list 10 uint)))
  (match (map-get? polls poll-id)
    poll
    (let (
      (voter-power (get-voter-power tx-sender))
      (effective-weight (calculate-vote-weight (get vote-type poll) voter-power (len selected-options)))
    )
      (asserts! (not (get cancelled poll)) err-poll-cancelled)
      (asserts! (>= stacks-block-height (get start-block poll)) err-voting-not-started)
      (asserts! (<= stacks-block-height (get end-block poll)) err-voting-ended)
      (asserts! (>= voter-power (get min-voting-power poll)) err-not-eligible)
      (asserts! (is-none (map-get? user-votes { poll-id: poll-id, voter: tx-sender })) err-already-voted)
      (asserts! (validate-options selected-options (len (get options poll))) err-invalid-option)
      
      ;; Record vote
      (map-set user-votes { poll-id: poll-id, voter: tx-sender }
        {
          options-selected: selected-options,
          weight: effective-weight,
          voted-at: stacks-block-height
        }
      )
      
      ;; Update vote counts
      (fold update-vote-count selected-options { poll-id: poll-id, weight: effective-weight })
      
      ;; Update poll total
      (map-set polls poll-id
        (merge poll { total-votes: (+ (get total-votes poll) u1) })
      )
      
      ;; Update voter stats
      (map-set voter-stats tx-sender
        (merge (get-voter-stats tx-sender)
          {
            polls-participated: (+ (get polls-participated (get-voter-stats tx-sender)) u1),
            total-voting-power-used: (+ (get total-voting-power-used (get-voter-stats tx-sender)) effective-weight)
          }
        )
      )
      
      (ok { poll-id: poll-id, weight: effective-weight })
    )
    err-poll-not-found
  )
)

(define-private (update-vote-count (option-index uint) (state { poll-id: uint, weight: uint }))
  (begin
    (map-set vote-counts 
      { poll-id: (get poll-id state), option-index: option-index }
      (+ (default-to u0 (map-get? vote-counts { poll-id: (get poll-id state), option-index: option-index })) (get weight state))
    )
    state
  )
)

(define-private (validate-options (selected (list 10 uint)) (max-options uint))
  (fold validate-option selected { valid: true, max: max-options })
)

(define-private (validate-option (option uint) (state { valid: bool, max: uint }))
  { valid: (and (get valid state) (< option (get max state))), max: (get max state) }
)

(define-private (calculate-vote-weight (vote-type uint) (power uint) (num-selections uint))
  (if (is-eq vote-type vote-type-quadratic)
    (sqrti power)
    (if (is-eq vote-type vote-type-multiple)
      (/ power num-selections)
      power
    )
  )
)

(define-public (finalize-poll (poll-id uint))
  (match (map-get? polls poll-id)
    poll
    (begin
      (asserts! (> stacks-block-height (get end-block poll)) err-voting-not-started)
      (asserts! (not (get finalized poll)) err-poll-cancelled)
      
      ;; Find winning option
      (let (
        (result (find-winner poll-id (len (get options poll))))
      )
        ;; Store results
        (map-set poll-results poll-id
          {
            winning-option: (get winner result),
            winning-votes: (get votes result),
            total-participation: (get total-votes poll),
            finalized-at: stacks-block-height
          }
        )
        
        ;; Mark as finalized
        (map-set polls poll-id
          (merge poll { finalized: true })
        )
        
        (ok { 
          poll-id: poll-id, 
          winning-option: (get winner result),
          winning-votes: (get votes result)
        })
      )
    )
    err-poll-not-found
  )
)

(define-private (find-winner (poll-id uint) (num-options uint))
  (fold check-option-votes 
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
    { poll-id: poll-id, max-options: num-options, winner: u0, votes: u0 }
  )
)

(define-private (check-option-votes (index uint) (state { poll-id: uint, max-options: uint, winner: uint, votes: uint }))
  (if (< index (get max-options state))
    (let (
      (option-votes (default-to u0 (map-get? vote-counts { poll-id: (get poll-id state), option-index: index })))
    )
      (if (> option-votes (get votes state))
        { poll-id: (get poll-id state), max-options: (get max-options state), winner: index, votes: option-votes }
        state
      )
    )
    state
  )
)

(define-public (cancel-poll (poll-id uint))
  (match (map-get? polls poll-id)
    poll
    (begin
      (asserts! (or (is-eq tx-sender (get creator poll)) (is-eq tx-sender contract-owner)) err-owner-only)
      (asserts! (<= stacks-block-height (get end-block poll)) err-voting-ended)
      
      (map-set polls poll-id
        (merge poll { cancelled: true })
      )
      
      (ok { poll-id: poll-id, cancelled: true })
    )
    err-poll-not-found
  )
)

(define-public (register-voting-power (amount uint))
  (begin
    (map-set voting-power tx-sender (+ (get-voter-power tx-sender) amount))
    (ok { new-power: (get-voter-power tx-sender) })
  )
)

;; Read-only Functions

(define-read-only (get-poll (poll-id uint))
  (map-get? polls poll-id)
)

(define-read-only (get-vote (poll-id uint) (voter principal))
  (map-get? user-votes { poll-id: poll-id, voter: voter })
)

(define-read-only (get-option-votes (poll-id uint) (option-index uint))
  (default-to u0 (map-get? vote-counts { poll-id: poll-id, option-index: option-index }))
)

(define-read-only (get-voter-power (voter principal))
  (default-to u0 (map-get? voting-power voter))
)

(define-read-only (get-voter-stats (voter principal))
  (default-to 
    { polls-participated: u0, total-voting-power-used: u0 }
    (map-get? voter-stats voter)
  )
)

(define-read-only (get-poll-results (poll-id uint))
  (map-get? poll-results poll-id)
)

(define-read-only (get-poll-status (poll-id uint))
  (match (map-get? polls poll-id)
    poll
    (if (get cancelled poll)
      "cancelled"
      (if (get finalized poll)
        "finalized"
        (if (< stacks-block-height (get start-block poll))
          "pending"
          (if (<= stacks-block-height (get end-block poll))
            "active"
            "ended"
          )
        )
      )
    )
    "not-found"
  )
)

(define-read-only (get-all-option-votes (poll-id uint))
  (match (map-get? polls poll-id)
    poll
    (ok (map get-single-option-votes (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)))
    err-poll-not-found
  )
)

(define-private (get-single-option-votes (index uint))
  { option: index, votes: u0 }
)

(define-read-only (has-voted (poll-id uint) (voter principal))
  (is-some (map-get? user-votes { poll-id: poll-id, voter: voter }))
)

(define-read-only (can-vote (poll-id uint) (voter principal))
  (match (map-get? polls poll-id)
    poll
    (and
      (not (get cancelled poll))
      (>= stacks-block-height (get start-block poll))
      (<= stacks-block-height (get end-block poll))
      (>= (get-voter-power voter) (get min-voting-power poll))
      (not (has-voted poll-id voter))
    )
    false
  )
)

(define-read-only (get-voting-stats)
  {
    total-polls: (var-get total-polls),
    poll-nonce: (var-get poll-nonce)
  }
)

;; Helper function for square root
(define-read-only (sqrti (n uint))
  (if (<= n u1)
    n
    (sqrti-helper n (/ n u2))
  )
)

(define-private (sqrti-helper (n uint) (x uint))
  (let (
    (x-new (/ (+ x (/ n x)) u2))
  )
    (if (>= x-new x)
      x
      (sqrti-helper n x-new)
    )
  )
)

;; Admin Functions

(define-public (set-voter-power (voter principal) (power uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set voting-power voter power)
    (ok power)
  )
)
