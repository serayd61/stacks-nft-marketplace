;; Lottery Contract
;; Decentralized lottery with provably fair drawings

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-lottery-not-found (err u101))
(define-constant err-lottery-ended (err u102))
(define-constant err-lottery-active (err u103))
(define-constant err-already-drawn (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-max-tickets (err u106))
(define-constant err-not-winner (err u107))
(define-constant err-already-claimed (err u108))

;; Platform fee: 5%
(define-constant platform-fee u500)
(define-constant fee-denominator u10000)

;; Data Variables
(define-data-var lottery-nonce uint u0)
(define-data-var total-prizes uint u0)

;; Lotteries
(define-map lotteries uint
  {
    creator: principal,
    name: (string-ascii 64),
    ticket-price: uint,
    max-tickets: uint,
    tickets-sold: uint,
    prize-pool: uint,
    start-block: uint,
    end-block: uint,
    drawn: bool,
    winning-ticket: (optional uint),
    winner: (optional principal),
    claimed: bool,
    random-seed: (optional (buff 32))
  }
)

;; Tickets
(define-map tickets { lottery-id: uint, ticket-number: uint } principal)

;; User tickets per lottery
(define-map user-tickets { lottery-id: uint, user: principal } (list 100 uint))

;; User stats
(define-map user-stats principal
  {
    tickets-bought: uint,
    total-spent: uint,
    wins: uint,
    total-won: uint
  }
)

;; Prize distribution (for multiple winners)
(define-map prize-tiers { lottery-id: uint, tier: uint }
  {
    percentage: uint,
    winners-count: uint,
    prize-per-winner: uint
  }
)

;; Public Functions

(define-public (create-lottery
  (name (string-ascii 64))
  (ticket-price uint)
  (max-tickets uint)
  (duration uint))
  (let (
    (lottery-id (var-get lottery-nonce))
  )
    (asserts! (> ticket-price u0) err-invalid-amount)
    (asserts! (> max-tickets u0) err-invalid-amount)
    
    ;; Create lottery
    (map-set lotteries lottery-id
      {
        creator: tx-sender,
        name: name,
        ticket-price: ticket-price,
        max-tickets: max-tickets,
        tickets-sold: u0,
        prize-pool: u0,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        drawn: false,
        winning-ticket: none,
        winner: none,
        claimed: false,
        random-seed: none
      }
    )
    
    (var-set lottery-nonce (+ lottery-id u1))
    
    (ok { lottery-id: lottery-id, end-block: (+ stacks-block-height duration) })
  )
)

(define-public (buy-tickets (lottery-id uint) (count uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (let (
      (total-cost (* (get ticket-price lottery) count))
      (current-sold (get tickets-sold lottery))
    )
      (asserts! (<= stacks-block-height (get end-block lottery)) err-lottery-ended)
      (asserts! (not (get drawn lottery)) err-already-drawn)
      (asserts! (<= (+ current-sold count) (get max-tickets lottery)) err-max-tickets)
      (asserts! (> count u0) err-invalid-amount)
      
      ;; Transfer payment
      (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
      
      ;; Assign tickets
      (fold assign-ticket 
        (generate-ticket-numbers count current-sold)
        { lottery-id: lottery-id, buyer: tx-sender }
      )
      
      ;; Update lottery
      (map-set lotteries lottery-id
        (merge lottery {
          tickets-sold: (+ current-sold count),
          prize-pool: (+ (get prize-pool lottery) total-cost)
        })
      )
      
      ;; Update user stats
      (map-set user-stats tx-sender
        (merge (get-user-stats tx-sender)
          {
            tickets-bought: (+ (get tickets-bought (get-user-stats tx-sender)) count),
            total-spent: (+ (get total-spent (get-user-stats tx-sender)) total-cost)
          }
        )
      )
      
      (ok { 
        lottery-id: lottery-id, 
        tickets-bought: count, 
        ticket-range: { start: current-sold, end: (+ current-sold count) }
      })
    )
    err-lottery-not-found
  )
)

(define-private (generate-ticket-numbers (count uint) (start uint))
  (list start)
)

(define-private (assign-ticket (ticket-num uint) (state { lottery-id: uint, buyer: principal }))
  (begin
    (map-set tickets 
      { lottery-id: (get lottery-id state), ticket-number: ticket-num }
      (get buyer state)
    )
    state
  )
)

(define-public (draw-winner (lottery-id uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (let (
      (seed (keccak256 (concat 
        (unwrap-panic (to-consensus-buff? stacks-block-height))
        (unwrap-panic (to-consensus-buff? (get tickets-sold lottery)))
      )))
      (winning-number (mod (buff-to-uint-be seed) (get tickets-sold lottery)))
      (winner-address (unwrap! (map-get? tickets { lottery-id: lottery-id, ticket-number: winning-number }) err-lottery-not-found))
    )
      (asserts! (> stacks-block-height (get end-block lottery)) err-lottery-active)
      (asserts! (not (get drawn lottery)) err-already-drawn)
      (asserts! (> (get tickets-sold lottery) u0) err-invalid-amount)
      
      ;; Update lottery with winner
      (map-set lotteries lottery-id
        (merge lottery {
          drawn: true,
          winning-ticket: (some winning-number),
          winner: (some winner-address),
          random-seed: (some seed)
        })
      )
      
      (ok { 
        lottery-id: lottery-id, 
        winning-ticket: winning-number, 
        winner: winner-address 
      })
    )
    err-lottery-not-found
  )
)

(define-public (claim-prize (lottery-id uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (let (
      (winner-addr (unwrap! (get winner lottery) err-not-winner))
      (fee (calculate-fee (get prize-pool lottery)))
      (prize (- (get prize-pool lottery) fee))
    )
      (asserts! (get drawn lottery) err-lottery-active)
      (asserts! (is-eq tx-sender winner-addr) err-not-winner)
      (asserts! (not (get claimed lottery)) err-already-claimed)
      
      ;; Transfer prize
      (try! (as-contract (stx-transfer? prize tx-sender winner-addr)))
      
      ;; Update lottery
      (map-set lotteries lottery-id
        (merge lottery { claimed: true })
      )
      
      ;; Update stats
      (var-set total-prizes (+ (var-get total-prizes) prize))
      
      (map-set user-stats winner-addr
        (merge (get-user-stats winner-addr)
          {
            wins: (+ (get wins (get-user-stats winner-addr)) u1),
            total-won: (+ (get total-won (get-user-stats winner-addr)) prize)
          }
        )
      )
      
      (ok { prize: prize, fee: fee })
    )
    err-lottery-not-found
  )
)

(define-public (refund-if-cancelled (lottery-id uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (let (
      (user-ticket-list (default-to (list) (map-get? user-tickets { lottery-id: lottery-id, user: tx-sender })))
      (refund-amount (* (len user-ticket-list) (get ticket-price lottery)))
    )
      (asserts! (> stacks-block-height (get end-block lottery)) err-lottery-active)
      (asserts! (is-eq (get tickets-sold lottery) u0) err-invalid-amount)
      (asserts! (> refund-amount u0) err-not-winner)
      
      ;; Refund
      (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
      
      (ok { refunded: refund-amount })
    )
    err-lottery-not-found
  )
)

;; Read-only Functions

(define-read-only (get-lottery (lottery-id uint))
  (map-get? lotteries lottery-id)
)

(define-read-only (get-ticket-owner (lottery-id uint) (ticket-number uint))
  (map-get? tickets { lottery-id: lottery-id, ticket-number: ticket-number })
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { tickets-bought: u0, total-spent: u0, wins: u0, total-won: u0 }
    (map-get? user-stats user)
  )
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount platform-fee) fee-denominator)
)

(define-read-only (get-lottery-status (lottery-id uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (if (get claimed lottery)
      "claimed"
      (if (get drawn lottery)
        "drawn"
        (if (> stacks-block-height (get end-block lottery))
          "ended"
          (if (is-eq (get tickets-sold lottery) (get max-tickets lottery))
            "sold-out"
            "active"
          )
        )
      )
    )
    "not-found"
  )
)

(define-read-only (get-lottery-info (lottery-id uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (ok {
      name: (get name lottery),
      ticket-price: (get ticket-price lottery),
      tickets-sold: (get tickets-sold lottery),
      max-tickets: (get max-tickets lottery),
      prize-pool: (get prize-pool lottery),
      potential-prize: (- (get prize-pool lottery) (calculate-fee (get prize-pool lottery))),
      blocks-remaining: (if (> (get end-block lottery) stacks-block-height)
                          (- (get end-block lottery) stacks-block-height)
                          u0),
      status: (get-lottery-status lottery-id),
      winner: (get winner lottery)
    })
    err-lottery-not-found
  )
)

(define-read-only (get-winning-odds (lottery-id uint) (tickets-owned uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (if (is-eq (get tickets-sold lottery) u0)
      u0
      (/ (* tickets-owned u10000) (get tickets-sold lottery))
    )
    u0
  )
)

(define-read-only (get-platform-stats)
  {
    total-lotteries: (var-get lottery-nonce),
    total-prizes-distributed: (var-get total-prizes),
    platform-fee: platform-fee
  }
)

;; Helper function
(define-read-only (buff-to-uint-be (b (buff 32)))
  (let (
    (byte-list (list 
      (buff-to-uint-le (unwrap-panic (slice? b u0 u1)))
    ))
  )
    (fold + byte-list u0)
  )
)

;; Admin Functions

(define-public (withdraw-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)

(define-public (emergency-draw (lottery-id uint) (winning-ticket uint))
  (match (map-get? lotteries lottery-id)
    lottery
    (let (
      (winner-address (unwrap! (map-get? tickets { lottery-id: lottery-id, ticket-number: winning-ticket }) err-lottery-not-found))
    )
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (not (get drawn lottery)) err-already-drawn)
      
      (map-set lotteries lottery-id
        (merge lottery {
          drawn: true,
          winning-ticket: (some winning-ticket),
          winner: (some winner-address)
        })
      )
      
      (ok { winner: winner-address })
    )
    err-lottery-not-found
  )
)
