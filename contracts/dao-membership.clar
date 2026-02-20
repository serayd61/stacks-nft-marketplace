;; DAO Membership Contract
;; Manage DAO membership with tiers and benefits

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-member (err u101))
(define-constant err-not-member (err u102))
(define-constant err-invalid-tier (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-membership-expired (err u105))
(define-constant err-cooldown-active (err u106))
(define-constant err-invite-invalid (err u107))

;; Membership tiers
(define-constant tier-basic u1)
(define-constant tier-silver u2)
(define-constant tier-gold u3)
(define-constant tier-platinum u4)

;; Data Variables
(define-data-var member-count uint u0)
(define-data-var total-fees-collected uint u0)
(define-data-var membership-nonce uint u0)

;; Tier configurations
(define-map tier-configs uint
  {
    name: (string-ascii 32),
    monthly-fee: uint,
    voting-weight: uint,
    proposal-limit: uint,
    benefits: (string-utf8 256)
  }
)

;; Initialize tiers
(map-set tier-configs tier-basic { name: "Basic", monthly-fee: u1000000, voting-weight: u1, proposal-limit: u1, benefits: u"Basic voting rights" })
(map-set tier-configs tier-silver { name: "Silver", monthly-fee: u5000000, voting-weight: u2, proposal-limit: u3, benefits: u"Enhanced voting, early access" })
(map-set tier-configs tier-gold { name: "Gold", monthly-fee: u10000000, voting-weight: u5, proposal-limit: u5, benefits: u"Premium voting, exclusive events" })
(map-set tier-configs tier-platinum { name: "Platinum", monthly-fee: u25000000, voting-weight: u10, proposal-limit: u10, benefits: u"Maximum benefits, direct governance" })

;; Members
(define-map members principal
  {
    member-id: uint,
    tier: uint,
    joined-at: uint,
    expires-at: uint,
    total-paid: uint,
    referrer: (optional principal),
    active: bool
  }
)

;; Member stats
(define-map member-stats principal
  {
    proposals-created: uint,
    votes-cast: uint,
    referrals: uint,
    reputation: uint
  }
)

;; Invites
(define-map invites (buff 32)
  {
    creator: principal,
    tier: uint,
    uses-remaining: uint,
    expires-at: uint
  }
)

;; Referral rewards
(define-map referral-rewards principal uint)

;; Public Functions

(define-public (join (tier uint))
  (let (
    (tier-config (unwrap! (map-get? tier-configs tier) err-invalid-tier))
    (member-id (var-get membership-nonce))
    (duration u4320) ;; ~30 days
  )
    (asserts! (is-none (map-get? members tx-sender)) err-already-member)
    
    ;; Pay membership fee
    (try! (stx-transfer? (get monthly-fee tier-config) tx-sender (as-contract tx-sender)))
    
    ;; Create membership
    (map-set members tx-sender
      {
        member-id: member-id,
        tier: tier,
        joined-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        total-paid: (get monthly-fee tier-config),
        referrer: none,
        active: true
      }
    )
    
    ;; Initialize stats
    (map-set member-stats tx-sender
      { proposals-created: u0, votes-cast: u0, referrals: u0, reputation: u100 }
    )
    
    (var-set membership-nonce (+ member-id u1))
    (var-set member-count (+ (var-get member-count) u1))
    (var-set total-fees-collected (+ (var-get total-fees-collected) (get monthly-fee tier-config)))
    
    (ok { member-id: member-id, tier: tier, expires-at: (+ stacks-block-height duration) })
  )
)

(define-public (join-with-referral (tier uint) (referrer principal))
  (let (
    (tier-config (unwrap! (map-get? tier-configs tier) err-invalid-tier))
    (member-id (var-get membership-nonce))
    (duration u4320)
    (referrer-member (unwrap! (map-get? members referrer) err-not-member))
    (discounted-fee (/ (* (get monthly-fee tier-config) u90) u100)) ;; 10% discount
    (referral-reward (/ (get monthly-fee tier-config) u10)) ;; 10% to referrer
  )
    (asserts! (is-none (map-get? members tx-sender)) err-already-member)
    (asserts! (get active referrer-member) err-not-member)
    
    ;; Pay discounted fee
    (try! (stx-transfer? discounted-fee tx-sender (as-contract tx-sender)))
    
    ;; Create membership
    (map-set members tx-sender
      {
        member-id: member-id,
        tier: tier,
        joined-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        total-paid: discounted-fee,
        referrer: (some referrer),
        active: true
      }
    )
    
    ;; Initialize stats
    (map-set member-stats tx-sender
      { proposals-created: u0, votes-cast: u0, referrals: u0, reputation: u100 }
    )
    
    ;; Update referrer stats and rewards
    (map-set member-stats referrer
      (merge (get-member-stats referrer)
        { referrals: (+ (get referrals (get-member-stats referrer)) u1) }
      )
    )
    (map-set referral-rewards referrer
      (+ (default-to u0 (map-get? referral-rewards referrer)) referral-reward)
    )
    
    (var-set membership-nonce (+ member-id u1))
    (var-set member-count (+ (var-get member-count) u1))
    
    (ok { member-id: member-id, tier: tier, discount-applied: true })
  )
)

(define-public (renew-membership (months uint))
  (match (map-get? members tx-sender)
    member
    (let (
      (tier-config (unwrap! (map-get? tier-configs (get tier member)) err-invalid-tier))
      (total-fee (* (get monthly-fee tier-config) months))
      (duration (* u4320 months))
      (new-expiry (if (> (get expires-at member) stacks-block-height)
                    (+ (get expires-at member) duration)
                    (+ stacks-block-height duration)))
    )
      ;; Pay renewal fee
      (try! (stx-transfer? total-fee tx-sender (as-contract tx-sender)))
      
      ;; Update membership
      (map-set members tx-sender
        (merge member {
          expires-at: new-expiry,
          total-paid: (+ (get total-paid member) total-fee),
          active: true
        })
      )
      
      (var-set total-fees-collected (+ (var-get total-fees-collected) total-fee))
      
      (ok { new-expiry: new-expiry, paid: total-fee })
    )
    err-not-member
  )
)

(define-public (upgrade-tier (new-tier uint))
  (match (map-get? members tx-sender)
    member
    (let (
      (current-tier-config (unwrap! (map-get? tier-configs (get tier member)) err-invalid-tier))
      (new-tier-config (unwrap! (map-get? tier-configs new-tier) err-invalid-tier))
      (upgrade-fee (- (get monthly-fee new-tier-config) (get monthly-fee current-tier-config)))
    )
      (asserts! (> new-tier (get tier member)) err-invalid-tier)
      (asserts! (>= stacks-block-height (get expires-at member)) err-membership-expired)
      
      ;; Pay upgrade fee
      (try! (stx-transfer? upgrade-fee tx-sender (as-contract tx-sender)))
      
      ;; Update membership
      (map-set members tx-sender
        (merge member {
          tier: new-tier,
          total-paid: (+ (get total-paid member) upgrade-fee)
        })
      )
      
      (ok { new-tier: new-tier, upgrade-fee: upgrade-fee })
    )
    err-not-member
  )
)

(define-public (claim-referral-rewards)
  (let (
    (rewards (default-to u0 (map-get? referral-rewards tx-sender)))
  )
    (asserts! (> rewards u0) err-insufficient-payment)
    
    ;; Transfer rewards
    (try! (as-contract (stx-transfer? rewards tx-sender tx-sender)))
    
    ;; Clear rewards
    (map-set referral-rewards tx-sender u0)
    
    (ok { claimed: rewards })
  )
)

(define-public (create-invite (tier uint) (uses uint) (duration uint))
  (match (map-get? members tx-sender)
    member
    (let (
      (invite-hash (keccak256 (concat (unwrap-panic (to-consensus-buff? tx-sender)) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
    )
      (asserts! (get active member) err-membership-expired)
      (asserts! (>= (get tier member) tier) err-invalid-tier)
      
      (map-set invites invite-hash
        {
          creator: tx-sender,
          tier: tier,
          uses-remaining: uses,
          expires-at: (+ stacks-block-height duration)
        }
      )
      
      (ok { invite-hash: invite-hash })
    )
    err-not-member
  )
)

(define-public (leave-dao)
  (match (map-get? members tx-sender)
    member
    (begin
      (map-set members tx-sender
        (merge member { active: false })
      )
      (var-set member-count (- (var-get member-count) u1))
      (ok { left: true })
    )
    err-not-member
  )
)

;; Read-only Functions

(define-read-only (get-member (account principal))
  (map-get? members account)
)

(define-read-only (get-member-stats (account principal))
  (default-to 
    { proposals-created: u0, votes-cast: u0, referrals: u0, reputation: u100 }
    (map-get? member-stats account)
  )
)

(define-read-only (get-tier-config (tier uint))
  (map-get? tier-configs tier)
)

(define-read-only (is-active-member (account principal))
  (match (map-get? members account)
    member (and (get active member) (>= (get expires-at member) stacks-block-height))
    false
  )
)

(define-read-only (get-voting-weight (account principal))
  (match (map-get? members account)
    member
    (if (and (get active member) (>= (get expires-at member) stacks-block-height))
      (match (map-get? tier-configs (get tier member))
        tier-config (get voting-weight tier-config)
        u0
      )
      u0
    )
    u0
  )
)

(define-read-only (get-referral-rewards (account principal))
  (default-to u0 (map-get? referral-rewards account))
)

(define-read-only (get-invite (invite-hash (buff 32)))
  (map-get? invites invite-hash)
)

(define-read-only (get-membership-status (account principal))
  (match (map-get? members account)
    member
    (if (not (get active member))
      "inactive"
      (if (< stacks-block-height (get expires-at member))
        "active"
        "expired"
      )
    )
    "not-member"
  )
)

(define-read-only (get-dao-stats)
  {
    total-members: (var-get member-count),
    total-fees-collected: (var-get total-fees-collected),
    membership-nonce: (var-get membership-nonce)
  }
)

(define-read-only (get-member-tier-info (account principal))
  (match (map-get? members account)
    member
    (match (map-get? tier-configs (get tier member))
      tier-config
      (ok {
        tier: (get tier member),
        tier-name: (get name tier-config),
        voting-weight: (get voting-weight tier-config),
        proposal-limit: (get proposal-limit tier-config),
        expires-at: (get expires-at member),
        days-remaining: (if (> (get expires-at member) stacks-block-height)
                          (/ (- (get expires-at member) stacks-block-height) u144)
                          u0)
      })
      err-invalid-tier
    )
    err-not-member
  )
)

;; Admin Functions

(define-public (update-tier-config (tier uint) (name (string-ascii 32)) (fee uint) (weight uint) (limit uint) (benefits (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set tier-configs tier
      { name: name, monthly-fee: fee, voting-weight: weight, proposal-limit: limit, benefits: benefits }
    )
    (ok true)
  )
)

(define-public (grant-membership (account principal) (tier uint) (duration uint))
  (let (
    (member-id (var-get membership-nonce))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set members account
      {
        member-id: member-id,
        tier: tier,
        joined-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        total-paid: u0,
        referrer: none,
        active: true
      }
    )
    
    (var-set membership-nonce (+ member-id u1))
    (var-set member-count (+ (var-get member-count) u1))
    
    (ok { member-id: member-id })
  )
)

(define-public (revoke-membership (account principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? members account)
      member
      (begin
        (map-set members account (merge member { active: false }))
        (var-set member-count (- (var-get member-count) u1))
        (ok true)
      )
      err-not-member
    )
  )
)

(define-public (withdraw-fees (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)
  )
)
