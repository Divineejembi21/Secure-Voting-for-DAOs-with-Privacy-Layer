(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_DOES_NOT_EXIST (err u101))
(define-constant ERR_PROPOSAL_ALREADY_EXISTS (err u102))
(define-constant ERR_PROPOSAL_EXPIRED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_INVALID_VOTE (err u105))
(define-constant ERR_SELF_DELEGATION (err u201))
(define-constant ERR_CIRCULAR_DELEGATION (err u202))
(define-constant ERR_NO_DELEGATION (err u203))
(define-constant ERR_INVALID_PROOF (err u106))
(define-constant ERR_PROPOSAL_NOT_ENDED (err u107))
(define-constant ERR_INSUFFICIENT_TOKENS (err u108))
(define-constant ERR_INSUFFICIENT_TREASURY (err u109))
(define-constant ERR_INVALID_BUDGET (err u110))
(define-constant ERR_BUDGET_EXCEEDED (err u111))
(define-constant ERR_SNAPSHOT_NOT_FOUND (err u112))
(define-constant ERR_INSUFFICIENT_CREDITS (err u114))
(define-constant ERR_INVALID_CREDIT_AMOUNT (err u115))
(define-constant ERR_CREDITS_ALREADY_ALLOCATED (err u116))
(define-constant ERR_MAX_CREDITS_EXCEEDED (err u117))

(define-data-var dao-admin principal tx-sender)
(define-data-var proposal-count uint u0)
(define-data-var min-voting-power uint u100)
(define-data-var treasury-balance uint u0)
(define-data-var total-allocated uint u0)
(define-data-var snapshot-count uint u0)
(define-data-var max-credits-per-proposal uint u100)

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-utf8 500),
    proposer: principal,
    start-stacks-block-height: uint,
    end-stacks-block-height: uint,
    yes-votes: uint,
    no-votes: uint,
    status: (string-ascii 20),
    merkle-root: (buff 32),
    category: (string-ascii 20),
    budget-requested: uint,
    budget-allocated: uint,
    snapshot-id: uint
  }
)

(define-map vote-receipts
  { proposal-id: uint, voter: principal }
  { commitment-hash: (buff 32), voted: bool }
)

(define-map voting-power
  { user: principal }
  { amount: uint }
)

(define-map snapshots
  { snapshot-id: uint }
  { block-height: uint, total-supply: uint }
)

(define-map snapshot-balances
  { snapshot-id: uint, user: principal }
  { voting-power: uint, delegated-power: uint }
)

;; Quadratic voting system maps
(define-map user-vote-credits
  { user: principal }
  { total-credits: uint, available-credits: uint }
)

(define-map proposal-credit-allocations
  { proposal-id: uint, user: principal }
  { credits-spent: uint, vote-weight: uint, vote-direction: bool }
)

(define-map quadratic-vote-totals
  { proposal-id: uint }
  { yes-weight: uint, no-weight: uint, total-participants: uint }
)

(define-read-only (get-dao-admin)
  (var-get dao-admin)
)

(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote-receipt (proposal-id uint) (voter principal))
  (map-get? vote-receipts { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (user principal))
  (default-to { amount: u0 } (map-get? voting-power { user: user }))
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-available-treasury)
  (- (var-get treasury-balance) (var-get total-allocated))
)

(define-read-only (get-total-allocated)
  (var-get total-allocated)
)

(define-read-only (get-snapshot (snapshot-id uint))
  (map-get? snapshots { snapshot-id: snapshot-id })
)

(define-read-only (get-snapshot-balance (snapshot-id uint) (user principal))
  (default-to 
    { voting-power: u0, delegated-power: u0 }
    (map-get? snapshot-balances { snapshot-id: snapshot-id, user: user })
  )
)

(define-read-only (get-snapshot-voting-power (snapshot-id uint) (user principal))
  (let (
    (snapshot-data (get-snapshot-balance snapshot-id user))
  )
    (+ (get voting-power snapshot-data) (get delegated-power snapshot-data))
  )
)

;; Quadratic voting read-only functions
(define-read-only (get-user-credits (user principal))
  (default-to 
    { total-credits: u0, available-credits: u0 }
    (map-get? user-vote-credits { user: user })
  )
)

(define-read-only (get-proposal-allocation (proposal-id uint) (user principal))
  (map-get? proposal-credit-allocations { proposal-id: proposal-id, user: user })
)

(define-read-only (get-quadratic-vote-totals (proposal-id uint))
  (default-to 
    { yes-weight: u0, no-weight: u0, total-participants: u0 }
    (map-get? quadratic-vote-totals { proposal-id: proposal-id })
  )
)

;; Calculate square root using Newton's method approximation
(define-private (sqrt-newton (n uint))
  (if (is-eq n u0)
    u0
    (if (is-eq n u1)
      u1
      (let (
        (x (/ n u2))
        (x1 (/ (+ x (/ n x)) u2))
        (x2 (/ (+ x1 (/ n x1)) u2))
        (x3 (/ (+ x2 (/ n x2)) u2))
        (x4 (/ (+ x3 (/ n x3)) u2))
      )
        x4
      )
    )
  )
)

;; Calculate quadratic vote weight (square root of credits)
(define-read-only (calculate-vote-weight (credits uint))
  (sqrt-newton credits)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (match (get-vote-receipt proposal-id voter)
    receipt (get voted receipt)
    false
  )
)

(define-read-only (is-proposal-active (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (and 
              (>= stacks-block-height (get start-stacks-block-height proposal))
              (<= stacks-block-height (get end-stacks-block-height proposal))
              (is-eq (get status proposal) "active"))
    false
  )
)

(define-read-only (is-proposal-ended (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (> stacks-block-height (get end-stacks-block-height proposal))
    false
  )
)

(define-public (set-dao-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (ok (var-set dao-admin new-admin))
  )
)

(define-public (set-min-voting-power (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (ok (var-set min-voting-power amount))
  )
)

;; Admin function to set maximum credits per proposal
(define-public (set-max-credits-per-proposal (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_CREDIT_AMOUNT)
    (ok (var-set max-credits-per-proposal amount))
  )
)

;; Allocate vote credits to users based on their voting power
(define-public (allocate-user-credits (user principal))
  (let (
    (user-power (get-weighted-voting-power user))
    (credits-to-allocate (/ user-power u10)) ;; 1 credit per 10 voting power
    (current-credits (get-user-credits user))
  )
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (> user-power u0) ERR_INSUFFICIENT_TOKENS)
    
    (map-set user-vote-credits
      { user: user }
      { 
        total-credits: (+ (get total-credits current-credits) credits-to-allocate),
        available-credits: (+ (get available-credits current-credits) credits-to-allocate)
      }
    )
    
    (ok credits-to-allocate)
  )
)

(define-public (add-voting-power (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (map-set voting-power 
      { user: user } 
      { amount: (+ (get amount (get-voting-power user)) amount) }
    )
    (ok true)
  )
)

(define-public (fund-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_BUDGET)
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-public (create-snapshot)
  (let (
    (snapshot-id (+ (var-get snapshot-count) u1))
    (current-height stacks-block-height)
  )
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    
    (map-set snapshots
      { snapshot-id: snapshot-id }
      { block-height: current-height, total-supply: u0 }
    )
    
    (var-set snapshot-count snapshot-id)
    (ok snapshot-id)
  )
)

(define-public (record-snapshot-balance (snapshot-id uint) (user principal))
  (let (
    (current-power (get-weighted-voting-power user))
    (current-delegated (get total-power (get-delegated-power user)))
  )
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-some (get-snapshot snapshot-id)) ERR_SNAPSHOT_NOT_FOUND)
    
    (map-set snapshot-balances
      { snapshot-id: snapshot-id, user: user }
      { voting-power: current-power, delegated-power: current-delegated }
    )
    
    (ok true)
  )
)

(define-public (withdraw-treasury (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_BUDGET)
    (asserts! (<= amount (get-available-treasury)) ERR_INSUFFICIENT_TREASURY)
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-public (create-proposal-with-budget (title (string-ascii 100)) (description (string-utf8 500)) (duration uint) (merkle-root (buff 32)) (category (string-ascii 20)) (budget-requested uint))
  (let 
    (
      (proposal-id (+ (var-get proposal-count) u1))
      (user-voting-power (get amount (get-voting-power tx-sender)))
      (snapshot-id (+ (var-get snapshot-count) u1))
    )
    (asserts! (>= user-voting-power (var-get min-voting-power)) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-none (map-get? proposals { proposal-id: proposal-id })) ERR_PROPOSAL_ALREADY_EXISTS)
    (asserts! (<= budget-requested (get-available-treasury)) ERR_INSUFFICIENT_TREASURY)
    
    (map-set snapshots
      { snapshot-id: snapshot-id }
      { block-height: stacks-block-height, total-supply: u0 }
    )
    
    (var-set snapshot-count snapshot-id)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        start-stacks-block-height: stacks-block-height,
        end-stacks-block-height: (+ stacks-block-height duration),
        yes-votes: u0,
        no-votes: u0,
        status: "active",
        merkle-root: merkle-root,
        category: category,
        budget-requested: budget-requested,
        budget-allocated: u0,
        snapshot-id: snapshot-id
      }
    )
    
    (var-set proposal-count proposal-id)
    (ok proposal-id)
  )
)

(define-public (cast-vote (proposal-id uint) (vote-value bool) (commitment-hash (buff 32)) (nullifier (buff 32)) (proof (buff 512)))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
      (snapshot-id (get snapshot-id proposal))
      (user-snapshot-power (get-snapshot-voting-power snapshot-id tx-sender))
    )
    (asserts! (>= user-snapshot-power (var-get min-voting-power)) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-proposal-active proposal-id) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-none (get-vote-receipt proposal-id tx-sender)) ERR_ALREADY_VOTED)
    (asserts! (not (is-eq nullifier 0x)) ERR_INVALID_PROOF)
    
    (map-set vote-receipts
      { proposal-id: proposal-id, voter: tx-sender }
      { commitment-hash: commitment-hash, voted: true }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal 
        {
          yes-votes: (if vote-value (+ (get yes-votes proposal) u1) (get yes-votes proposal)),
          no-votes: (if vote-value (get no-votes proposal) (+ (get no-votes proposal) u1))
        }
      )
    )
    
    (ok true)
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
    )
    (asserts! (is-proposal-ended proposal-id) ERR_PROPOSAL_NOT_ENDED)
    (asserts! (is-eq (get status proposal) "active") ERR_PROPOSAL_EXPIRED)
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal 
        {
          status: (if (> (get yes-votes proposal) (get no-votes proposal)) "passed" "rejected")
        }
      )
    )
    
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
      (budget-requested (get budget-requested proposal))
    )
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status proposal) "passed") ERR_UNAUTHORIZED)
    (asserts! (<= budget-requested (get-available-treasury)) ERR_INSUFFICIENT_TREASURY)
    
    (var-set total-allocated (+ (var-get total-allocated) budget-requested))
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { 
        status: "executed",
        budget-allocated: budget-requested
      })
    )
    
    (ok true)
  )
)

(define-public (release-allocated-budget (proposal-id uint) (recipient principal))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
      (allocated-amount (get budget-allocated proposal))
    )
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status proposal) "executed") ERR_UNAUTHORIZED)
    (asserts! (> allocated-amount u0) ERR_INVALID_BUDGET)
    
    (var-set treasury-balance (- (var-get treasury-balance) allocated-amount))
    (var-set total-allocated (- (var-get total-allocated) allocated-amount))
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { 
        status: "completed",
        budget-allocated: u0
      })
    )
    
    (ok allocated-amount)
  )
)



(define-constant ERR_INVALID_LOCK_TIME (err u113))

(define-map token-lock-time 
  { user: principal }
  { lock-height: uint }
)

(define-read-only (get-time-multiplier (blocks uint))
  (if (< blocks u10000) 
    u1
    (if (< blocks u50000)
      u2 
      u3
    )
  )
)

(define-public (lock-tokens (duration uint))
  (let (
    (current-height stacks-block-height)
    (end-height (+ current-height duration))
  )
    (asserts! (>= duration u10000) ERR_INVALID_LOCK_TIME)
    (map-set token-lock-time
      { user: tx-sender }
      { lock-height: end-height }
    )
    (ok true)
  )
)

(define-read-only (get-weighted-voting-power (user principal))
  (let (
    (base-power (get amount (get-voting-power user)))
    (lock-data (default-to { lock-height: u0 } (map-get? token-lock-time { user: user })))
    (blocks-locked (- (get lock-height lock-data) stacks-block-height))
    (multiplier (get-time-multiplier blocks-locked))
  )
    (* base-power multiplier)
  )
)



(define-constant CATEGORY_GENERAL "general")
(define-constant CATEGORY_CRITICAL "critical")
(define-constant CATEGORY_MINOR "minor")

(define-map category-thresholds
  { category: (string-ascii 20) }
  { 
    min-votes: uint,
    approval-percentage: uint
  }
)

(define-public (set-category-threshold (category (string-ascii 20)) (min-votes uint) (approval-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (map-set category-thresholds
      { category: category }
      { 
        min-votes: min-votes,
        approval-percentage: approval-percentage
      }
    )
    (ok true)
  )
)

(define-read-only (get-category-threshold (category (string-ascii 20)))
  (default-to 
    { min-votes: u100, approval-percentage: u51 }
    (map-get? category-thresholds { category: category })
  )
)

(define-public (create-categorized-proposal 
    (title (string-ascii 100)) 
    (description (string-utf8 500)) 
    (duration uint) 
    (merkle-root (buff 32))
    (category (string-ascii 20))
  )
  (let 
    (
      (proposal-id (+ (var-get proposal-count) u1))
      (user-voting-power (get amount (get-voting-power tx-sender)))
      (snapshot-id (+ (var-get snapshot-count) u1))
    )
    (asserts! (>= user-voting-power (var-get min-voting-power)) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-none (map-get? proposals { proposal-id: proposal-id })) ERR_PROPOSAL_ALREADY_EXISTS)
    
    (map-set snapshots
      { snapshot-id: snapshot-id }
      { block-height: stacks-block-height, total-supply: u0 }
    )
    
    (var-set snapshot-count snapshot-id)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        start-stacks-block-height: stacks-block-height,
        end-stacks-block-height: (+ stacks-block-height duration),
        yes-votes: u0,
        no-votes: u0,
        status: "active",
        merkle-root: merkle-root,
        category: category,
        budget-requested: u0,
        budget-allocated: u0,
        snapshot-id: snapshot-id
      }
    )
    
    (var-set proposal-count proposal-id)
    (ok proposal-id)
  )
)



(define-map delegations
  { delegator: principal }
  { delegate: principal, delegated-at: uint }
)

(define-map delegation-counts
  { delegate: principal }
  { count: uint }
)

(define-map delegated-power
  { delegate: principal }
  { total-power: uint }
)

(define-read-only (get-delegation (delegator principal))
  (map-get? delegations { delegator: delegator })
)

(define-read-only (get-delegation-count (delegate principal))
  (default-to { count: u0 } (map-get? delegation-counts { delegate: delegate }))
)

(define-read-only (get-delegated-power (delegate principal))
  (default-to { total-power: u0 } (map-get? delegated-power { delegate: delegate }))
)

(define-read-only (get-effective-voting-power (user principal))
  (let (
    (base-power (get-weighted-voting-power user))
    (delegated (get total-power (get-delegated-power user)))
  )
    (+ base-power delegated)
  )
)

(define-read-only (is-delegating (user principal))
  (is-some (get-delegation user))
)

(define-private (check-circular-delegation (delegator principal) (potential-delegate principal))
  (let (
    (delegate-delegation (get-delegation potential-delegate))
  )
    (match delegate-delegation
      delegation-info 
        (not (is-eq (get delegate delegation-info) delegator))
      true
    )
  )
)

(define-public (delegate-voting-power (delegate principal))
  (let (
    (delegator-power (get-weighted-voting-power tx-sender))
    (current-delegated (get total-power (get-delegated-power delegate)))
    (current-count (get count (get-delegation-count delegate)))
    (existing-delegation (get-delegation tx-sender))
  )
    (asserts! (not (is-eq tx-sender delegate)) ERR_SELF_DELEGATION)
    (asserts! (check-circular-delegation tx-sender delegate) ERR_CIRCULAR_DELEGATION)
    
    (match existing-delegation
      old-delegation
        (let (
          (old-delegate (get delegate old-delegation))
          (old-delegate-power (get total-power (get-delegated-power old-delegate)))
          (old-delegate-count (get count (get-delegation-count old-delegate)))
        )
          (map-set delegated-power
            { delegate: old-delegate }
            { total-power: (- old-delegate-power delegator-power) }
          )
          (map-set delegation-counts
            { delegate: old-delegate }
            { count: (- old-delegate-count u1) }
          )
        )
      true
    )
    
    (map-set delegations
      { delegator: tx-sender }
      { delegate: delegate, delegated-at: stacks-block-height }
    )
    
    (map-set delegated-power
      { delegate: delegate }
      { total-power: (+ current-delegated delegator-power) }
    )
    
    (map-set delegation-counts
      { delegate: delegate }
      { count: (+ current-count u1) }
    )
    
    (ok true)
  )
)

(define-public (revoke-delegation)
  (let (
    (delegation-info (unwrap! (get-delegation tx-sender) ERR_NO_DELEGATION))
    (delegate (get delegate delegation-info))
    (delegator-power (get-weighted-voting-power tx-sender))
    (current-delegated (get total-power (get-delegated-power delegate)))
    (current-count (get count (get-delegation-count delegate)))
  )
    (map-delete delegations { delegator: tx-sender })
    
    (map-set delegated-power
      { delegate: delegate }
      { total-power: (- current-delegated delegator-power) }
    )
    
    (map-set delegation-counts
      { delegate: delegate }
      { count: (- current-count u1) }
    )
    
    (ok true)
  )
)

(define-public (vote-as-delegate (proposal-id uint) (vote-value bool) (commitment-hash (buff 32)) (nullifier (buff 32)) (proof (buff 512)))
  (let (
    (effective-power (get-effective-voting-power tx-sender))
    (min-power (get-min-voting-power))
  )
    (asserts! (>= effective-power min-power) (err u108))
    (cast-vote proposal-id vote-value commitment-hash nullifier proof)
  )
)

(define-read-only (get-min-voting-power)
  (var-get min-voting-power)
)

(define-public (cast-vote-with-delegation (proposal-id uint) (vote-value bool) (commitment-hash (buff 32)) (nullifier (buff 32)) (proof (buff 512)))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
      (snapshot-id (get snapshot-id proposal))
      (snapshot-power (get-snapshot-voting-power snapshot-id tx-sender))
    )
    (asserts! (>= snapshot-power (var-get min-voting-power)) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-proposal-active proposal-id) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-none (get-vote-receipt proposal-id tx-sender)) ERR_ALREADY_VOTED)
    (asserts! (not (is-eq nullifier 0x)) ERR_INVALID_PROOF)
    
    (map-set vote-receipts
      { proposal-id: proposal-id, voter: tx-sender }
      { commitment-hash: commitment-hash, voted: true }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal 
        {
          yes-votes: (if vote-value (+ (get yes-votes proposal) u1) (get yes-votes proposal)),
          no-votes: (if vote-value (get no-votes proposal) (+ (get no-votes proposal) u1))
        }
      )
    )
    
    (ok true)
  )
)

;; Core quadratic voting function
(define-public (cast-quadratic-vote (proposal-id uint) (vote-value bool) (credits-to-spend uint) (commitment-hash (buff 32)) (nullifier (buff 32)) (proof (buff 512)))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
    (user-credits (get-user-credits tx-sender))
    (available-credits (get available-credits user-credits))
    (vote-weight (calculate-vote-weight credits-to-spend))
    (current-totals (get-quadratic-vote-totals proposal-id))
    (existing-allocation (get-proposal-allocation proposal-id tx-sender))
  )
    ;; Validation checks
    (asserts! (is-proposal-active proposal-id) ERR_PROPOSAL_EXPIRED)
    (asserts! (> credits-to-spend u0) ERR_INVALID_CREDIT_AMOUNT)
    (asserts! (<= credits-to-spend (var-get max-credits-per-proposal)) ERR_MAX_CREDITS_EXCEEDED)
    (asserts! (<= credits-to-spend available-credits) ERR_INSUFFICIENT_CREDITS)
    (asserts! (is-none existing-allocation) ERR_CREDITS_ALREADY_ALLOCATED)
    (asserts! (not (is-eq nullifier 0x)) ERR_INVALID_PROOF)
    
    ;; Record the vote commitment for privacy
    (map-set vote-receipts
      { proposal-id: proposal-id, voter: tx-sender }
      { commitment-hash: commitment-hash, voted: true }
    )
    
    ;; Record credit allocation
    (map-set proposal-credit-allocations
      { proposal-id: proposal-id, user: tx-sender }
      { 
        credits-spent: credits-to-spend,
        vote-weight: vote-weight,
        vote-direction: vote-value
      }
    )
    
    ;; Update user's available credits
    (map-set user-vote-credits
      { user: tx-sender }
      (merge user-credits {
        available-credits: (- available-credits credits-to-spend)
      })
    )
    
    ;; Update proposal totals with quadratic weights
    (map-set quadratic-vote-totals
      { proposal-id: proposal-id }
      {
        yes-weight: (if vote-value 
                      (+ (get yes-weight current-totals) vote-weight)
                      (get yes-weight current-totals)),
        no-weight: (if vote-value 
                     (get no-weight current-totals)
                     (+ (get no-weight current-totals) vote-weight)),
        total-participants: (+ (get total-participants current-totals) u1)
      }
    )
    
    (ok vote-weight)
  )
)

;; Allow users to increase their vote on existing allocation
(define-public (increase-quadratic-vote (proposal-id uint) (additional-credits uint) (commitment-hash (buff 32)) (nullifier (buff 32)) (proof (buff 512)))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_DOES_NOT_EXIST))
    (user-credits (get-user-credits tx-sender))
    (available-credits (get available-credits user-credits))
    (existing-allocation (unwrap! (get-proposal-allocation proposal-id tx-sender) ERR_CREDITS_ALREADY_ALLOCATED))
    (current-credits (get credits-spent existing-allocation))
    (new-total-credits (+ current-credits additional-credits))
    (new-vote-weight (calculate-vote-weight new-total-credits))
    (old-vote-weight (get vote-weight existing-allocation))
    (vote-direction (get vote-direction existing-allocation))
    (current-totals (get-quadratic-vote-totals proposal-id))
  )
    ;; Validation checks
    (asserts! (is-proposal-active proposal-id) ERR_PROPOSAL_EXPIRED)
    (asserts! (> additional-credits u0) ERR_INVALID_CREDIT_AMOUNT)
    (asserts! (<= new-total-credits (var-get max-credits-per-proposal)) ERR_MAX_CREDITS_EXCEEDED)
    (asserts! (<= additional-credits available-credits) ERR_INSUFFICIENT_CREDITS)
    (asserts! (not (is-eq nullifier 0x)) ERR_INVALID_PROOF)
    
    ;; Update vote commitment
    (map-set vote-receipts
      { proposal-id: proposal-id, voter: tx-sender }
      { commitment-hash: commitment-hash, voted: true }
    )
    
    ;; Update credit allocation
    (map-set proposal-credit-allocations
      { proposal-id: proposal-id, user: tx-sender }
      { 
        credits-spent: new-total-credits,
        vote-weight: new-vote-weight,
        vote-direction: vote-direction
      }
    )
    
    ;; Update user's available credits
    (map-set user-vote-credits
      { user: tx-sender }
      (merge user-credits {
        available-credits: (- available-credits additional-credits)
      })
    )
    
    ;; Update proposal totals (remove old weight, add new weight)
    (map-set quadratic-vote-totals
      { proposal-id: proposal-id }
      {
        yes-weight: (if vote-direction 
                      (+ (- (get yes-weight current-totals) old-vote-weight) new-vote-weight)
                      (get yes-weight current-totals)),
        no-weight: (if vote-direction 
                     (get no-weight current-totals)
                     (+ (- (get no-weight current-totals) old-vote-weight) new-vote-weight)),
        total-participants: (get total-participants current-totals)
      }
    )
    
    (ok new-vote-weight)
  )
)



