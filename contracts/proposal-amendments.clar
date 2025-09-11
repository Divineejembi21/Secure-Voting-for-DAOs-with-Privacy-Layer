;; Proposal Amendment System Contract
;; Enables controlled amendments to active proposals while preserving vote privacy

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u301))
(define-constant ERR_AMENDMENT_NOT_FOUND (err u302))
(define-constant ERR_AMENDMENT_ALREADY_EXISTS (err u303))
(define-constant ERR_INSUFFICIENT_SUPPORT (err u304))
(define-constant ERR_AMENDMENT_PERIOD_ENDED (err u305))
(define-constant ERR_INVALID_AMENDMENT_TYPE (err u306))
(define-constant ERR_TOO_MANY_AMENDMENTS (err u307))
(define-constant ERR_ALREADY_VOTED (err u308))

;; Data variables
(define-data-var amendment-id-nonce uint u1)
(define-data-var min-amendment-support uint u25) ;; 25% support needed to propose amendment
(define-data-var amendment-voting-period uint u1440) ;; ~24 hours in blocks

;; Amendment types
(define-constant AMENDMENT_TYPE_DESCRIPTION "description")
(define-constant AMENDMENT_TYPE_BUDGET "budget")
(define-constant AMENDMENT_TYPE_DURATION "duration")

;; Amendment proposals for active proposals
(define-map proposal-amendments
  uint ;; amendment-id
  {
    amendment-id: uint,
    target-proposal-id: uint,
    proposer: principal,
    amendment-type: (string-ascii 20),
    original-value: (string-utf8 500),
    proposed-value: (string-utf8 500),
    justification: (string-utf8 300),
    support-votes: uint,
    opposition-votes: uint,
    submission-height: uint,
    deadline-height: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected", "expired"
    applied: bool
  }
)

;; Track amendment support votes
(define-map amendment-supporters
  {amendment-id: uint, voter: principal}
  {
    support: bool,
    voting-power: uint,
    voted-at: uint
  }
)

;; Track amendment count per proposal to prevent spam
(define-map proposal-amendment-count
  uint ;; proposal-id
  {count: uint}
)

;; Track which amendments have been applied to proposals
(define-map applied-amendments
  uint ;; proposal-id
  {
    amendments: (list 5 uint),
    last-amended: uint
  }
)

;; Submit amendment proposal for an active proposal
(define-public (submit-amendment
  (target-proposal-id uint)
  (amendment-type (string-ascii 20))
  (original-value (string-utf8 500))
  (proposed-value (string-utf8 500))
  (justification (string-utf8 300)))
  (let
    (
      (amendment-id (var-get amendment-id-nonce))
      (current-height stacks-block-height)
      (deadline-height (+ current-height (var-get amendment-voting-period)))
      (proposal-count (default-to {count: u0} (map-get? proposal-amendment-count target-proposal-id)))
    )
    ;; Validate amendment type
    (asserts! (or (is-eq amendment-type AMENDMENT_TYPE_DESCRIPTION)
                  (or (is-eq amendment-type AMENDMENT_TYPE_BUDGET)
                      (is-eq amendment-type AMENDMENT_TYPE_DURATION))) ERR_INVALID_AMENDMENT_TYPE)
    
    ;; Check amendment limits per proposal
    (asserts! (< (get count proposal-count) u5) ERR_TOO_MANY_AMENDMENTS)
    
    ;; Create amendment proposal
    (map-set proposal-amendments amendment-id
      {
        amendment-id: amendment-id,
        target-proposal-id: target-proposal-id,
        proposer: tx-sender,
        amendment-type: amendment-type,
        original-value: original-value,
        proposed-value: proposed-value,
        justification: justification,
        support-votes: u0,
        opposition-votes: u0,
        submission-height: current-height,
        deadline-height: deadline-height,
        status: "pending",
        applied: false
      }
    )
    
    ;; Update amendment count for proposal
    (map-set proposal-amendment-count target-proposal-id
      {count: (+ (get count proposal-count) u1)}
    )
    
    (var-set amendment-id-nonce (+ amendment-id u1))
    (ok amendment-id)
  )
)

;; Vote on amendment proposal
(define-public (vote-on-amendment (amendment-id uint) (support bool))
  (let
    (
      (amendment (unwrap! (map-get? proposal-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (voter-power (contract-call? .wall get-effective-voting-power tx-sender))
      (current-height stacks-block-height)
      (support-key {amendment-id: amendment-id, voter: tx-sender})
    )
    ;; Check if amendment is still open for voting
    (asserts! (< current-height (get deadline-height amendment)) ERR_AMENDMENT_PERIOD_ENDED)
    (asserts! (is-eq (get status amendment) "pending") ERR_AMENDMENT_PERIOD_ENDED)
    
    ;; Check if already voted
    (asserts! (is-none (map-get? amendment-supporters support-key)) ERR_ALREADY_VOTED)
    
    ;; Record vote
    (map-set amendment-supporters support-key
      {
        support: support,
        voting-power: voter-power,
        voted-at: current-height
      }
    )
    
    ;; Update amendment vote counts
    (map-set proposal-amendments amendment-id
      (merge amendment {
        support-votes: (if support 
                        (+ (get support-votes amendment) voter-power)
                        (get support-votes amendment)),
        opposition-votes: (if support
                           (get opposition-votes amendment)
                           (+ (get opposition-votes amendment) voter-power))
      })
    )
    
    (ok true)
  )
)

;; Finalize amendment voting
(define-public (finalize-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? proposal-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (current-height stacks-block-height)
      (support-votes (get support-votes amendment))
      (opposition-votes (get opposition-votes amendment))
      (total-votes (+ support-votes opposition-votes))
      (min-support-threshold (var-get min-amendment-support))
    )
    ;; Check if voting period has ended
    (asserts! (>= current-height (get deadline-height amendment)) ERR_AMENDMENT_NOT_FOUND)
    (asserts! (is-eq (get status amendment) "pending") ERR_AMENDMENT_PERIOD_ENDED)
    
    ;; Determine if amendment passes
    (let
      (
        (support-percentage (if (> total-votes u0) (/ (* support-votes u100) total-votes) u0))
        (amendment-approved (>= support-percentage min-support-threshold))
        (new-status (if amendment-approved "approved" "rejected"))
      )
      
      ;; Update amendment status
      (map-set proposal-amendments amendment-id
        (merge amendment {status: new-status})
      )
      
      (ok amendment-approved)
    )
  )
)

;; Apply approved amendment to target proposal
(define-public (apply-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? proposal-amendments amendment-id) ERR_AMENDMENT_NOT_FOUND))
      (target-proposal-id (get target-proposal-id amendment))
      (applied-list (default-to {amendments: (list), last-amended: u0} 
                      (map-get? applied-amendments target-proposal-id)))
    )
    ;; Only DAO admin can apply approved amendments
    (asserts! (is-eq tx-sender (contract-call? .wall get-dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status amendment) "approved") ERR_INSUFFICIENT_SUPPORT)
    (asserts! (not (get applied amendment)) ERR_AMENDMENT_ALREADY_EXISTS)
    
    ;; Mark amendment as applied
    (map-set proposal-amendments amendment-id
      (merge amendment {applied: true})
    )
    
    ;; Track applied amendments
    (map-set applied-amendments target-proposal-id
      {
        amendments: (unwrap-panic (as-max-len? 
          (append (get amendments applied-list) amendment-id) u5)),
        last-amended: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Admin function to set minimum amendment support threshold
(define-public (set-min-amendment-support (percentage uint))
  (begin
    (asserts! (is-eq tx-sender (contract-call? .wall get-dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (and (> percentage u0) (<= percentage u100)) ERR_INSUFFICIENT_SUPPORT)
    (ok (var-set min-amendment-support percentage))
  )
)

;; Admin function to set amendment voting period
(define-public (set-amendment-voting-period (blocks uint))
  (begin
    (asserts! (is-eq tx-sender (contract-call? .wall get-dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (> blocks u0) ERR_INVALID_AMENDMENT_TYPE)
    (ok (var-set amendment-voting-period blocks))
  )
)

;; Read-only functions

(define-read-only (get-amendment (amendment-id uint))
  (map-get? proposal-amendments amendment-id)
)

(define-read-only (get-amendment-vote (amendment-id uint) (voter principal))
  (map-get? amendment-supporters {amendment-id: amendment-id, voter: voter})
)

(define-read-only (get-proposal-amendments (proposal-id uint))
  (map-get? applied-amendments proposal-id)
)

(define-read-only (get-amendment-count (proposal-id uint))
  (default-to {count: u0} (map-get? proposal-amendment-count proposal-id))
)

(define-read-only (is-amendment-active (amendment-id uint))
  (match (map-get? proposal-amendments amendment-id)
    amendment (and
                (is-eq (get status amendment) "pending")
                (< stacks-block-height (get deadline-height amendment)))
    false
  )
)

(define-read-only (get-amendment-support-percentage (amendment-id uint))
  (match (map-get? proposal-amendments amendment-id)
    amendment (let
                (
                  (support (get support-votes amendment))
                  (opposition (get opposition-votes amendment))
                  (total (+ support opposition))
                )
                (if (> total u0) 
                  (/ (* support u100) total)
                  u0))
    u0
  )
)

(define-read-only (get-min-amendment-support)
  (var-get min-amendment-support)
)

(define-read-only (get-amendment-voting-period)
  (var-get amendment-voting-period)
)

(define-read-only (get-last-amendment-id)
  (- (var-get amendment-id-nonce) u1)
)
