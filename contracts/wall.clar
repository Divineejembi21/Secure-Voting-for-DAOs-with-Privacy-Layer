(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_DOES_NOT_EXIST (err u101))
(define-constant ERR_PROPOSAL_ALREADY_EXISTS (err u102))
(define-constant ERR_PROPOSAL_EXPIRED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_INVALID_VOTE (err u105))
(define-constant ERR_INVALID_PROOF (err u106))
(define-constant ERR_PROPOSAL_NOT_ENDED (err u107))
(define-constant ERR_INSUFFICIENT_TOKENS (err u108))

(define-data-var dao-admin principal tx-sender)
(define-data-var proposal-count uint u0)
(define-data-var min-voting-power uint u100)

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
    merkle-root: (buff 32)
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

(define-public (create-proposal (title (string-ascii 100)) (description (string-utf8 500)) (duration uint) (merkle-root (buff 32)))
  (let 
    (
      (proposal-id (+ (var-get proposal-count) u1))
      (user-voting-power (get amount (get-voting-power tx-sender)))
    )
    (asserts! (>= user-voting-power (var-get min-voting-power)) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-none (map-get? proposals { proposal-id: proposal-id })) ERR_PROPOSAL_ALREADY_EXISTS)
    
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
        merkle-root: merkle-root
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
      (user-voting-power (get amount (get-voting-power tx-sender)))
    )
    (asserts! (>= user-voting-power (var-get min-voting-power)) ERR_INSUFFICIENT_TOKENS)
    (asserts! (is-proposal-active proposal-id) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-none (get-vote-receipt proposal-id tx-sender)) ERR_ALREADY_VOTED)
    
    ;; In a real implementation, we would verify the ZK proof here
    ;; This is a simplified version that just checks the nullifier isn't empty
    (asserts! (not (is-eq nullifier 0x)) ERR_INVALID_PROOF)
    
    ;; Record the vote commitment
    (map-set vote-receipts
      { proposal-id: proposal-id, voter: tx-sender }
      { commitment-hash: commitment-hash, voted: true }
    )
    
    ;; Update vote counts
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
    )
    (asserts! (is-eq tx-sender (var-get dao-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status proposal) "passed") ERR_UNAUTHORIZED)
    
    ;; In a real implementation, this would execute the proposal's actions
    ;; For this MVP, we just mark it as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: "executed" })
    )
    
    (ok true)
  )
)