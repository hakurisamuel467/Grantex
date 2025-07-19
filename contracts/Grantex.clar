(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_GRANT_NOT_FOUND (err u101))
(define-constant ERR_MILESTONE_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u104))
(define-constant ERR_INVALID_MILESTONE (err u105))
(define-constant ERR_GRANT_ALREADY_EXISTS (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_MILESTONE_NOT_READY (err u108))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u109))
(define-constant ERR_VOTING_PERIOD_ENDED (err u110))
(define-constant ERR_VOTING_PERIOD_ACTIVE (err u111))
(define-constant ERR_ALREADY_VOTED (err u112))
(define-constant ERR_INSUFFICIENT_VOTING_POWER (err u113))
(define-constant ERR_PROPOSAL_NOT_APPROVED (err u114))
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED (err u115))
(define-constant ERR_INVALID_PROPOSAL_TYPE (err u116))
(define-constant ERR_DELEGATION_CYCLE (err u117))

(define-data-var next-grant-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var voting-period-blocks uint u144)
(define-data-var quorum-threshold uint u1000)
(define-data-var approval-threshold uint u6000)

(define-map grants
  { grant-id: uint }
  {
    researcher: principal,
    title: (string-ascii 100),
    total-amount: uint,
    released-amount: uint,
    created-at: uint,
    status: (string-ascii 20)
  }
)

(define-map milestones
  { grant-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    amount: uint,
    completed: bool,
    completed-at: (optional uint),
    reviewer: (optional principal)
  }
)

(define-map grant-milestone-count
  { grant-id: uint }
  { count: uint }
)

(define-map researcher-grants
  { researcher: principal }
  { grant-ids: (list 50 uint) }
)

(define-map authorized-reviewers
  { reviewer: principal }
  { authorized: bool }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    proposal-type: (string-ascii 20),
    target-researcher: principal,
    grant-title: (string-ascii 100),
    total-amount: uint,
    milestone-descriptions: (list 10 (string-ascii 200)),
    milestone-amounts: (list 10 uint),
    description: (string-ascii 500),
    created-at: uint,
    voting-end-block: uint,
    executed: bool,
    yes-votes: uint,
    no-votes: uint,
    total-voting-power: uint
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    vote-amount: uint,
    vote-choice: bool,
    voted-at: uint
  }
)

(define-map voting-power
  { holder: principal }
  {
    power: uint,
    delegated-to: (optional principal),
    delegated-power: uint,
    last-updated: uint
  }
)

(define-map delegation-history
  { delegator: principal, delegatee: principal }
  {
    delegated-amount: uint,
    delegation-block: uint,
    active: bool
  }
)

(define-map governance-stats
  { stat-type: (string-ascii 20) }
  {
    total-proposals: uint,
    executed-proposals: uint,
    total-participants: uint,
    total-voting-power: uint
  }
)

(define-public (create-grant 
  (researcher principal) 
  (title (string-ascii 100)) 
  (total-amount uint)
  (milestone-descriptions (list 10 (string-ascii 200)))
  (milestone-amounts (list 10 uint)))
  (let 
    (
      (grant-id (var-get next-grant-id))
      (milestone-count (len milestone-descriptions))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (fold + milestone-amounts u0) total-amount) ERR_INVALID_AMOUNT)
    
    (map-set grants
      { grant-id: grant-id }
      {
        researcher: researcher,
        title: title,
        total-amount: total-amount,
        released-amount: u0,
        created-at: stacks-block-height,
        status: "active"
      }
    )
    
    (map-set grant-milestone-count
      { grant-id: grant-id }
      { count: milestone-count }
    )
    
    ;; (try! (create-milestones grant-id milestone-descriptions milestone-amounts u1))
    (unwrap! (add-grant-to-researcher researcher grant-id) (err u102))
    
    (var-set next-grant-id (+ grant-id u1))
    (ok grant-id)
  )
)

;; (define-private (create-milestones
;;   (grant-id uint)
;;   (descriptions (list 10 (string-ascii 200)))
;;   (amounts (list 10 uint))
;;   (milestone-id uint))
;;   (let ((count (len descriptions)))
;;     (if (not (is-eq count (len amounts)))
;;       (err u999)
;;       (letrec
;;         (
;;           (create-milestones-loop (lambda (i)
;;             (if (>= i count)
;;               (ok true)
;;               (let (
;;                 (desc (unwrap-panic (element-at descriptions i)))
;;                 (amt (unwrap-panic (element-at amounts i)))
;;               )
;;                 (map-set milestones
;;                   { grant-id: grant-id, milestone-id: (+ milestone-id i) }
;;                   {
;;                     description: desc,
;;                     amount: amt,
;;                     completed: false,
;;                     completed-at: none,
;;                     reviewer: none
;;                   }
;;                 )
;;                 (create-milestones-loop (+ i u1))
;;               )
;;             )
;;           ))
;;         )
;;         (create-milestones-loop u0)
;;       )
;;     )
;;   )
;; )

(define-private (add-grant-to-researcher (researcher principal) (grant-id uint))
  (let 
    (
      (current-grants (default-to (list) (get grant-ids (map-get? researcher-grants { researcher: researcher }))))
    )
    (map-set researcher-grants
      { researcher: researcher }
      { grant-ids: (unwrap-panic (as-max-len? (append current-grants grant-id) u50)) }
    )
    (ok true)
  )
)

(define-public (authorize-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-reviewers
      { reviewer: reviewer }
      { authorized: true }
    )
    (ok true)
  )
)

(define-public (complete-milestone (grant-id uint) (milestone-id uint))
  (let 
    (
      (grant (unwrap! (map-get? grants { grant-id: grant-id }) ERR_GRANT_NOT_FOUND))
      (milestone (unwrap! (map-get? milestones { grant-id: grant-id, milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
      (is-authorized (default-to false (get authorized (map-get? authorized-reviewers { reviewer: tx-sender }))))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) is-authorized) ERR_UNAUTHORIZED)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    
    (map-set milestones
      { grant-id: grant-id, milestone-id: milestone-id }
      (merge milestone {
        completed: true,
        completed-at: (some stacks-block-height),
        reviewer: (some tx-sender)
      })
    )
    
    (try! (release-milestone-funds grant-id milestone-id))
    (ok true)
  )
)

(define-private (release-milestone-funds (grant-id uint) (milestone-id uint))
  (let 
    (
      (grant (unwrap! (map-get? grants { grant-id: grant-id }) ERR_GRANT_NOT_FOUND))
      (milestone (unwrap! (map-get? milestones { grant-id: grant-id, milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
      (milestone-amount (get amount milestone))
      (new-released-amount (+ (get released-amount grant) milestone-amount))
    )
    (map-set grants
      { grant-id: grant-id }
      (merge grant { released-amount: new-released-amount })
    )
    
    (try! (stx-transfer? milestone-amount tx-sender (get researcher grant)))
    (ok true)
  )
)

(define-public (fund-contract (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok true)
  )
)

(define-read-only (get-grant (grant-id uint))
  (map-get? grants { grant-id: grant-id })
)

(define-read-only (get-milestone (grant-id uint) (milestone-id uint))
  (map-get? milestones { grant-id: grant-id, milestone-id: milestone-id })
)

(define-read-only (get-researcher-grants (researcher principal))
  (map-get? researcher-grants { researcher: researcher })
)

(define-read-only (get-grant-progress (grant-id uint))
  (match (map-get? grants { grant-id: grant-id })
    grant
      (let 
        (
          (total-amount (get total-amount grant))
          (released-amount (get released-amount grant))
          (progress-percentage (if (> total-amount u0) (/ (* released-amount u100) total-amount) u0))
        )
        (some {
          total-amount: total-amount,
          released-amount: released-amount,
          remaining-amount: (- total-amount released-amount),
          progress-percentage: progress-percentage
        })
      )
    none
  )
)

(define-read-only (get-milestone-count (grant-id uint))
  (map-get? grant-milestone-count { grant-id: grant-id })
)

(define-read-only (is-reviewer-authorized (reviewer principal))
  (default-to false (get authorized (map-get? authorized-reviewers { reviewer: reviewer })))
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-next-grant-id)
  (var-get next-grant-id)
)

(define-public (update-grant-status (grant-id uint) (new-status (string-ascii 20)))
  (let 
    (
      (grant (unwrap! (map-get? grants { grant-id: grant-id }) ERR_GRANT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set grants
      { grant-id: grant-id }
      (merge grant { status: new-status })
    )
    (ok true)
  )
)

(define-public (allocate-voting-power (amount uint))
  (let 
    (
      (current-power (default-to { power: u0, delegated-to: none, delegated-power: u0, last-updated: u0 } 
                                 (map-get? voting-power { holder: tx-sender })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set voting-power
      { holder: tx-sender }
      {
        power: (+ (get power current-power) amount),
        delegated-to: (get delegated-to current-power),
        delegated-power: (get delegated-power current-power),
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (delegate-voting-power (delegatee principal) (amount uint))
  (let 
    (
      (delegator-power (unwrap! (map-get? voting-power { holder: tx-sender }) ERR_INSUFFICIENT_VOTING_POWER))
      (delegatee-power (default-to { power: u0, delegated-to: none, delegated-power: u0, last-updated: u0 } 
                                   (map-get? voting-power { holder: delegatee })))
      (available-power (get power delegator-power))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= available-power amount) ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! (not (is-eq tx-sender delegatee)) ERR_DELEGATION_CYCLE)
    
    (map-set voting-power
      { holder: tx-sender }
      (merge delegator-power { power: (- available-power amount) })
    )
    
    (map-set voting-power
      { holder: delegatee }
      (merge delegatee-power { delegated-power: (+ (get delegated-power delegatee-power) amount) })
    )
    
    (map-set delegation-history
      { delegator: tx-sender, delegatee: delegatee }
      {
        delegated-amount: amount,
        delegation-block: stacks-block-height,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (submit-grant-proposal 
  (target-researcher principal)
  (grant-title (string-ascii 100))
  (total-amount uint)
  (milestone-descriptions (list 10 (string-ascii 200)))
  (milestone-amounts (list 10 uint))
  (description (string-ascii 500)))
  (let 
    (
      (proposal-id (var-get next-proposal-id))
      (voting-end (+ stacks-block-height (var-get voting-period-blocks)))
      (voter-power (default-to { power: u0, delegated-to: none, delegated-power: u0, last-updated: u0 } 
                               (map-get? voting-power { holder: tx-sender })))
    )
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (+ (get power voter-power) (get delegated-power voter-power)) u100) ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (fold + milestone-amounts u0) total-amount) ERR_INVALID_AMOUNT)
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        proposal-type: "grant-funding",
        target-researcher: target-researcher,
        grant-title: grant-title,
        total-amount: total-amount,
        milestone-descriptions: milestone-descriptions,
        milestone-amounts: milestone-amounts,
        description: description,
        created-at: stacks-block-height,
        voting-end-block: voting-end,
        executed: false,
        yes-votes: u0,
        no-votes: u0,
        total-voting-power: u0
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (unwrap-panic (update-governance-stats "total-proposals" u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-choice bool))
  (let 
    (
      (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (voter-power (unwrap! (map-get? voting-power { holder: tx-sender }) ERR_INSUFFICIENT_VOTING_POWER))
      (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
      (vote-weight (+ (get power voter-power) (get delegated-power voter-power)))
      (current-yes (get yes-votes proposal))
      (current-no (get no-votes proposal))
    )
    (asserts! (> vote-weight u0) ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! (<= stacks-block-height (get voting-end-block proposal)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote-amount: vote-weight,
        vote-choice: vote-choice,
        voted-at: stacks-block-height
      }
    )
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      (merge proposal {
        yes-votes: (if vote-choice (+ current-yes vote-weight) current-yes),
        no-votes: (if vote-choice current-no (+ current-no vote-weight)),
        total-voting-power: (+ (get total-voting-power proposal) vote-weight)
      })
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let 
    (
      (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (total-votes (get total-voting-power proposal))
      (yes-votes (get yes-votes proposal))
      (approval-rate (if (> total-votes u0) (/ (* yes-votes u10000) total-votes) u0))
      (quorum-met (>= total-votes (var-get quorum-threshold)))
      (approval-met (>= approval-rate (var-get approval-threshold)))
    )
    (asserts! (> stacks-block-height (get voting-end-block proposal)) ERR_VOTING_PERIOD_ACTIVE)
    (asserts! (not (get executed proposal)) ERR_PROPOSAL_ALREADY_EXECUTED)
    (asserts! quorum-met ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! approval-met ERR_PROPOSAL_NOT_APPROVED)
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    (if (is-eq (get proposal-type proposal) "grant-funding")
      (begin
        (try! (create-grant 
          (get target-researcher proposal)
          (get grant-title proposal)
          (get total-amount proposal)
          (get milestone-descriptions proposal)
          (get milestone-amounts proposal)
        ))
        (unwrap-panic (update-governance-stats "executed-proposals" u1))
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (set-governance-parameter (param-type (string-ascii 20)) (new-value uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-value u0) ERR_INVALID_AMOUNT)
    
    (if (is-eq param-type "voting-period")
      (begin (var-set voting-period-blocks new-value) (ok true))
      (if (is-eq param-type "quorum")
        (begin (var-set quorum-threshold new-value) (ok true))
        (if (is-eq param-type "approval")
          (begin (var-set approval-threshold new-value) (ok true))
          ERR_INVALID_PROPOSAL_TYPE
        )
      )
    )
  )
)

(define-private (update-governance-stats (stat-type (string-ascii 20)) (increment uint))
  (let 
    (
      (current-stats (default-to { total-proposals: u0, executed-proposals: u0, total-participants: u0, total-voting-power: u0 } 
                                 (map-get? governance-stats { stat-type: stat-type })))
    )
    (begin
      (if (is-eq stat-type "total-proposals")
        (map-set governance-stats
          { stat-type: stat-type }
          (merge current-stats { total-proposals: (+ (get total-proposals current-stats) increment) })
        )
        (if (is-eq stat-type "executed-proposals")
          (map-set governance-stats
            { stat-type: stat-type }
            (merge current-stats { executed-proposals: (+ (get executed-proposals current-stats) increment) })
          )
          false
        )
      )
      (ok true)
    )
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (holder principal))
  (map-get? voting-power { holder: holder })
)

(define-read-only (get-effective-voting-power (holder principal))
  (match (map-get? voting-power { holder: holder })
    power-data
      (some (+ (get power power-data) (get delegated-power power-data)))
    none
  )
)

(define-read-only (get-delegation-info (delegator principal) (delegatee principal))
  (map-get? delegation-history { delegator: delegator, delegatee: delegatee })
)

(define-read-only (get-governance-stats (stat-type (string-ascii 20)))
  (map-get? governance-stats { stat-type: stat-type })
)

(define-read-only (get-governance-parameters)
  {
    voting-period-blocks: (var-get voting-period-blocks),
    quorum-threshold: (var-get quorum-threshold),
    approval-threshold: (var-get approval-threshold),
    next-proposal-id: (var-get next-proposal-id)
  }
)

(define-read-only (is-proposal-approved (proposal-id uint))
  (match (map-get? governance-proposals { proposal-id: proposal-id })
    proposal
      (let 
        (
          (total-votes (get total-voting-power proposal))
          (yes-votes (get yes-votes proposal))
          (approval-rate (if (> total-votes u0) (/ (* yes-votes u10000) total-votes) u0))
          (quorum-met (>= total-votes (var-get quorum-threshold)))
          (approval-met (>= approval-rate (var-get approval-threshold)))
        )
        (and quorum-met approval-met)
      )
    false
  )
)