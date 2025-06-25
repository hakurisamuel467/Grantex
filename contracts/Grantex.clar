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

(define-data-var next-grant-id uint u1)

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