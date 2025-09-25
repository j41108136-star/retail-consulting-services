(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))

(define-data-var next-assessment-id uint u1)
(define-data-var next-strategy-id uint u1)

(define-map assessments
    { assessment-id: uint }
    {
        client: principal,
        consultant: principal,
        scope: (string-ascii 500),
        status: (string-ascii 20),
        created-at: uint,
        completed-at: (optional uint)
    }
)

(define-map strategies
    { strategy-id: uint }
    {
        assessment-id: uint,
        recommendations: (string-ascii 1000),
        implementation-timeline: uint,
        performance-metrics: (string-ascii 500),
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-map client-assessments
    { client: principal }
    { assessment-ids: (list 50 uint) }
)

(define-public (create-assessment (client principal) (scope (string-ascii 500)))
    (let
        (
            (assessment-id (var-get next-assessment-id))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set assessments
            { assessment-id: assessment-id }
            {
                client: client,
                consultant: tx-sender,
                scope: scope,
                status: "pending",
                created-at: stacks-block-height,
                completed-at: none
            }
        )
        (map-set client-assessments
            { client: client }
            {
                assessment-ids: (unwrap-panic (as-max-len? 
                    (append (default-to (list) (get assessment-ids (map-get? client-assessments { client: client }))) assessment-id) 
                    u50))
            }
        )
        (var-set next-assessment-id (+ assessment-id u1))
        (ok assessment-id)
    )
)

(define-public (update-assessment-status (assessment-id uint) (new-status (string-ascii 20)))
    (let
        (
            (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) ERR_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set assessments
            { assessment-id: assessment-id }
            (merge assessment { 
                status: new-status,
                completed-at: (if (is-eq new-status "completed") (some stacks-block-height) (get completed-at assessment))
            })
        )
        (ok true)
    )
)

(define-public (create-strategy (assessment-id uint) (recommendations (string-ascii 1000)) (timeline uint) (metrics (string-ascii 500)))
    (let
        (
            (strategy-id (var-get next-strategy-id))
            (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) ERR_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set strategies
            { strategy-id: strategy-id }
            {
                assessment-id: assessment-id,
                recommendations: recommendations,
                implementation-timeline: timeline,
                performance-metrics: metrics,
                status: "draft",
                created-at: stacks-block-height
            }
        )
        (var-set next-strategy-id (+ strategy-id u1))
        (ok strategy-id)
    )
)

(define-public (update-strategy-status (strategy-id uint) (new-status (string-ascii 20)))
    (let
        (
            (strategy (unwrap! (map-get? strategies { strategy-id: strategy-id }) ERR_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set strategies
            { strategy-id: strategy-id }
            (merge strategy { status: new-status })
        )
        (ok true)
    )
)

(define-read-only (get-assessment (assessment-id uint))
    (map-get? assessments { assessment-id: assessment-id })
)

(define-read-only (get-strategy (strategy-id uint))
    (map-get? strategies { strategy-id: strategy-id })
)

(define-read-only (get-client-assessments (client principal))
    (default-to (list) (get assessment-ids (map-get? client-assessments { client: client })))
)
