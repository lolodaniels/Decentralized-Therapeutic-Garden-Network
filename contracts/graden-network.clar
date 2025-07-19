;; Decentralized Therapeutic Garden Network - Core Garden Management
;; Manages garden facilities, maintenance, volunteers, and wellness impact tracking

;; Error codes
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-INPUT (err u400))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INSUFFICIENT-BALANCE (err u402))
(define-constant ERR-GARDEN-INACTIVE (err u410))

;; Garden status constants
(define-constant GARDEN-ACTIVE u1)
(define-constant GARDEN-MAINTENANCE u2)
(define-constant GARDEN-INACTIVE u3)

;; Garden types
(define-constant TYPE-SENSORY u1)
(define-constant TYPE-HEALING u2)
(define-constant TYPE-MEDITATION u3)
(define-constant TYPE-REHABILITATION u4)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Garden registry
(define-map gardens
  { garden-id: uint }
  {
    facility-id: (string-ascii 50),
    name: (string-ascii 100),
    location: (string-ascii 200),
    garden-type: uint,
    capacity: uint,
    status: uint,
    manager: principal,
    created-at: uint,
    maintenance-schedule: (list 12 uint), ;; Monthly maintenance blocks
    accessibility-features: (list 10 (string-ascii 50)),
    therapeutic-elements: (list 20 (string-ascii 50))
  }
)

;; Garden maintenance records
(define-map maintenance-records
  { garden-id: uint, record-id: uint }
  {
    maintenance-type: (string-ascii 50),
    performed-by: principal,
    completed-at: uint,
    next-due: uint,
    notes: (string-ascii 500),
    cost: uint,
    quality-rating: uint ;; 1-10 scale
  }
)

;; Volunteer registry
(define-map volunteers
  { volunteer-id: principal }
  {
    name: (string-ascii 100),
    skills: (list 10 (string-ascii 50)),
    certifications: (list 5 (string-ascii 100)),
    availability: (list 7 uint), ;; Days of week (0-6)
    hours-contributed: uint,
    assigned-gardens: (list 5 uint),
    rating: uint, ;; 1-100 scale
    background-check: bool,
    active-since: uint
  }
)

;; Wellness impact metrics
(define-map wellness-metrics
  { garden-id: uint, period: uint } ;; Period as block height ranges
  {
    patient-visits: uint,
    avg-session-duration: uint,
    stress-reduction-avg: uint, ;; 1-100 scale
    mobility-improvement-avg: uint, ;; 1-100 scale
    mood-improvement-avg: uint, ;; 1-100 scale
    social-interaction-score: uint, ;; 1-100 scale
    therapeutic-goals-met: uint,
    total-therapeutic-goals: uint,
    evidence-based-outcomes: (list 10 uint) ;; Various standardized metrics
  }
)

;; Garden counter
(define-data-var next-garden-id uint u1)
(define-data-var next-maintenance-id uint u1)

;; Register a new therapeutic garden
(define-public (register-garden
  (facility-id (string-ascii 50))
  (name (string-ascii 100))
  (location (string-ascii 200))
  (garden-type uint)
  (capacity uint)
  (accessibility-features (list 10 (string-ascii 50)))
  (therapeutic-elements (list 20 (string-ascii 50)))
)
  (let
    (
      (garden-id (var-get next-garden-id))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (var-get contract-owner))
                  (is-facility-manager tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (and (> capacity u0) (<= garden-type u4) (>= garden-type u1)) ERR-INVALID-INPUT)

    (map-set gardens
      { garden-id: garden-id }
      {
        facility-id: facility-id,
        name: name,
        location: location,
        garden-type: garden-type,
        capacity: capacity,
        status: GARDEN-ACTIVE,
        manager: tx-sender,
        created-at: current-block,
        maintenance-schedule: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0 u0),
        accessibility-features: accessibility-features,
        therapeutic-elements: therapeutic-elements
      }
    )

    (var-set next-garden-id (+ garden-id u1))
    (ok garden-id)
  )
)

;; Register volunteer
(define-public (register-volunteer
  (name (string-ascii 100))
  (skills (list 10 (string-ascii 50)))
  (certifications (list 5 (string-ascii 100)))
  (availability (list 7 uint))
)
  (let ((current-block stacks-block-height))
    (map-set volunteers
      { volunteer-id: tx-sender }
      {
        name: name,
        skills: skills,
        certifications: certifications,
        availability: availability,
        hours-contributed: u0,
        assigned-gardens: (list),
        rating: u50, ;; Start with middle rating
        background-check: false,
        active-since: current-block
      }
    )
    (ok tx-sender)
  )
)

;; Assign volunteer to garden
(define-public (assign-volunteer (volunteer-id principal) (garden-id uint))
  (let
    (
      (garden (unwrap! (map-get? gardens { garden-id: garden-id }) ERR-NOT-FOUND))
      (volunteer (unwrap! (map-get? volunteers { volunteer-id: volunteer-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get manager garden)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status garden) GARDEN-ACTIVE) ERR-GARDEN-INACTIVE)
    (asserts! (get background-check volunteer) ERR-UNAUTHORIZED)

    (let ((updated-gardens (unwrap! (as-max-len?
                                      (append (get assigned-gardens volunteer) garden-id) u5)
                                   ERR-INVALID-INPUT)))
      (map-set volunteers
        { volunteer-id: volunteer-id }
        (merge volunteer { assigned-gardens: updated-gardens })
      )
    )
    (ok true)
  )
)

;; Record maintenance activity
(define-public (record-maintenance
  (garden-id uint)
  (maintenance-type (string-ascii 50))
  (notes (string-ascii 500))
  (cost uint)
  (quality-rating uint)
  (next-due uint)
)
  (let
    (
      (garden (unwrap! (map-get? gardens { garden-id: garden-id }) ERR-NOT-FOUND))
      (maintenance-id (var-get next-maintenance-id))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender (get manager garden))
                  (is-volunteer-assigned tx-sender garden-id)) ERR-UNAUTHORIZED)
    (asserts! (and (>= quality-rating u1) (<= quality-rating u10)) ERR-INVALID-INPUT)

    (map-set maintenance-records
      { garden-id: garden-id, record-id: maintenance-id }
      {
        maintenance-type: maintenance-type,
        performed-by: tx-sender,
        completed-at: current-block,
        next-due: next-due,
        notes: notes,
        cost: cost,
        quality-rating: quality-rating
      }
    )

    (var-set next-maintenance-id (+ maintenance-id u1))
    (ok maintenance-id)
  )
)

;; Update wellness metrics for a garden
(define-public (update-wellness-metrics
  (garden-id uint)
  (period uint)
  (patient-visits uint)
  (avg-session-duration uint)
  (stress-reduction-avg uint)
  (mobility-improvement-avg uint)
  (mood-improvement-avg uint)
  (social-interaction-score uint)
  (therapeutic-goals-met uint)
  (total-therapeutic-goals uint)
  (evidence-based-outcomes (list 10 uint))
)
  (let ((garden (unwrap! (map-get? gardens { garden-id: garden-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get manager garden)) ERR-UNAUTHORIZED)
    (asserts! (and (<= stress-reduction-avg u100) (<= mobility-improvement-avg u100)
                   (<= mood-improvement-avg u100) (<= social-interaction-score u100)) ERR-INVALID-INPUT)

    (map-set wellness-metrics
      { garden-id: garden-id, period: period }
      {
        patient-visits: patient-visits,
        avg-session-duration: avg-session-duration,
        stress-reduction-avg: stress-reduction-avg,
        mobility-improvement-avg: mobility-improvement-avg,
        mood-improvement-avg: mood-improvement-avg,
        social-interaction-score: social-interaction-score,
        therapeutic-goals-met: therapeutic-goals-met,
        total-therapeutic-goals: total-therapeutic-goals,
        evidence-based-outcomes: evidence-based-outcomes
      }
    )
    (ok true)
  )
)

;; Update garden status
(define-public (update-garden-status (garden-id uint) (new-status uint))
  (let ((garden (unwrap! (map-get? gardens { garden-id: garden-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get manager garden)) ERR-UNAUTHORIZED)
    (asserts! (and (>= new-status u1) (<= new-status u3)) ERR-INVALID-INPUT)

    (map-set gardens
      { garden-id: garden-id }
      (merge garden { status: new-status })
    )
    (ok true)
  )
)

;; Volunteer check-in (records hours)
(define-public (volunteer-checkin (garden-id uint) (hours-worked uint))
  (let
    (
      (volunteer (unwrap! (map-get? volunteers { volunteer-id: tx-sender }) ERR-NOT-FOUND))
      (garden (unwrap! (map-get? gardens { garden-id: garden-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-volunteer-assigned tx-sender garden-id) ERR-UNAUTHORIZED)
    (asserts! (and (> hours-worked u0) (<= hours-worked u12)) ERR-INVALID-INPUT)

    (map-set volunteers
      { volunteer-id: tx-sender }
      (merge volunteer {
        hours-contributed: (+ (get hours-contributed volunteer) hours-worked)
      })
    )
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-garden (garden-id uint))
  (map-get? gardens { garden-id: garden-id })
)

(define-read-only (get-volunteer (volunteer-id principal))
  (map-get? volunteers { volunteer-id: volunteer-id })
)

(define-read-only (get-wellness-metrics (garden-id uint) (period uint))
  (map-get? wellness-metrics { garden-id: garden-id, period: period })
)

(define-read-only (get-maintenance-record (garden-id uint) (record-id uint))
  (map-get? maintenance-records { garden-id: garden-id, record-id: record-id })
)

(define-read-only (calculate-garden-utilization (garden-id uint) (period uint))
  (let
    (
      (garden (unwrap! (map-get? gardens { garden-id: garden-id }) ERR-NOT-FOUND))
      (metrics (map-get? wellness-metrics { garden-id: garden-id, period: period }))
    )
    (match metrics
      wellness-data
        (let
          (
            (visits (get patient-visits wellness-data))
            (capacity (get capacity garden))
            (utilization-rate (if (> capacity u0) (/ (* visits u100) capacity) u0))
          )
          (ok utilization-rate)
        )
      (ok u0)
    )
  )
)

(define-read-only (get-garden-effectiveness-score (garden-id uint) (period uint))
  (match (map-get? wellness-metrics { garden-id: garden-id, period: period })
    wellness-data
      (let
        (
          (stress-score (get stress-reduction-avg wellness-data))
          (mobility-score (get mobility-improvement-avg wellness-data))
          (mood-score (get mood-improvement-avg wellness-data))
          (social-score (get social-interaction-score wellness-data))
          (goal-completion-rate (if (> (get total-therapeutic-goals wellness-data) u0)
                                  (/ (* (get therapeutic-goals-met wellness-data) u100)
                                     (get total-therapeutic-goals wellness-data))
                                  u0))
        )
        (ok (/ (+ stress-score mobility-score mood-score social-score goal-completion-rate) u5))
      )
    (ok u0)
  )
)

;; Helper functions

(define-private (is-facility-manager (user principal))
  ;; In production, this would check against a facility manager registry
  ;; For now, return false to require contract owner approval
  false
)

(define-private (is-volunteer-assigned (volunteer-id principal) (garden-id uint))
  (match (map-get? volunteers { volunteer-id: volunteer-id })
    volunteer-data
      (is-some (index-of (get assigned-gardens volunteer-data) garden-id))
    false
  )
)
