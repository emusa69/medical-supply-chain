;; Medical Supply Chain Management Contract
;; A healthcare inventory system with expiration tracking, recall management, and automated reordering

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-stock (err u102))
(define-constant err-expired (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-already-recalled (err u105))
(define-constant err-not-authorized (err u106))

;; Data Variables
(define-data-var next-supply-id uint u1)
(define-data-var next-facility-id uint u1)

;; Data Maps
(define-map medical-supplies
  { supply-id: uint }
  {
    name: (string-ascii 100),
    category: (string-ascii 50),
    current-stock: uint,
    min-threshold: uint,
    max-capacity: uint,
    unit-cost: uint,
    expiration-date: uint,
    batch-number: (string-ascii 50),
    supplier: principal,
    facility-id: uint,
    is-recalled: bool,
    last-updated: uint
  }
)

(define-map healthcare-facilities
  { facility-id: uint }
  {
    name: (string-ascii 100),
    facility-type: (string-ascii 50), ;; "hospital", "clinic", "pharmacy"
    administrator: principal,
    address: (string-ascii 200),
    is-active: bool
  }
)

(define-map facility-permissions
  { facility-id: uint, user: principal }
  { role: (string-ascii 20) } ;; "admin", "manager", "staff"
)

(define-map supply-transactions
  { transaction-id: uint }
  {
    supply-id: uint,
    transaction-type: (string-ascii 20), ;; "received", "dispensed", "expired", "recalled"
    quantity: uint,
    timestamp: uint,
    performed-by: principal,
    notes: (optional (string-ascii 200))
  }
)

(define-data-var next-transaction-id uint u1)

;; Reorder suggestions map
(define-map reorder-suggestions
  { supply-id: uint }
  {
    suggested-quantity: uint,
    urgency-level: (string-ascii 20), ;; "low", "medium", "high", "critical"
    created-at: uint,
    is-processed: bool
  }
)

;; Public Functions

;; Register a new healthcare facility
(define-public (register-facility (name (string-ascii 100)) (facility-type (string-ascii 50)) (address (string-ascii 200)))
  (let
    (
      (facility-id (var-get next-facility-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set healthcare-facilities
      { facility-id: facility-id }
      {
        name: name,
        facility-type: facility-type,
        administrator: tx-sender,
        address: address,
        is-active: true
      }
    )
    (map-set facility-permissions
      { facility-id: facility-id, user: tx-sender }
      { role: "admin" }
    )
    (var-set next-facility-id (+ facility-id u1))
    (ok facility-id)
  )
)

;; Add a new medical supply to inventory
(define-public (add-medical-supply
  (name (string-ascii 100))
  (category (string-ascii 50))
  (initial-stock uint)
  (min-threshold uint)
  (max-capacity uint)
  (unit-cost uint)
  (expiration-date uint)
  (batch-number (string-ascii 50))
  (supplier principal)
  (facility-id uint)
)
  (let
    (
      (supply-id (var-get next-supply-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-authorized facility-id tx-sender) err-not-authorized)
    (asserts! (> expiration-date current-time) err-invalid-input)
    (asserts! (> max-capacity min-threshold) err-invalid-input)
    
    (map-set medical-supplies
      { supply-id: supply-id }
      {
        name: name,
        category: category,
        current-stock: initial-stock,
        min-threshold: min-threshold,
        max-capacity: max-capacity,
        unit-cost: unit-cost,
        expiration-date: expiration-date,
        batch-number: batch-number,
        supplier: supplier,
        facility-id: facility-id,
        is-recalled: false,
        last-updated: current-time
      }
    )
    
    ;; Record the initial stock transaction
    (unwrap-panic (record-transaction supply-id "received" initial-stock (some "Initial stock")))
    
    (var-set next-supply-id (+ supply-id u1))
    (ok supply-id)
  )
)

;; Update stock levels (for receiving new inventory or dispensing)
(define-public (update-stock (supply-id uint) (quantity-change int) (transaction-type (string-ascii 20)) (notes (optional (string-ascii 200))))
  (let
    (
      (supply-data (unwrap! (map-get? medical-supplies { supply-id: supply-id }) err-not-found))
      (current-stock (get current-stock supply-data))
      (new-stock (if (>= quantity-change 0)
                    (+ current-stock (to-uint quantity-change))
                    (if (>= current-stock (to-uint (- 0 quantity-change)))
                        (- current-stock (to-uint (- 0 quantity-change)))
                        u0)))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-authorized (get facility-id supply-data) tx-sender) err-not-authorized)
    (asserts! (not (get is-recalled supply-data)) err-already-recalled)
    
    ;; Check if trying to dispense more than available
    (asserts! (or (>= quantity-change 0) (>= current-stock (to-uint (- 0 quantity-change)))) err-insufficient-stock)
    
    ;; Update the supply record
    (map-set medical-supplies
      { supply-id: supply-id }
      (merge supply-data { current-stock: new-stock, last-updated: current-time })
    )
    
    ;; Record the transaction
    (unwrap-panic (record-transaction supply-id transaction-type (to-uint (if (>= quantity-change 0) quantity-change (- 0 quantity-change))) notes))
    
    ;; Check if reorder is needed
    (if (< new-stock (get min-threshold supply-data))
        (unwrap-panic (create-reorder-suggestion supply-id))
        true
    )
    
    (ok new-stock)
  )
)

;; Mark a supply as recalled
(define-public (recall-supply (supply-id uint) (reason (string-ascii 200)))
  (let
    (
      (supply-data (unwrap! (map-get? medical-supplies { supply-id: supply-id }) err-not-found))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-authorized (get facility-id supply-data) tx-sender) err-not-authorized)
    (asserts! (not (get is-recalled supply-data)) err-already-recalled)
    
    ;; Mark as recalled
    (map-set medical-supplies
      { supply-id: supply-id }
      (merge supply-data { is-recalled: true, last-updated: current-time })
    )
    
    ;; Record recall transaction
    (unwrap-panic (record-transaction supply-id "recalled" (get current-stock supply-data) (some reason)))
    
    (ok true)
  )
)

;; Grant facility access to a user
(define-public (grant-facility-access (facility-id uint) (user principal) (role (string-ascii 20)))
  (let
    (
      (facility-data (unwrap! (map-get? healthcare-facilities { facility-id: facility-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get administrator facility-data)) err-not-authorized)
    (map-set facility-permissions
      { facility-id: facility-id, user: user }
      { role: role }
    )
    (ok true)
  )
)

;; Process a reorder suggestion
(define-public (process-reorder (supply-id uint) (order-quantity uint))
  (let
    (
      (supply-data (unwrap! (map-get? medical-supplies { supply-id: supply-id }) err-not-found))
      (suggestion-data (unwrap! (map-get? reorder-suggestions { supply-id: supply-id }) err-not-found))
    )
    (asserts! (is-authorized (get facility-id supply-data) tx-sender) err-not-authorized)
    (asserts! (not (get is-processed suggestion-data)) err-invalid-input)
    
    ;; Mark suggestion as processed
    (map-set reorder-suggestions
      { supply-id: supply-id }
      (merge suggestion-data { is-processed: true })
    )
    
    ;; Add the ordered stock
    (unwrap-panic (update-stock supply-id (to-int order-quantity) "received" (some "Reorder processed")))
    
    (ok true)
  )
)

;; Private Functions

;; Check if user is authorized for facility operations
(define-private (is-authorized (facility-id uint) (user principal))
  (or
    (is-eq user contract-owner)
    (is-some (map-get? facility-permissions { facility-id: facility-id, user: user }))
  )
)

;; Record a supply transaction
(define-private (record-transaction (supply-id uint) (transaction-type (string-ascii 20)) (quantity uint) (notes (optional (string-ascii 200))))
  (let
    (
      (transaction-id (var-get next-transaction-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (map-set supply-transactions
      { transaction-id: transaction-id }
      {
        supply-id: supply-id,
        transaction-type: transaction-type,
        quantity: quantity,
        timestamp: current-time,
        performed-by: tx-sender,
        notes: notes
      }
    )
    (var-set next-transaction-id (+ transaction-id u1))
    (ok transaction-id)
  )
)

;; Create reorder suggestion
(define-private (create-reorder-suggestion (supply-id uint))
  (let
    (
      (supply-data (unwrap-panic (map-get? medical-supplies { supply-id: supply-id })))
      (current-stock (get current-stock supply-data))
      (max-capacity (get max-capacity supply-data))
      (min-threshold (get min-threshold supply-data))
      (suggested-quantity (- max-capacity current-stock))
      (urgency-level (if (<= current-stock (/ min-threshold u2))
                        "critical"
                        (if (<= current-stock (/ (* min-threshold u3) u4))
                            "high"
                            "medium")))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (map-set reorder-suggestions
      { supply-id: supply-id }
      {
        suggested-quantity: suggested-quantity,
        urgency-level: urgency-level,
        created-at: current-time,
        is-processed: false
      }
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get supply details
(define-read-only (get-supply (supply-id uint))
  (map-get? medical-supplies { supply-id: supply-id })
)

;; Get facility details
(define-read-only (get-facility (facility-id uint))
  (map-get? healthcare-facilities { facility-id: facility-id })
)

;; Get reorder suggestions for a supply
(define-read-only (get-reorder-suggestion (supply-id uint))
  (map-get? reorder-suggestions { supply-id: supply-id })
)

;; Check if supply is expired
(define-read-only (is-supply-expired (supply-id uint))
  (match (map-get? medical-supplies { supply-id: supply-id })
    supply-data
    (let
      (
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (expiration-date (get expiration-date supply-data))
      )
      (>= current-time expiration-date)
    )
    false
  )
)

;; Get transaction history for a supply
(define-read-only (get-transaction (transaction-id uint))
  (map-get? supply-transactions { transaction-id: transaction-id })
)

;; Check if user has facility access
(define-read-only (has-facility-access (facility-id uint) (user principal))
  (is-authorized facility-id user)
)

;; Get current supply counts
(define-read-only (get-supply-counts)
  {
    next-supply-id: (var-get next-supply-id),
    next-facility-id: (var-get next-facility-id),
    next-transaction-id: (var-get next-transaction-id)
  }
)
