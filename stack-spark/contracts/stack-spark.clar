;; StackSpark - Blockchain Batch Management Platform
;; A smart contract for predictive compliance orchestration and batch quality tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-already-exists (err u104))

;; Data Variables
(define-data-var batch-nonce uint u0)
(define-data-var supplier-nonce uint u0)

;; Batch Status Types
(define-constant status-created u0)
(define-constant status-in-transit u1)
(define-constant status-quality-check u2)
(define-constant status-approved u3)
(define-constant status-rejected u4)
(define-constant status-recalled u5)

;; Data Maps
(define-map batches
    { batch-id: uint }
    {
        supplier: principal,
        product-type: (string-ascii 64),
        quantity: uint,
        status: uint,
        quality-score: uint,
        compliance-score: uint,
        created-at: uint,
        parent-batch: (optional uint),
        metadata-hash: (buff 32)
    }
)

(define-map batch-attestations
    { batch-id: uint, attestation-id: uint }
    {
        attestor: principal,
        attestation-type: (string-ascii 32),
        quality-metric: uint,
        timestamp: uint,
        proof-hash: (buff 32)
    }
)

(define-map suppliers
    { supplier-id: uint }
    {
        address: principal,
        name: (string-ascii 64),
        reputation-score: uint,
        total-batches: uint,
        approved-batches: uint,
        active: bool
    }
)

(define-map supplier-principals
    { address: principal }
    { supplier-id: uint }
)

(define-map batch-compliance
    { batch-id: uint }
    {
        jurisdiction: (string-ascii 32),
        regulation-version: (string-ascii 16),
        compliant: bool,
        verified-at: uint,
        verifier: principal
    }
)

;; Private helper functions
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

;; Read-only functions
(define-read-only (get-batch (batch-id uint))
    (map-get? batches { batch-id: batch-id })
)

(define-read-only (get-supplier (supplier-id uint))
    (map-get? suppliers { supplier-id: supplier-id })
)

(define-read-only (get-supplier-by-principal (address principal))
    (match (map-get? supplier-principals { address: address })
        supplier-data (map-get? suppliers { supplier-id: (get supplier-id supplier-data) })
        none
    )
)

(define-read-only (get-batch-attestation (batch-id uint) (attestation-id uint))
    (map-get? batch-attestations { batch-id: batch-id, attestation-id: attestation-id })
)

(define-read-only (get-batch-compliance (batch-id uint))
    (map-get? batch-compliance { batch-id: batch-id })
)

(define-read-only (get-batch-quality-score (batch-id uint))
    (match (get-batch batch-id)
        batch (ok (get quality-score batch))
        (err err-not-found)
    )
)

;; Supplier Management
(define-public (register-supplier (name (string-ascii 64)))
    (let
        (
            (new-supplier-id (+ (var-get supplier-nonce) u1))
        )
        (asserts! (is-none (map-get? supplier-principals { address: tx-sender })) err-already-exists)
        (map-set suppliers
            { supplier-id: new-supplier-id }
            {
                address: tx-sender,
                name: name,
                reputation-score: u50,
                total-batches: u0,
                approved-batches: u0,
                active: true
            }
        )
        (map-set supplier-principals
            { address: tx-sender }
            { supplier-id: new-supplier-id }
        )
        (var-set supplier-nonce new-supplier-id)
        (ok new-supplier-id)
    )
)

;; Batch Creation
(define-public (create-batch 
    (product-type (string-ascii 64))
    (quantity uint)
    (parent-batch (optional uint))
    (metadata-hash (buff 32)))
    (let
        (
            (new-batch-id (+ (var-get batch-nonce) u1))
            (supplier-data (unwrap! (get-supplier-by-principal tx-sender) err-unauthorized))
        )
        (asserts! (get active supplier-data) err-unauthorized)
        (map-set batches
            { batch-id: new-batch-id }
            {
                supplier: tx-sender,
                product-type: product-type,
                quantity: quantity,
                status: status-created,
                quality-score: u0,
                compliance-score: u0,
                created-at: block-height,
                parent-batch: parent-batch,
                metadata-hash: metadata-hash
            }
        )
        (var-set batch-nonce new-batch-id)
        (ok new-batch-id)
    )
)

;; Quality Attestation
(define-public (add-quality-attestation
    (batch-id uint)
    (attestation-id uint)
    (attestation-type (string-ascii 32))
    (quality-metric uint)
    (proof-hash (buff 32)))
    (let
        (
            (batch (unwrap! (get-batch batch-id) err-not-found))
        )
        (map-set batch-attestations
            { batch-id: batch-id, attestation-id: attestation-id }
            {
                attestor: tx-sender,
                attestation-type: attestation-type,
                quality-metric: quality-metric,
                timestamp: block-height,
                proof-hash: proof-hash
            }
        )
        (ok true)
    )
)

;; Update Batch Status
(define-public (update-batch-status (batch-id uint) (new-status uint))
    (let
        (
            (batch (unwrap! (get-batch batch-id) err-not-found))
        )
        (asserts! (is-eq (get supplier batch) tx-sender) err-unauthorized)
        (asserts! (<= new-status status-recalled) err-invalid-status)
        (map-set batches
            { batch-id: batch-id }
            (merge batch { status: new-status })
        )
        (ok true)
    )
)

;; Update Quality Score
(define-public (update-quality-score (batch-id uint) (quality-score uint))
    (let
        (
            (batch (unwrap! (get-batch batch-id) err-not-found))
        )
        (asserts! (is-eq (get supplier batch) tx-sender) err-unauthorized)
        (asserts! (<= quality-score u100) err-invalid-status)
        (map-set batches
            { batch-id: batch-id }
            (merge batch { quality-score: quality-score })
        )
        (ok true)
    )
)

;; Compliance Verification
(define-public (verify-compliance
    (batch-id uint)
    (jurisdiction (string-ascii 32))
    (regulation-version (string-ascii 16))
    (compliant bool))
    (let
        (
            (batch (unwrap! (get-batch batch-id) err-not-found))
        )
        (map-set batch-compliance
            { batch-id: batch-id }
            {
                jurisdiction: jurisdiction,
                regulation-version: regulation-version,
                compliant: compliant,
                verified-at: block-height,
                verifier: tx-sender
            }
        )
        (ok true)
    )
)

;; Batch Approval (updates supplier reputation)
(define-public (approve-batch (batch-id uint))
    (let
        (
            (batch (unwrap! (get-batch batch-id) err-not-found))
            (supplier-lookup (unwrap! (map-get? supplier-principals { address: (get supplier batch) }) err-not-found))
            (supplier (unwrap! (map-get? suppliers { supplier-id: (get supplier-id supplier-lookup) }) err-not-found))
            (new-reputation (+ (get reputation-score supplier) u5))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set batches
            { batch-id: batch-id }
            (merge batch { status: status-approved })
        )
        (map-set suppliers
            { supplier-id: (get supplier-id supplier-lookup) }
            (merge supplier { 
                approved-batches: (+ (get approved-batches supplier) u1),
                reputation-score: (if (> new-reputation u100) u100 new-reputation)
            })
        )
        (ok true)
    )
)

;; Batch Recall
(define-public (recall-batch (batch-id uint) (reason (string-ascii 256)))
    (let
        (
            (batch (unwrap! (get-batch batch-id) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set batches
            { batch-id: batch-id }
            (merge batch { status: status-recalled })
        )
        (print { event: "batch-recalled", batch-id: batch-id, reason: reason })
        (ok true)
    )
)

;; Update Supplier Reputation
(define-public (update-supplier-reputation (supplier-id uint) (new-score uint))
    (let
        (
            (supplier (unwrap! (get-supplier supplier-id) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-score u100) err-invalid-status)
        (map-set suppliers
            { supplier-id: supplier-id }
            (merge supplier { reputation-score: new-score })
        )
        (ok true)
    )
)

;; Initialize contract
(begin
    (var-set batch-nonce u0)
    (var-set supplier-nonce u0)
)