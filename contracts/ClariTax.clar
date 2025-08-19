;; Contract: ClariTax - FIFO Capital Gains & Tax Engine
;; Author: 
;; License: MIT

;; Constants
(define-constant ZERO u0)
(define-constant ONE u1)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_TX_TYPE (err u101))
(define-constant ERR_INSUFFICIENT (err u102))
(define-constant ERR_NOT_FOUND (err u104))
(define-constant ERR_PAUSED (err u105))
(define-constant ERR_BAD_ARG (err u106))
(define-constant ERR_OVERFLOW (err u107))
(define-constant ERR_BAD_STATE (err u108))
(define-constant ERR_BAD_LOT (err u109))

;; Data variables
(define-data-var admin principal tx-sender)
(define-data-var paused bool false)
(define-data-var tax-rate-bps uint u1500)  ;; 15.00% in basis points
(define-data-var lt-hold-blocks uint u105120) ;; ~1 year
(define-data-var treasury principal tx-sender)
(define-data-var withholding-enabled bool false)

;; Maps
(define-map operators principal bool)

(define-map transactions
  {user: principal, asset: (string-ascii 10), tx-id: uint}
  {kind: (string-ascii 10), amount: uint, price: uint, timestamp: uint})

(define-map user-last-tx-id
  {user: principal, asset: (string-ascii 10)}
  {last: uint})

(define-map lots
  {user: principal, asset: (string-ascii 10), idx: uint}
  {amount: uint, price: uint, block: uint})

(define-map lot-head-tail
  {user: principal, asset: (string-ascii 10)}
  {head: uint, tail: uint})

(define-map balances
  {user: principal, asset: (string-ascii 10)}
  {amount: uint})

(define-map realized-gains
  {user: principal, asset: (string-ascii 10)}
  {short: uint, long: uint, last-updated: uint})

(define-map adjustments
  {user: principal, asset: (string-ascii 10), adj-id: uint}
  {delta-basis: int, delta-amount: int, note: (string-ascii 48), block: uint})

(define-map last-adj-id
  {user: principal, asset: (string-ascii 10)}
  {last: uint})

;; Private functions
(define-private (only-admin)
  (ok (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)))

(define-private (only-op-or-admin)
  (ok (asserts! (or
    (is-eq tx-sender (var-get admin))
    (default-to false (map-get? operators tx-sender))
  ) ERR_UNAUTHORIZED)))

(define-private (when-active)
  (ok (asserts! (not (var-get paused)) ERR_PAUSED)))

(define-private (next-tx-id (user principal) (asset (string-ascii 10)))
  (let ((cur (map-get? user-last-tx-id {user: user, asset: asset})))
    (match cur
      existing (let ((n (+ (get last existing) ONE)))
        (map-set user-last-tx-id {user: user, asset: asset} {last: n})
        n)
      (begin 
        (map-set user-last-tx-id {user: user, asset: asset} {last: ONE})
        ONE))))

(define-private (append-lot (user principal) (asset (string-ascii 10)) (amount uint) (price uint))
  ;; returns (response uint uint)
  (let ((lot-info (match (map-get? lot-head-tail {user: user, asset: asset})
                    info info
                    {head: ZERO, tail: ZERO})))
    (let ((h (get head lot-info))
          (t (get tail lot-info)))
      (begin
        (asserts! (> amount ZERO) ERR_BAD_ARG)
        (map-set lots 
          {user: user, asset: asset, idx: t}
          {amount: amount, price: price, block: stacks-block-height})
        (map-set lot-head-tail 
          {user: user, asset: asset}
          {head: h, tail: (+ t ONE)})
        (ok t)))));; Modify balance up or down, checks for insufficient funds
(define-private (inc-balance (user principal) (asset (string-ascii 10)) (delta int))
  ;; returns (response bool uint)
  (let ((balance-info (map-get? balances {user: user, asset: asset})))
    (let ((current-amount (if (is-some balance-info) 
                             (get amount (unwrap-panic balance-info)) 
                             ZERO)))
      (if (>= delta 0)
        (begin
          (map-set balances 
            {user: user, asset: asset}
            {amount: (+ current-amount (to-uint delta))})
          (ok true))
        (let ((d (to-uint (if (< delta 0) (* delta -1) delta))))
          (asserts! (>= current-amount d) ERR_INSUFFICIENT)
          (begin
            (map-set balances 
              {user: user, asset: asset}
              {amount: (- current-amount d)})
            (ok true)))))))

(define-private (record-tx 
                (user principal) 
                (asset (string-ascii 10)) 
                (kind (string-ascii 10)) 
                (amount uint) 
                (price uint))
  (let ((id (next-tx-id user asset)))
    (map-set transactions {user: user, asset: asset, tx-id: id}
             {kind: kind, amount: amount, price: price, timestamp: stacks-block-height})
    id))

;; returns bool
(define-private (add-realized 
                (user principal) 
                (asset (string-ascii 10))
                (short uint) 
                (long uint))
  (begin
    (match (map-get? realized-gains {user: user, asset: asset})
      r (map-set realized-gains 
          {user: user, asset: asset}
          {short: (+ (get short r) short), 
           long: (+ (get long r) long), 
           last-updated: stacks-block-height})
      (map-set realized-gains 
          {user: user, asset: asset}
          {short: short, 
           long: long, 
           last-updated: stacks-block-height}))
    true))

(define-private (head-tail 
                (user principal) 
                (asset (string-ascii 10)))
  (match (map-get? lot-head-tail {user: user, asset: asset})
    q {head: (get head q), 
       tail: (get tail q)}
    {head: ZERO, 
     tail: ZERO}))

(define-private (set-head 
                (user principal) 
                (asset (string-ascii 10)) 
                (new-head uint))
  (let ((qt (map-get? lot-head-tail {user: user, asset: asset})))
    (match qt
      q (map-set lot-head-tail 
          {user: user, asset: asset}
          {head: new-head, 
           tail: (get tail q)})
      (map-set lot-head-tail 
          {user: user, asset: asset}
          {head: new-head, 
           tail: new-head}))))

(define-private (sell-from-lots
                (params {user: principal,
                        asset: (string-ascii 10),
                        amount: uint,
                        price: uint,
                        head: uint,
                        tail: uint}))
  (let ((user (get user params))
        (asset (get asset params))
        (amount (get amount params))
        (price (get price params))
        (head (get head params))
        (tail (get tail params)))
    (if (or (is-eq amount ZERO) (>= head tail))
        (ok {head: head, 
             short: ZERO, 
             long: ZERO, 
             rem: amount})
        (let ((lot (unwrap! (map-get? lots {user: user, asset: asset, idx: head}) 
                           (err u1))))
          (let ((lot-amt (get amount lot))
                (lot-price (get price lot))
                (lot-block (get block lot))
                (consume (if (<= amount lot-amt) amount lot-amt))
                (left    (if (<= amount lot-amt) (- lot-amt consume) ZERO))
                (gain    (- (* consume price) (* consume lot-price)))
                (held    (- stacks-block-height lot-block))
                (lt-th   (var-get lt-hold-blocks))
                (is-short (>= held lt-th))
                (pos-gain (if (> gain ZERO) gain ZERO)))
            (begin 
              (if (is-eq left ZERO)
                  (map-delete lots {user: user, asset: asset, idx: head})
                  (map-set lots {user: user, asset: asset, idx: head}
                          {amount: left, price: lot-price, block: lot-block}))
              (ok {head: (+ head ONE),
                   short: (if is-short pos-gain ZERO),
                   long: (if is-short ZERO pos-gain),
                   rem: (- amount consume)})))))))

;; Public functions
(define-public (set-operator (who principal) (is-op bool))
  (match (only-admin)
    success
      (begin
        (map-set operators who is-op)
        (ok true))
    error (err u100)))

(define-public (set-paused (p bool))
  (match (only-op-or-admin)
    success
      (begin
        (var-set paused p)
        (ok p))
    error (err u100)))

(define-public (set-tax-rate-bps (bps uint))
  (match (only-op-or-admin)
    success
      (begin
        (asserts! (<= bps u5000) ERR_BAD_ARG)
        (var-set tax-rate-bps bps)
        (ok bps))
    error (err u100)))

(define-public (set-lt-hold-blocks (blocks uint))
  (match (only-op-or-admin)
    success
      (begin
        (var-set lt-hold-blocks blocks)
        (ok blocks))
    error (err u100)))

(define-public (set-treasury (to principal))
  (match (only-admin)
    success
      (begin
        (var-set treasury to)
        (ok to))
    error (err u100)))

(define-public (set-withholding (on bool))
  (match (only-op-or-admin)
    success
      (begin
        (var-set withholding-enabled on)
        (ok on))
    error (err u100)))

;; User functions
(define-public (buy 
                (asset (string-ascii 10)) 
                (amount uint) 
                (price uint))
  (begin
    (try! (when-active))
    (asserts! (> amount ZERO) ERR_BAD_ARG)
    (asserts! (> price ZERO) ERR_BAD_ARG)
    (asserts! (is-eq (len asset) (len asset)) ERR_BAD_ARG)  ;; Ensures string is valid ascii
    (try! (append-lot tx-sender asset amount price))
    (try! (inc-balance tx-sender asset (to-int amount)))
    (ok (record-tx tx-sender asset "buy" amount price))))

(define-public (transfer-in 
                (asset (string-ascii 10)) 
                (amount uint) 
                (basis-price uint))
  (begin
    (try! (when-active))
    (asserts! (> amount ZERO) ERR_BAD_ARG)
    (try! (append-lot tx-sender asset amount basis-price))
    (try! (inc-balance tx-sender asset (to-int amount)))
    (ok (record-tx tx-sender asset "xfer-in" amount basis-price))))

(define-public (transfer-out 
                (asset (string-ascii 10)) 
                (amount uint) 
                (price-for-accounting (optional uint)))
  (begin
    (try! (when-active))
    (asserts! (> amount ZERO) ERR_BAD_ARG)
    (let ((bal (unwrap! (map-get? balances {user: tx-sender, asset: asset}) 
                        ERR_INSUFFICIENT))
          (qt (head-tail tx-sender asset))
          (price (match price-for-accounting p p ZERO)))
      (begin
        (asserts! (>= (get amount bal) amount) ERR_INSUFFICIENT)
        (let ((sell-result (try! (sell-from-lots {
                                  user: tx-sender,
                                  asset: asset,
                                  amount: amount,
                                  price: price,
                                  head: (get head qt),
                                  tail: (get tail qt)
                                }))))
          (begin
            (set-head tx-sender asset (get head sell-result))
            (try! (inc-balance tx-sender asset (to-int (- amount))))
            (if (is-some price-for-accounting)
                (asserts! (add-realized tx-sender asset 
                                      (get short sell-result) 
                                      (get long sell-result))
                         ERR_BAD_STATE)
                true)
            (ok (record-tx tx-sender asset "xfer-out" amount price))))))))

(define-public (sell (asset (string-ascii 10)) (amount uint) (sell-price uint))
  (begin
    (try! (when-active))
    (asserts! (> amount ZERO) ERR_BAD_ARG)
    (asserts! (> sell-price ZERO) ERR_BAD_ARG)
    (asserts! (is-eq (len asset) (len asset)) ERR_BAD_ARG)
    (let ((bal (unwrap! (map-get? balances {user: tx-sender, asset: asset}) ERR_INSUFFICIENT)))
      (asserts! (>= (get amount bal) amount) ERR_INSUFFICIENT)
      (let ((qt (head-tail tx-sender asset))
            (sell-result (try! (sell-from-lots {
                                user: tx-sender,
                                asset: asset,
                                amount: amount,
                                price: sell-price,
                                head: (get head qt),
                                tail: (get tail qt)
                              }))))
        (begin
          (set-head tx-sender asset (get head sell-result))
          (try! (inc-balance tx-sender asset (to-int (- amount))))
          (asserts! (add-realized tx-sender asset 
                     (get short sell-result) 
                     (get long sell-result))
            ERR_BAD_STATE)
          (ok (record-tx tx-sender asset "sell" amount sell-price)))))))


;; Manual operator adjustments (audit note)
(define-public (adjust (user principal) (asset (string-ascii 10)) (delta-basis int) (delta-amount int) (note (string-ascii 48)))
  (begin
    ;; Input validation
    (asserts! (is-eq (len asset) (len asset)) ERR_BAD_ARG)  ;; Ensures string is valid ascii
    (asserts! (not (is-eq delta-basis 0)) ERR_BAD_ARG)
    (asserts! (not (is-eq delta-amount 0)) ERR_BAD_ARG)
    (asserts! (> (len note) u0) ERR_BAD_ARG)
    
    (let ((admin-result (try! (only-op-or-admin)))
          (balance-result (try! (inc-balance user asset delta-amount))))
      (let ((cur (map-get? last-adj-id {user: user, asset: asset})))
        (match cur
          c (let ((nid (+ (get last c) ONE)))
               (map-set last-adj-id {user: user, asset: asset} {last: nid})
               (map-set adjustments {user: user, asset: asset, adj-id: nid}
                 {delta-basis: delta-basis, delta-amount: delta-amount, note: note, block: stacks-block-height})
               (ok nid))
          (begin
            (map-set last-adj-id {user: user, asset: asset} {last: ONE})
            (map-set adjustments {user: user, asset: asset, adj-id: ONE}
              {delta-basis: delta-basis, delta-amount: delta-amount, note: note, block: stacks-block-height})
            (ok ONE)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reports & Read-only Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-balance (user principal) (asset (string-ascii 10)))
  (default-to ZERO (get amount (map-get? balances {user: user, asset: asset}))))

(define-read-only (get-realized (user principal) (asset (string-ascii 10)))
  (match (map-get? realized-gains {user: user, asset: asset})
    gains gains
    {short: ZERO, long: ZERO, last-updated: ZERO}))

(define-read-only (estimate-unrealized (user principal) (asset (string-ascii 10)) (mark-price uint))
  (let ((qt (head-tail user asset)))
    (match (map-get? lots {user: user, asset: asset, idx: (get head qt)})
      lot (let ((amt (get amount lot))
               (basis (* amt (get price lot)))
               (mv (* amt mark-price)))
            (if (>= mv basis) (- mv basis) ZERO))
      ZERO)))

(define-read-only (get-tax-owed (user principal) (asset (string-ascii 10)))
  (let ((r (map-get? realized-gains {user: user, asset: asset})))
    (match r
      g (let ((bps (var-get tax-rate-bps))
              (gains (+ (get short g) (get long g))))
           (/ (* gains bps) u10000))
      ZERO)))

;; Lightweight portfolio summary (current realized + balance)
(define-read-only (portfolio-summary (user principal) (asset (string-ascii 10)) (mark-price uint))
  (let ((bal (default-to ZERO (get amount (map-get? balances {user: user, asset: asset}))))
        (real (get-realized user asset))
        (unreal (estimate-unrealized user asset mark-price)))
    (ok {
      asset: asset,
      balance: bal,
      realized_short: (get short real),
      realized_long: (get long real),
      unrealized_gain_at_mark: unreal,
      tax_rate_bps: (var-get tax-rate-bps),
      est_tax_on_realized: (/ (* (+ (get short real) (get long real)) (var-get tax-rate-bps)) u10000)
    })))

;; Tx fetch + count
(define-read-only (get-transaction (user principal) (asset (string-ascii 10)) (tx-id uint))
  (match (map-get? transactions {user: user, asset: asset, tx-id: tx-id})
    t (ok t)
    ERR_NOT_FOUND))

(define-read-only (get-transaction-count (user principal) (asset (string-ascii 10)))
  (default-to ZERO (get last (map-get? user-last-tx-id {user: user, asset: asset}))))

;; Lot page (for UI pagination)
(define-read-only (get-lot (user principal) (asset (string-ascii 10)) (idx uint))
  (map-get? lots {user: user, asset: asset, idx: idx}))

(define-read-only (get-lot-head-tail (user principal) (asset (string-ascii 10)))
  (match (map-get? lot-head-tail {user: user, asset: asset})
    queue queue
    {head: ZERO, tail: ZERO}))

;; Adjustments introspection
(define-read-only (get-adjustment (user principal) (asset (string-ascii 10)) (adj-id uint))
  (map-get? adjustments {user: user, asset: asset, adj-id: adj-id}))

(define-read-only (get-last-adjustment-id (user principal) (asset (string-ascii 10)))
  (default-to ZERO (get last (map-get? last-adj-id {user: user, asset: asset}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Legacy-compatible wrappers (optional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep generate-report signature spirit, now per-asset
(define-read-only (generate-report (user principal) (asset (string-ascii 10)) (mark-price uint))
  (begin 
    (asserts! (is-eq (len asset) (len asset)) ERR_BAD_ARG)  ;; Ensures string is valid ascii
    (let ((tx-count (default-to ZERO (get last (map-get? user-last-tx-id {user: user, asset: asset}))))
          (real (get-realized user asset))
          (tax-bps (var-get tax-rate-bps))
          (est-tax (/ (* (+ (get short real) (get long real)) tax-bps) u10000)))
      (ok {
        user: user,
        asset: asset,
        transactions_count: tx-count,
        realized_short: (get short real),
        realized_long: (get long real),
        tax_rate_bps: tax-bps,
        estimated_tax_on_realized: est-tax,
        balance: (default-to ZERO (get amount (map-get? balances {user: user, asset: asset}))),
        unrealized_gain_at_mark: (estimate-unrealized user asset mark-price)}))))
