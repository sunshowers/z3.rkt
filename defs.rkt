#lang racket
;; Indicates an instance of a datatype (e.g. (List Int) for List).
(struct datatype-instance (z3-sort fns))

;; A complex sort (e.g. List) has data about the base sort, a creator function
;; (which takes the base sort and a list of sort parameters to apply and produces
;; an immutable datatype-instance. We also want to cache instances for specific sort
;; parameters (so (List Int) followed by (List Int) should return the same
;; datatype-instance.
(struct z3-complex-sort (base-sort creator instance-hash))

;; Creates a new complex sort. This adds hooks for each constructor to the namespace
;; provided.
;; XXX handle hooks
(define (make-complex-sort base-sort creator ns hook-ids)
  (let ([res (z3-complex-sort base-sort creator (make-hash))])))

;; Given a base sort and parameter sorts get or create a parameterized datatype.
(define (get-or-create-instance sort params)
  (let ([ref (hash-ref (z3-complex-sort-instance-hash sort) params #f)])
    (if ref ref
        ((z3-complex-sort-creator sort) (z3-complex-sort-base-sort sort) params))))

(provide (struct-out datatype-instance)
         (struct-out z3-complex-sort)
         get-or-create-instance)

