#lang racket
;; Indicates an instance of a datatype (e.g. (List Int) for List).
(struct datatype-instance (z3-sort fns))

;; A complex sort (e.g. List) has data about the base sort, a creator function
;; (which takes the base sort and a list of sort parameters to apply and produces
;; an immutable datatype-instance. We also want to cache values for specific sort
;; parameters. 
(struct z3-complex-sort (base-sort creator instance-hash))

;; Given a base sort and parameter sorts get or create a parameterized datatype.
(define (get-or-create-instance sort params)
  (let ([ref (hash-ref (z3-complex-sort-instance-hash sort) params #f)])
    (if ref ref
        ((z3-complex-sort-creator sort) (z3-complex-sort-base-sort sort) params))))

(provide (struct-out datatype-instance)
         (struct-out z3-complex-sort)
         get-or-create-instance)

