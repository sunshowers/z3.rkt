#lang racket/base

(require "../main.rkt"
         "../examples/list-helpers.rkt")
(require rackunit)

(define/provide-test-suite test-list-helpers
  (test-case
   "Test reverse"
   (smt:with-context
    (smt:new-context-info)
    (define reverse (car (make-reverse 10)))
    (smt:declare-fun v1 () IntList)
    (smt:assert (=/s v1 (reverse (nil/s) (nil/s))))
    (smt:declare-fun v2 () IntList)
    (smt:assert (=/s v2 (reverse (list->z3-list '(1 2 3 4 5)) (nil/s))))
    (smt:declare-fun v3 () IntList)
    (smt:assert (=/s v3 (reverse (list->z3-list '(1 2 3 4 5 6 7 8 9 10)) (nil/s))))
    (check-eq? (smt:check-sat) 'sat)
    (check-equal? (z3-list->list (smt:eval v1)) '())
    (check-equal? (z3-list->list (smt:eval v2)) '(5 4 3 2 1))
    (check-equal? (z3-list->list (smt:eval v3)) '(10 9 8 7 6 5 4 3 2 1)))))
