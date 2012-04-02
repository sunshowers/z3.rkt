#lang racket

(require rackunit)
(require "../smtlib2-parser.rkt")

(define/provide-test-suite test-arrays
  (test-case
   "Test basic array operations"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun a () (Array Int Int))
    (smt:assert (=/s (select/s a 0) 5))
    (smt:assert (</s (select/s a 1) (select/s a 0)))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval (select/s a 0)) 5)
    (check-true (< (smt:eval (select/s a 1)) (smt:eval (select/s a 0)))))))
