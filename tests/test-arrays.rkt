#lang racket

(require rackunit)
(require "../smtlib2-parser.rkt")

(define/provide-test-suite test-arrays
  (test-case
   "Test basic array operations"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun a () (Array Int Int))
    (smt:assert (= (select a 0) 5))
    (smt:assert (< (select a 1) (select a 0)))
    (check-eq? (smt:check-sat) 'sat))))
