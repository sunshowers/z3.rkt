#lang racket

(require rackunit)
(require "../smtlib2-parser.rkt")

(define/provide-test-suite test-datatypes
  (test-case
   "Test basic list operations"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun a () (List Int))
    (smt:declare-fun b () (List Int))
    (smt:assert (= a (cons 4 (cons 5 (nil)))))
    (smt:assert (not (= b (nil))))
    (smt:assert (< (head b) (- (head a) 2)))
    (smt:assert (= (tail b) a))
    (check-eq? (smt:check-sat) 'sat)
    (check-true (< (smt:eval (head b)) 2))
    (check-eq? (smt:eval (head (tail b))) 4)
    (check-eq? (smt:eval (head (tail (tail b)))) 5)))
  
  (test-case
   "Test defining scalars"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-datatypes () ((S A B C)))
    (smt:declare-fun a () S)
    (smt:declare-fun b () S)
    (smt:declare-fun c () S)
    (smt:assert (distinct a b c))
    (check-eq? (smt:check-sat) 'sat)
    (smt:declare-fun d () S)
    (smt:assert (distinct a b c d))
    (check-eq? (smt:check-sat) 'unsat))))
