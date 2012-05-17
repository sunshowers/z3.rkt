#lang racket

(require rackunit)
(require "../main.rkt")

(define/provide-test-suite test-datatypes
  (test-case
   "Test basic list operations"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun a () (List Int))
    (smt:declare-fun b () (List Int))
    (smt:assert (=/s a (cons/s 4 (cons/s 5 (nil/s)))))
    (smt:assert (not/s (= b (nil/s))))
    (smt:assert (< (head/s b) (- (head/s a) 2)))
    (smt:assert (= (tail/s b) a))
    (check-eq? (smt:check-sat) 'sat)
    (check-true (< (smt:eval (head/s b)) 2))
    (check-eq? (smt:eval (head/s (tail/s b))) 4)
    (check-eq? (smt:eval (head/s (tail/s (tail/s b)))) 5)))
  
  (test-case
   "Test defining scalars"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-datatypes () ((S A B C)))
    (smt:declare-fun a () S)
    (smt:declare-fun b () S)
    (smt:declare-fun c () S)
    (smt:assert (distinct/s a b c))
    (check-eq? (smt:check-sat) 'sat)
    (smt:declare-fun d () S)
    (smt:assert (distinct/s a b c d))
    (check-eq? (smt:check-sat) 'unsat))))
