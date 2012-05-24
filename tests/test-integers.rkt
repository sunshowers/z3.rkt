#lang racket

(require rackunit)
(require "../main.rkt")

(define/provide-test-suite test-integers
  (test-case
   "Test >, <, and +"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun a () Int)
    (smt:assert (>/s a 10))
    (smt:assert (</s a 20))
    (check-eq? (smt:check-sat) 'sat)
    (define a-eval (smt:eval a))
    (check-true (and (> a-eval 10) (< a-eval 20)))
    (smt:assert (=/s (+/s a 5) 14))
    (check-eq? (smt:check-sat) 'unsat)))

  (test-case
   "Test a quantifier over integers"
   (smt:with-context
    (smt:new-context-info #:mbqi? #t)
    (smt:declare-fun f (Int Int) Int)
    (smt:declare-fun a () Int)
    (smt:declare-fun b () Int)
    (smt:assert (forall/s ((x Int)) (>=/s (f x x) (+/s x a))))
    (smt:assert (</s (f a b) a))
    (smt:assert (>/s a 0))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval (f (+/s a 10) 20)) 12))))
