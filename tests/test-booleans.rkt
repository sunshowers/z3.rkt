#lang racket

(require rackunit)
(require "../main.rkt")

(define/provide-test-suite test-booleans
  (test-case
   "Test one constraint"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun p () Bool)
    (check-eq? (smt:check-sat) 'sat)
    (smt:assert (=/s p #t))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval p) 'true)))
  
  (test-case
   "Test a basic contradiction"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun p () Bool)
    (smt:assert (and/s p (not/s p)))
    (check-eq? (smt:check-sat) 'unsat)))
  
  (test-case
   "Test implies"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun p () Bool)
    (smt:declare-fun q () Bool)
    (smt:assert (=>/s p q))
    (check-eq? (smt:check-sat) 'sat)
    (smt:assert (=/s p #t))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval q) 'true)
    (smt:assert (=/s q #f))
    (check-eq? (smt:check-sat) 'unsat)))
  
  (test-case
   "Test a quantifier over all booleans"
   (smt:with-context
    (smt:new-context-info #:mbqi? #t)
    (smt:assert (forall/s ((x Bool)) x))
    (smt:declare-fun p () Bool)
    (smt:assert (not/s p))
    (check-eq? (smt:check-sat) 'unsat))))
