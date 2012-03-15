#lang racket

(require rackunit)
(require "../smtlib2-parser.rkt")

(define/provide-test-suite test-booleans
  (test-case
   "Test one constraint"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun p () Bool)
    (check-eq? (smt:check-sat) 'sat)
    (smt:assert (= p true))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval p) 'true)))
  
  (test-case
   "Test a basic contradiction"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun p () Bool)
    (smt:assert (and p (not p)))
    (check-eq? (smt:check-sat) 'unsat)))
  
  (test-case
   "Test implies"
   (smt:with-context
    (smt:new-context-info)
    (smt:declare-fun p () Bool)
    (smt:declare-fun q () Bool)
    (smt:assert (implies p q))
    (check-eq? (smt:check-sat) 'sat)
    (smt:assert (= p true))
    (check-eq? (smt:check-sat) 'sat)
    (check-eq? (smt:eval q) 'true)
    (smt:assert (= q false))
    (check-eq? (smt:check-sat) 'unsat))))
