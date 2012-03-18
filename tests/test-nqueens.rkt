#lang racket

(require rackunit)
(require "../examples/nqueens.rkt")

;; 0 is ignored, we start from 1
(define nqueen-solutions '(#f 1 0 0 2 10 4 40 92 352 724))

(define/provide-test-suite test-nqueens
  (for ([(iqueens i) (in-indexed nqueen-solutions)]
        #:when (> i 0))
    (check-eq? (solve-nqueens i) iqueens)))
