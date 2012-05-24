#lang racket/base

(require rackunit rackunit/text-ui rackunit/gui)
(require "tests/test-booleans.rkt"
         "tests/test-integers.rkt"
         "tests/test-datatypes.rkt"
         "tests/test-list-helpers.rkt"
         "tests/test-bounded-model-checking.rkt"
         "tests/test-arrays.rkt"
         "tests/test-sudoku.rkt"
         "tests/test-nqueens.rkt"
         "tests/test-numbermind.rkt")

(define total-failures 0)

(define-syntax-rule (run-test-suite suite)
  (begin
    (printf "~a: " 'suite)
    (set! total-failures (+ total-failures (run-tests suite)))))

(run-test-suite test-booleans)
(run-test-suite test-integers)
(run-test-suite test-datatypes)
(run-test-suite test-list-helpers)
(run-test-suite test-bounded-model-checking)
(run-test-suite test-arrays)
(run-test-suite test-sudoku)
(run-test-suite test-nqueens)
(run-test-suite test-numbermind)

(printf "Total failures: ~a~n" total-failures)
