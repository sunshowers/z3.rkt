#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
(require "tests/test-booleans.rkt"
         "tests/test-integers.rkt"
;         "tests/test-datatypes.rkt"
;         "tests/test-arrays.rkt"
         "tests/test-sudoku.rkt"
;         "tests/test-nqueens.rkt"
         )

(run-tests
 (test-suite
  "Z3 API tests"
  test-booleans
  test-integers
;  test-datatypes
;  test-arrays
  test-sudoku
;  test-nqueens
))
