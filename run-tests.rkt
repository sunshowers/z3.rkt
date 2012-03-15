#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
; Load all files in the directory
(require "tests/test-booleans.rkt"
         "tests/test-integers.rkt")

((make-gui-runner) test-booleans test-integers)
