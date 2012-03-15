#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
; Load all files in the directory
(require "tests/test-booleans.rkt"
         "tests/test-integers.rkt")

(test/gui test-booleans test-integers #:wait? #t)
