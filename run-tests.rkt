#lang racket

(require rackunit rackunit/text-ui)
; Load all files in the directory
(require "tests/test-booleans.rkt")

(run-tests test-booleans)
