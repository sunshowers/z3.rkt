#lang racket

(require rackunit rackunit/text-ui)
; Load all files in the directory
(require "tests/smtlib2.rkt")

(run-tests smtlib2-tests)
