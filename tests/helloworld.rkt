#lang racket

(require "../api.rkt")

(define (helloworld)
  (let ([config (make-config #:model? #t)])
    (z3:context-to-string (z3:mk-context config))))

(display (helloworld))
