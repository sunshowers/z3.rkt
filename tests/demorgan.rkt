#lang racket

(require "../api.rkt")

(define (demorgan)
  (let ([(ctx (make-regular-context))])
    (
