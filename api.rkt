#lang racket

(require (prefix-in z3: "z3-wrapper.rkt"))

(provide (all-from-out "z3-wrapper.rkt"))
(provide make-regular-context make-model-context)
(provide parse-smtlib2)

(define (make-regular-context)
  (z3:mk-context (make-config)))

(define (make-model-context)
  (z3:mk-context (make-config #:model? #t)))

;; Given a definition in smtlib2, parse it and return the AST formed.
(define (parse-smtlib2 ctx smtlib2-defn)
  (let ([o (open-output-string)])
    (for-each (lambda (l) (displayln l o)) smtlib2-defn)
    (let ([str (get-output-string o)])
      (z3:parse-smtlib2-string ctx str #() #() #() #()))))
