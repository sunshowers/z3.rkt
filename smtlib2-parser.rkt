#lang racket

(require "api.rkt")

; This must be parameterized every time any syntax is used
(define ctx (make-parameter #f))
(define (current-context) (ctx))
(define ctx-namespace (make-parameter #f))

(define (get-value id)
  (namespace-variable-value id #t #f (ctx-namespace)))
(define (set-value id v)
  (namespace-set-variable-value! id v #t (ctx-namespace)))

(struct z3-context-info (context namespace))

;; Wraps a binary function so that arguments are processed
;; in a right-associative manner.
(define (rassoc fn)
  (lambda args
    (foldr fn (last args) (drop-right args 1))))

(define (flip fn) (lambda (x y) (fn y x)))

;; Wraps a binary function so that arguments are processed
;; in a left-associative manner. Note that foldl calls functions
;; in their reverse order, so we flip the arguments to fix that.
(define (lassoc fn)
  (lambda (fst . rst)
    (foldl (flip fn) fst rst)))

(define (chainable fn)
  (lambda (fst . rst)
    (apply z3:mk-and (foldl (flip fn) fst rst))))

(define (new-context-info model?)
  (define ctx (z3:mk-context (make-config #:model? model?)))
  (define ns (make-base-namespace))
  (for-each (lambda (arg)
              (namespace-set-variable-value! (first arg) (second arg) #t ns))
            `([Bool ,(z3:mk-bool-sort ctx)]
              [Int ,(z3:mk-int-sort ctx)]
              [Real ,(z3:mk-real-sort ctx)]
              [BitVec ,(z3:mk-bv-sort ctx)]
              [true ,(z3:mk-true ctx)]
              [false ,(z3:mk-false ctx)]
              [= ,(chainable (z3:mk-eq ctx))]
              [not ,(z3:mk-not ctx)]
              [ite ,(z3:mk-ite ctx)]
              [iff ,(z3:mk-iff ctx)]
              [=> ,(rassoc (z3:mk-implies ctx))]
              [xor ,(lassoc (z3:mk-xor ctx))]
              [and ,(lassoc (z3:mk-and ctx))]
              [or ,(lassoc (z3:mk-or ctx))]
              ; These functions already accept an arbitrary number of arguments
              [+ ,(z3:mk-add ctx)]
              [* ,(z3:mk-mul ctx)]
              [- ,(z3:mk-sub ctx)]
              ; These don't
              [/ ,(lassoc (z3:mk-div ctx))]
              [div ,(lassoc (z3:mk-div ctx))]
              [mod ,(lassoc (z3:mk-mod ctx))]
              [rem ,(lassoc (z3:mk-rem ctx))]
              ; Comparisons are chainable (i.e. (< a b c) == (and (< a b) (< b c)))
              [< ,(chainable (z3:mk-lt ctx))]
              [<= ,(chainable (z3:mk-le ctx))]
              [> ,(chainable (z3:mk-gt ctx))]
              [> ,(chainable (z3:mk-ge ctx))]))
  (z3-context-info ctx ns))

(define-syntax-rule (with-context info body ...)
  (parameterize ([ctx (z3-context-info-context info)]
                 [ctx-namespace (z3-context-info-namespace info)])
    body ...))

;; Handle the next error.
(define (handle-next-error)
  (define err (z3:get-error-code (ctx)))
  (raise-user-error "~s" (z3:get-error-msg err)))

;; Set the logic for this context. This can only be called once.
(define (set-logic logic)
  (let ([rv
         (z3:set-logic (ctx) (symbol->string logic))])
    (when (not rv) (handle-next-error))))

;; Helper function to make a symbol with the given name (Racket symbol)
(define (make-symbol symbol-name)
  (z3:mk-string-symbol (ctx) (symbol->string symbol-name)))
   
;; Declare a new sort. num-params is currently ignored.
(define-syntax-rule (declare-sort sort num-params)
  (set-value 'sort
             (z3:mk-uninterpreted-sort (ctx) (make-symbol 'sort))))

;; sort-exprs are either sort ids or (id sort-expr*).
(define (sort-expr->z3-sort expr)
  (match (syntax->datum expr)
    [(list '_ id args ...) (apply (get-value id) args)]
    [id (get-value id)]))

;; Declare a new function. argsort is a sort-expr.
(define-syntax (declare-fun stx)
  (syntax-case stx ()
    [(declare-fun fn (argsort ...) retsort)
     #'(let ([args (vector (sort-expr->z3-sort #'argsort) ...)]
             [ret (sort-expr->z3-sort #'retsort)])
         (set-value 'fn (z3:mk-func-decl (ctx) (make-symbol 'fn) args ret))
         (void))]))

(provide current-context
         with-context
         new-context-info
         declare-sort
         declare-fun
         (contract-out
          [set-logic (-> symbol? any)]))
