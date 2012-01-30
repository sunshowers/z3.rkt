#lang racket

(require "api.rkt")

; This must be parameterized every time any syntax is used
(define ctx (make-parameter #f))
(define (current-context) (ctx))
(define current-symbol-table (make-parameter #f))

(struct z3-context-info (context symbol-table))

(define (new-context-info model?)
  (define ctx (z3:mk-context (make-config #:model? model?)))
  (define st (make-hash))
  (z3-context-info ctx st))

(define-syntax-rule (make-with-context fn ((builtin-name builtin-expr) ...))
  (define-syntax (fn stx)
    (syntax-case stx ()
      [(_ z3-context-info body (... ...))
       (with-syntax ([builtin-name (datum->syntax stx 'builtin-name)] ...)
         #'(parameterize ([ctx (z3-context-info-context z3-context-info)]
                          [current-symbol-table (z3-context-info-symbol-table z3-context-info)])
           (let ([builtin-name (builtin-expr (ctx))] ...)
             body (... ...))))])))

(make-with-context with-context
                   ([Bool z3:mk-bool-sort]
                    [Int z3:mk-int-sort]
                    [Real z3:mk-int-sort]
                    [BitVec (lambda (ctx2) (lambda (size) (z3:mk-bv-sort ctx2 size)))]))

;; Handle the next error.
(define (handle-next-error)
  (define err (z3:get-error-code (ctx)))
  (raise-user-error "~s" (z3:get-error-msg err)))

;; Set the logic for this context. This can only be called once.
(define (set-logic logic)
  (let ([rv
         (z3:set-logic (ctx) (symbol->string logic))])
    (when (not rv) (handle-next-error))))

;; Helper function to make a symbol with the given name (syntax object)
(define (make-symbol symbol-name)
  (z3:mk-string-symbol (ctx) (symbol->string (syntax-e symbol-name))))
   
;; Declare a new sort. num-params is currently ignored.
(define-syntax (declare-sort stx)
  (syntax-case stx ()
    [(declare-sort sort num-params)
     #'(define sort (z3:mk-uninterpreted-sort (ctx) (make-symbol #'sort)))]))

;; sort-exprs are either sort ids or (id sort-expr*).
(define-syntax (sort-expr->z3-sort stx)
  (syntax-case stx ()
    [(op (_ id args ...)) #'(id args ...)]
    [(op id) #'id]))

;; Declare a new function. argsort is a sort-expr.
(define-syntax (declare-fun stx)
  (syntax-case stx ()
    [(declare-fun fn (argsort ...) retsort)
     #'(let ([args (vector (sort-expr->z3-sort argsort) ...)]
             [ret (sort-expr->z3-sort retsort)])
         (define fn (z3:mk-func-decl (ctx) (make-symbol #'fn) args ret))
         (void))]))


(provide current-context
         with-context
         new-context-info
         declare-sort
         declare-fun
         (contract-out
          [set-logic (-> symbol? any)]))
