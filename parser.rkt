#lang racket/base

(require (prefix-in z3: "z3-wrapper.rkt")
         "utils.rkt"
         "builtins.rkt")
(require racket/match
         racket/contract/base)

(define (get-value id)
  (hash-ref (z3ctx-vals (current-context-info)) id))
(define (set-value id v)
  (hash-set! (z3ctx-vals (current-context-info)) id v))

;; The current model for this context. This is a mutable box.
(define (get-current-model)
  (define model (unbox (z3ctx-current-model (current-context-info))))
  (if (eq? model #f)
      (raise (make-exn:fail "No model found"))
      model))
(define (set-current-model! new-model)
  (set-box! (z3ctx-current-model (current-context-info)) new-model))

(define (make-config #:model? [model? #t])
  (let ([config (z3:mk-config)])
    (z3:set-param-value! config "MODEL" (if model? "true" "false"))
    config))

(define (new-context-info #:model? [model? #t])
  (define ctx (z3:mk-context (make-config #:model? model?)))
  (define vals (make-hash))
  (define sorts (make-hash))
  (define new-info (z3ctx ctx vals sorts (box #f)))
  (with-context
   new-info
   (init-builtins))
  new-info)

(define-syntax-rule (with-context info body ...)
  (parameterize ([current-context-info info])
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

;; Declare a new sort. num-params is currently ignored.
(define-syntax-rule (declare-sort sort num-params)
  (set-value 'sort
             (z3:mk-uninterpreted-sort (ctx) (make-symbol 'sort))))

;; sort-exprs are sort ids, (_ id parameter*), or (id sort-expr*).
(define (sort-expr->_z3-sort expr)
  (match expr
    [(list '_ id params ...) (apply (get-sort id) params)]
    [(list id args ...)
     (let ([sort (get-sort id)])
       ;; The sort can either be a complex sort which needs to be
       ;; instantiated, or a simple array sort.
       (if (z3-complex-sort? sort)
           (datatype-instance-z3-sort
            (get-or-create-instance (get-sort id) (map sort-expr->_z3-sort args)))
           (apply sort (map sort-expr->_z3-sort args))))]
    [id (get-sort id)]))

;; Given an expr, convert it to a Z3 AST. This is a really simple recursive descent parser.
(define (expr->_z3-ast expr)
  ;(displayln (format "IN: ~a" expr))
  (define ast (match expr
    ; Non-basic expressions
    [(list fn args ...) (apply (get-value fn) (cons (ctx) (map expr->_z3-ast args)))]
    ; Numerals
    [(? exact-integer?) (z3:mk-numeral (ctx) (number->string expr) (get-sort 'Int))]
    [(? inexact-real?) (z3:mk-numeral (ctx) (number->string expr) (get-sort 'Real))]
    ; Symbols
    [(? symbol?) (get-value expr)]
    ; Anything else
    [_ expr]))
  ;(displayln (format "Output: ~a ~a ~a" expr ast (z3:ast-to-string (ctx) ast)))
  ast)

;; Given a Z3 AST, convert it to an expression that can be parsed again into an AST,
;; assuming the same context. This is the inverse of expr->_z3-ast above.
(define (_z3-ast->expr ast)
  (read (open-input-string (z3:ast-to-string (ctx) ast))))

;; Make an uninterpreted function given arg sorts and return sort.
(define (make-uninterpreted name argsorts retsort)
  (let ([args (map sort-expr->_z3-sort argsorts)]
        [ret (sort-expr->_z3-sort retsort)])
    (if (= 0 (length args))
        (z3:mk-const (ctx) (make-symbol name) ret)
        (z3:mk-func-decl (ctx) (make-symbol name) args ret))))

;; Declare a new function. argsort is a sort-expr.
(define-syntax-rule (declare-fun fn args ...)
  (define fn (make-uninterpreted 'fn 'args ...)))

(define-syntax-rule (make-fun args ...)
  (make-uninterpreted (gensym) 'args ...))

(define-syntax-rule (make-fun/vector n args ...)
  (for/vector ([i (in-range 0 n)])
    (make-uninterpreted (gensym) 'args ...)))

(define-syntax-rule (make-fun/list n args ...)
  (for/list ([i (in-range 0 n)])
    (make-uninterpreted (gensym) 'args ...)))

;; We only support plain symbol for now
(define (constr->_z3-constructor expr)
  (z3:mk-constructor (ctx)
                     (make-symbol expr)
                     (z3:mk-string-symbol (ctx) (string-append "is-" (symbol->string expr)))
                     '()))

;; Declare a complex datatype. Currently one scalar type is supported.
;; param-types is currently ignored
(define-syntax-rule (declare-datatypes param-types ((stx-typename stx-args ...)))
  (let* ([typename `stx-typename]
         [args (list `stx-args ...)]
         [constrs (map constr->_z3-constructor args)]
         [datatype (z3:mk-datatype (ctx) (make-symbol 'typename) constrs)])
    (new-sort typename datatype)
    (for-each
     (lambda (constr-name constr)
       (let-values ([(constr-fn tester-fn accessor-fns)
                     (z3:query-constructor (ctx) constr 0)]) ; XXX handle > 0
         (set-value constr-name (z3:mk-app (ctx) constr-fn '()))))
     args constrs)))

(define (assert expr)
  (z3:assert-cnstr (ctx) (expr->_z3-ast expr)))

(define (check-sat)
  (define-values (rv model) (z3:check-and-get-model (ctx)))
  (set-current-model! model)
  rv)

(define (get-model)
  (get-current-model))

(define (eval-in-model model expr)
  (define-values (rv ast) (z3:eval (ctx) model (expr->_z3-ast expr)))
  (if (eq? rv #f)
      (raise (make-exn:fail "Evaluation failed"))
      (_z3-ast->expr ast)))

(define (smt:eval expr)
  (eval-in-model (get-current-model) expr))

;; XXX need to implement a function to get all models. To do that we need
;; push, pop, and a way to navigate a model.

(provide
 (prefix-out
  smt:
  (combine-out
   with-context
   new-context-info
   declare-datatypes
   declare-sort
   declare-fun
   make-fun
   make-fun/vector
   make-fun/list
   assert
   check-sat
   get-model))
 smt:eval
 (prefix-out smt: (contract-out
                   [set-logic (-> symbol? any)])))
