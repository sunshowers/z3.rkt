#lang racket/base

(require (prefix-in z3: "z3-wrapper.rkt")
         "defs.rkt")
(require racket/list
         racket/match
         racket/contract/base)

(define (get-value id)
  (hash-ref (z3ctx-vals (current-context-info)) id))
(define (set-value id v)
  (hash-set! (z3ctx-vals (current-context-info)) id v))

;; A symbol table for sorts
(define (get-sort id)
  (hash-ref (z3ctx-sort-table (current-context-info)) id))
(define (new-sort id v)
  (define sort-table (z3ctx-sort-table (current-context-info)))
  (if (not (hash-ref sort-table id #f))
      (hash-set! sort-table id v)
      (raise (make-exn:fail "Defining a pre-existing sort!"))))

;; The current model for this context. This is a mutable box.
(define (get-current-model)
  (define model (unbox (z3ctx-current-model (current-context-info))))
  (if (eq? model #f)
      (raise (make-exn:fail "No model found"))
      model))
(define (set-current-model! new-model)
  (set-box! (z3ctx-current-model (current-context-info)) new-model))

;; Lists are a builtin complex sort. z3:mk-list-sort already returns a
;; datatype-instance.
(define (make-list-sort base-sort params)
  (if (= (length params) 1)
      (z3:mk-list-sort (ctx) (make-symbol (gensym)) (car params))
      (raise (make-exn:fail "List sort should have just one parameter!"))))

;; Creates a new complex sort. This adds hooks for each constructor to the namespace
;; provided and returns the sort.
;; XXX handle hooks properly. We should have some sort of tag in place on every
;; variable to figure out what instance function to call. Right now we just take the
;; first element.
(define (make-complex-sort base-sort creator hash hook-ids)
  (let* ([instance-hash (make-hash)]
         [res (z3-complex-sort base-sort creator instance-hash)])
    (for-each
     (lambda (hook)
       (let ([hook-fn
              (lambda args
                (let ([z3-fn (hash-ref (datatype-instance-fns (car (hash-values instance-hash))) hook)])
                  (z3:mk-app (ctx) z3-fn args)))])
         (hash-set! hash hook hook-fn)))
     hook-ids)
    res))

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

(define-syntax-rule (builtin var fn)
  (list 'var (fn (ctx))))

(define-syntax builtin-curried
  (syntax-rules ()
    [(_ var fn) (list 'var (curry-once fn (ctx)))]
    [(_ var fn wrap) (list 'var (wrap (curry-once fn (ctx))))]))

(define (make-config #:model? [model? #t])
  (let ([config (z3:mk-config)])
    (z3:set-param-value! config "MODEL" (if model? "true" "false"))
    config))

(define-builtin-symbol true z3:mk-true)
(define-builtin-symbol false z3:mk-false)
(define-builtin-proc = z3:mk-eq)
(define-builtin-proc distinct z3:mk-distinct)
(define-builtin-proc not z3:mk-not)
(define-builtin-proc ite z3:mk-ite)
(define-builtin-proc iff z3:mk-iff)
(define-builtin-proc implies z3:mk-implies rassoc)
(define-builtin-proc xor z3:mk-xor lassoc)
;; These functions already accept an arbitrary number of arguments
(define-builtin-proc and z3:mk-and)
(define-builtin-proc or z3:mk-or)
(define-builtin-proc + z3:mk-add)
(define-builtin-proc * z3:mk-mul)
(define-builtin-proc - z3:mk-sub)
;; These don't
(define-builtin-proc / z3:mk-div lassoc)
(define-builtin-proc div z3:mk-div lassoc)
(define-builtin-proc mod z3:mk-mod lassoc)
(define-builtin-proc rem z3:mk-rem lassoc)
;; XXX Comparisons are chainable (i.e. (< a b c) == (and (< a b) (< b c)))
(define-builtin-proc < z3:mk-lt)
(define-builtin-proc <= z3:mk-le)
(define-builtin-proc > z3:mk-gt)
(define-builtin-proc >= z3:mk-ge)
;; Array operations
(define-builtin-proc select z3:mk-select)
(define-builtin-proc store z3:mk-store)

(define (new-context-info #:model? [model? #t])
  (define ctx (z3:mk-context (make-config #:model? model?)))
  (define vals (hash-copy builtin-vals))
  ;; Evaluate whatever values are supposed to be evaluated at initialization time
  (define sorts (make-hash))
  (define new-info (z3ctx ctx vals sorts (box #f)))
  (with-context
   new-info
   (for ([(k fn) (in-hash builtin-vals-eval-at-init)])
     (hash-set! vals k (fn ctx)))
   ;; Sorts go into a separate table
   (for ([sort
          (in-list
           (list
            (builtin Bool z3:mk-bool-sort)
            (builtin Int z3:mk-int-sort)
            ;; The base sort for lists is irrelevant
            (list 'List (make-complex-sort #f make-list-sort vals '(nil is-nil cons is-cons head tail)))
            (builtin-curried Array z3:mk-array-sort)))])
     (new-sort (first sort) (second sort))))
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

;; Helper function to make a symbol with the given name (Racket symbol)
(define (make-symbol symbol-name)
  (z3:mk-string-symbol (ctx) (symbol->string symbol-name)))

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
