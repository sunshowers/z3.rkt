#lang racket

(require mzlib/foreign) (unsafe!)
(require racket/runtime-path)
(require "defs.rkt")

; We need this because libz3 is stupid and doesn't mention a dependence on
; libgomp. Loading this causes libz3 to pick up libgomp and thus not error out.
(define libgomp (ffi-lib "libgomp" '["1" #f]))

(define-runtime-path libz3-path "z3/lib/libz3-gmp")
(define libz3 (ffi-lib libz3-path))

(define-cpointer-type _z3-config)
(define-cpointer-type _z3-context)
(define-cpointer-type _z3-symbol)
(define-cpointer-type _z3-ast)

; We distinguish between different kinds of asts here to add a bit of
; type checking
(define-cpointer-type _z3-bool-ast _z3-ast)
(define-cpointer-type _z3-int-ast _z3-ast)
(define-cpointer-type _z3-real-ast _z3-ast)
(define-cpointer-type _z3-bv-ast _z3-ast)
(define-cpointer-type _z3-app-ast _z3-ast)
(define-cpointer-type _z3-var-ast _z3-ast)
(define-cpointer-type _z3-quantifier-ast _z3-ast)

; We distinguish between the different kinds of sorts here to add a bit of
; type checking. Note that _z3-bool-sort is NOT _z3-bool-ast, etc.
; _z3-bool-sort is the bool type while _z3-bool-ast is a variable or
; expression of that type.
(define-cpointer-type _z3-sort _z3-ast)
(define-cpointer-type _z3-uninterpreted-sort _z3-sort)
(define-cpointer-type _z3-bool-sort _z3-sort)
(define-cpointer-type _z3-int-sort _z3-sort)
(define-cpointer-type _z3-real-sort _z3-sort)
(define-cpointer-type _z3-bv-sort _z3-sort)
(define-cpointer-type _z3-array-sort _z3-sort)
(define-cpointer-type _z3-datatype-sort _z3-sort)
(define-cpointer-type _z3-relation-sort _z3-sort)
(define-cpointer-type _z3-finite-domain-sort _z3-sort)
(define-cpointer-type _z3-unknown-sort _z3-sort)

; An association between sorts, func-decls and asts.
(define sort-decl-ast '([z3-int-sort z3-int-decl z3-int-ast]
                        [z3-bool-sort z3-bool-decl z3-bool-ast]
                        [z3-real-sort z3-real-decl z3-real-ast]
                        [z3-bv-sort z3-bv-decl z3-bv-ast]))

(define-cpointer-type _z3-app _z3-ast)

(define-cpointer-type _z3-pattern)
(define-cpointer-type _z3-model)

; Function declarations. The subtypes indicate return types
(define-cpointer-type _z3-func-decl _z3-ast)

;; Enumerations
(define _z3-lbool (_enum '(false = -1 undef true)))
(define _z3-sat-lbool (_enum '(unsat = -1 unknown sat)))
(define _z3-ast-kind (_enum '(numeral app var quantifier unknown = 1000)))
(define _z3-error-code (_enum '(ok sort-error iob invalid-arg parser-error
                                   no-parser invalid-pattern memout-fail
                                   file-access-error invalid-usage
                                   internal-fatal dec-ref-error)))

(define _z3-error-handler (_fun #:keep #t _int -> _void))

(define-syntax defz3
  (syntax-rules (:)
    [(_ name : type ...)
     (begin
       (define (name . args)
         (displayln (format "Calling (internal) function: ~a" 'name))
         (apply (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_")
                                                      (#rx"^" "Z3_")
                                                      (#rx"!$" "")))
                             libz3 (_fun type ...)) args))
       (provide name))]))

(defz3 mk-config : -> _z3-config)
(defz3 set-param-value! : _z3-config _string _string -> _void)
(defz3 mk-context : _z3-config -> _z3-context)

(defz3 set-logic : _z3-context _string -> _bool)

(defz3 mk-string-symbol : _z3-context _string -> _z3-symbol)
(defz3 mk-uninterpreted-sort : _z3-context _z3-symbol -> _z3-uninterpreted-sort)
(defz3 mk-bool-sort : _z3-context -> _z3-bool-sort)
(defz3 mk-int-sort : _z3-context -> _z3-int-sort)
(defz3 mk-real-sort : _z3-context -> _z3-real-sort)
(defz3 mk-bv-sort : _z3-context _uint -> _z3-bv-sort)

(defz3 mk-list-sort : _z3-context _z3-symbol _z3-sort
  (nil-decl : (_ptr o _z3-func-decl))
  (is-nil-decl : (_ptr o _z3-func-decl))
  (cons-decl : (_ptr o _z3-func-decl))
  (is-cons-decl : (_ptr o _z3-func-decl))
  (head-decl : (_ptr o _z3-func-decl))
  (tail-decl : (_ptr o _z3-func-decl)) ->
  (res : _z3-sort) ->
  (datatype-instance res (hash 'nil nil-decl
                               'is-nil is-nil-decl
                               'insert cons-decl
                               'is-insert is-cons-decl
                               'head head-decl
                               'tail tail-decl)))

(defz3 mk-true : _z3-context -> _z3-bool-ast)
(defz3 mk-false : _z3-context -> _z3-bool-ast)
(defz3 mk-eq : _z3-context _z3-ast _z3-ast -> _z3-bool-ast)

; Helper macro to define n-ary AST functions
(define-syntax define-nary
  (syntax-rules (: ->)
    [(_ fn : argtype -> rettype)
     (defz3 fn : (ctx . args) ::
       (ctx : _z3-context)
       (_uint = (length args))
       (args : (_list i argtype)) -> rettype)]))

(define-nary mk-distinct : _z3-ast -> _z3-bool-ast)

; Boolean operations
(defz3 mk-not : _z3-context _z3-bool-ast -> _z3-bool-ast)
(defz3 mk-ite : _z3-context _z3-bool-ast _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-iff : _z3-context _z3-ast _z3-bool-ast -> _z3-ast)
(defz3 mk-implies : _z3-context _z3-bool-ast _z3-bool-ast -> _z3-bool-ast)
(defz3 mk-xor : _z3-context _z3-bool-ast _z3-bool-ast -> _z3-bool-ast)
(define-nary mk-and : _z3-bool-ast -> _z3-bool-ast)
(define-nary mk-or : _z3-bool-ast -> _z3-bool-ast)

; Arithmetic operations
(define-nary mk-add : _z3-int-ast -> _z3-int-ast)
(define-nary mk-mul : _z3-int-ast -> _z3-int-ast)
(define-nary mk-sub : _z3-int-ast -> _z3-int-ast)
(defz3 mk-div : _z3-context _z3-int-ast _z3-int-ast -> _z3-int-ast)
(defz3 mk-mod : _z3-context _z3-int-ast _z3-int-ast -> _z3-int-ast)
(defz3 mk-rem : _z3-context _z3-int-ast _z3-int-ast -> _z3-int-ast)

; Comparisons
(defz3 mk-lt : _z3-context _z3-int-ast _z3-int-ast -> _z3-bool-ast)
(defz3 mk-le : _z3-context _z3-int-ast _z3-int-ast -> _z3-bool-ast)
(defz3 mk-gt : _z3-context _z3-int-ast _z3-int-ast -> _z3-bool-ast)
(defz3 mk-ge : _z3-context _z3-int-ast _z3-int-ast -> _z3-bool-ast)

; Numerals
(defz3 mk-numeral : _z3-context _string _z3-sort -> _z3-int-ast)

(defz3 mk-func-decl :
  (ctx s domain range) ::
  (ctx : _z3-context)
  (s : _z3-symbol)
  (_uint = (vector-length domain))
  (domain : (_vector i _z3-sort))
  (range : _z3-sort) ->
  (decl : _z3-func-decl) ->
  (begin
    (for-each (match-lambda [(list sort-tag decl-tag _)
      (when (cpointer-has-tag? range sort-tag)
        (cpointer-push-tag! decl decl-tag))])
              sort-decl-ast)
    decl))

(defz3 mk-app : (ctx d args) ::
  (ctx : _z3-context)
  (d : _z3-func-decl)
  (_uint = (length args))
  (args : (_list i _z3-ast)) -> _z3-ast)

(defz3 mk-const :
  (ctx s sort) ::
  (ctx : _z3-context)
  (s : _z3-symbol)
  (sort : _z3-sort) ->
  (app : _z3-app) ->
  (begin
    (for-each (match-lambda [(list sort-tag _ ast-tag)
      (when (cpointer-has-tag? sort sort-tag)
        (cpointer-push-tag! app ast-tag))])
              sort-decl-ast)
    app))

(define (vector-multilength v1 v2)
  (cond
   [(and (not v1) (not v2)) 0]
   [else
    (let ([l1 (vector-length v1)])
      (if (equal? l1 (vector-length v2))
          l1
          (raise-user-error "vector lengths don't match")))]))

(defz3 parse-smtlib2-string :
  (ctx smtlib2-defn sort-names sorts decl-names decls) ::
  (ctx : _z3-context)
  (smtlib2-defn : _string)
  (_uint = (vector-multilength sort-names sorts))
  (sort-names : (_vector i _z3-symbol))
  (sorts : (_vector i _z3-sort))
  (_uint = (vector-multilength decl-names decls))
  (decl-names : (_vector i _z3-symbol))
  (decls : (_vector i _z3-func-decl))
  -> _z3-ast)

;; -> string functions
(defz3 context-to-string : _z3-context -> _string)
(defz3 ast-to-string : _z3-context _z3-ast -> _string)
(defz3 model-to-string : _z3-context _z3-model -> _string)
(defz3 sort-to-string : _z3-context _z3-sort -> _string)
(defz3 func-decl-to-string : _z3-context _z3-func-decl -> _string)

;; error handling functions
(defz3 get-error-code : _z3-context -> _z3-error-code)
(defz3 get-error-msg : _z3-error-code -> _string)

(defz3 assert-cnstr : _z3-context _z3-ast -> _void)
(defz3 check : _z3-context -> _z3-sat-lbool)
(defz3 check-and-get-model : _z3-context (model : (_ptr o (_or-null _z3-model))) -> (rv : _z3-sat-lbool) -> (values rv model))
(defz3 eval : _z3-context _z3-model _z3-ast (v : (_ptr o (_or-null _z3-ast))) -> (rv : _bool) -> (values rv v))
(defz3 get-ast-kind : _z3-context _z3-ast -> _z3-ast-kind)
(defz3 get-numeral-string : _z3-context _z3-ast -> _string)
(defz3 to-app : _z3-context _z3-ast -> _z3-app)
(defz3 get-app-num-args : _z3-context _z3-app -> _uint)
(defz3 get-app-decl : _z3-context _z3-app -> _z3-func-decl)
