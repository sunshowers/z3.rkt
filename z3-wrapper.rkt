#lang racket

(require mzlib/foreign) (unsafe!)
(require racket/runtime-path)
(require ffi/cvector)

; We need this because libz3 is stupid and doesn't mention a dependence on
; libgomp. Loading this causes libz3 to pick up libgomp and thus not error out.
(define libgomp (ffi-lib "libgomp" '["1" #f]))

(define-runtime-path libz3-path "z3/lib/libz3-gmp")
(define libz3 (ffi-lib libz3-path))

(define-cpointer-type _z3-config)
(define-cpointer-type _z3-context)
(define-cpointer-type _z3-symbol)
(define-cpointer-type _z3-ast)
(define-cpointer-type _z3-sort)
(define-cpointer-type _z3-app)
(define-cpointer-type _z3-pattern)
(define-cpointer-type _z3-model)
(define-cpointer-type _z3-func-decl)

;; Enumerations
(define _z3-lbool (_enum '(false = -1 undef true)))
(define _z3-ast-kind (_enum '(numeral app var quantifier unknown = 1000)))

(define _z3-error-handler (_fun #:keep #t _int -> _void))

(define-syntax defz3
  (syntax-rules (:)
    [(_ name : type ...)
     (begin
       (define name
         (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_")
                                               (#rx"^" "Z3_")
                                               (#rx"!$" "")))
                      libz3 (_fun type ...)))
       (provide name))]))

(defz3 mk-config : -> _z3-config)
(defz3 set-param-value! : _z3-config _string _string -> _void)
(defz3 mk-context : _z3-config -> _z3-context)
(defz3 mk-string-symbol : _z3-config _string -> _z3-symbol)

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
  (sort-names : (_or-null (_vector i _z3-symbol)))
  (sorts : (_or-null (_vector i _z3-sort)))
  (_uint = (vector-multilength decl-names decls))
  (decl-names : (_or-null (_vector i _z3-symbol)))
  (decls : (_or-null (_vector i _z3-func-decl)))
  -> _z3-ast)

;; -> string functions
(defz3 context-to-string : _z3-context -> _string)
(defz3 ast-to-string : _z3-context _z3-ast -> _string)
(defz3 model-to-string : _z3-context _z3-model -> _string)

(defz3 assert-cnstr : _z3-context _z3-ast -> _void)
(defz3 check : _z3-context -> _z3-lbool)
(defz3 check-and-get-model : _z3-context (model : (_ptr o (_or-null _z3-model))) -> (rv : _z3-lbool) -> (values rv model))
(defz3 eval : _z3-context _z3-model _z3-ast (v : (_ptr o (_or-null _z3-ast))) -> (rv : _z3-lbool) -> (values rv v))
(defz3 get-ast-kind : _z3-context _z3-ast -> _z3-ast-kind)
(defz3 get-numeral-string : _z3-context _z3-ast -> _string)
(defz3 to-app : _z3-context _z3-ast -> _z3-app)
(defz3 get-app-num-args : _z3-context _z3-app -> _uint)
(defz3 get-app-decl : _z3-context _z3-app -> _z3-func-decl)
