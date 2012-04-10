#lang racket
;; Indicates an instance of a datatype (e.g. (List Int) for List).
(struct datatype-instance (z3-sort fns))

;; A complex sort (e.g. List) has data about the base sort, a creator function
;; (which takes the base sort and a list of sort parameters to apply and produces
;; an immutable datatype-instance. We also want to cache instances for specific sort
;; parameters (so (List Int) followed by (List Int) should return the same
;; datatype-instance.
(struct z3-complex-sort (base-sort creator instance-hash))

;; Given a base sort and parameter sorts, get or create a parameterized
;; datatype.
(define (get-or-create-instance sort params)
  (define instance-hash (z3-complex-sort-instance-hash sort))
  (define ref (hash-ref instance-hash params #f))
  (if ref
      ref
      (let ([new-instance ((z3-complex-sort-creator sort) (z3-complex-sort-base-sort sort) params)])
        (hash-set! instance-hash params new-instance)
        new-instance)))

;; Curry a function application exactly *once*. The second time function
;; arguments are applied, the application is evaluated.
(define (curry-once fn . args)
  (lambda more-args
    ;(displayln (format "Calling function: ~a with args ~a" 'fn more-args))
    (apply fn (append args more-args))))

;; This is the prototype namespace for new contexts. It is added to by
;; define-builtin-symbol and define-builtin-proc below.
(define builtin-vals-eval-at-init (make-hash))
(define builtin-vals (make-hash))

(define-for-syntax (add-smt-suffix stx)
  (define suffixed-string (string-append (symbol->string (syntax->datum stx)) "/s"))
  (datum->syntax stx (string->symbol suffixed-string)))

(define-syntax (define-builtin-symbol stx)
  (syntax-case stx ()
    [(_ name fn)
     (with-syntax ([proc-stx (add-smt-suffix #'name)])
       #'(begin
           (define proc-stx 'name)
           (hash-set! builtin-vals-eval-at-init 'name fn)
           (provide proc-stx)))]))

(define-for-syntax (with-syntax-define-proc name-stx fn-stx)
  (with-syntax ([proc-stx (add-smt-suffix name-stx)]
                [name name-stx]
                [fn fn-stx])
    #'(begin
        (define (proc-stx . args) `(name ,@args))
        (hash-set! builtin-vals 'name fn)
        (provide proc-stx))))

(define-syntax (define-builtin-proc stx)
  (syntax-case stx ()
    [(_ name fn)
     (with-syntax-define-proc #'name #'fn)]
    [(_ name fn wrap)
     (with-syntax-define-proc #'name #'(λ (context . args) (apply (wrap (curry-once fn context)) args)))]))

(provide (struct-out datatype-instance)
         (struct-out z3-complex-sort)
         get-or-create-instance
         builtin-vals-eval-at-init
         builtin-vals
         define-builtin-symbol
         define-builtin-proc
         curry-once)
