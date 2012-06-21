(load "tests/tests-driver.scm")
;; (load "tests/tests-1.1-req.scm")
;; (load "tests/tests-1.2-req.scm")
(load "tests/tests-1.3-req.scm")

;;; immediates

(define fxshift 2)
(define fxmask #x03)
(define bool_f #x2F)
(define bool_t #x6F)
(define wordsize 4) ;byte

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))	  ;min fixnum
(define fxupper (- (expt 2 (- fixnum-bits 1)) 1)) ;max fixnum

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fxshift))
   ((boolean? x) (if x bool_t bool_f))
   ((char? x) (logior (ash (char->integer x) 8) #b1111))
   ((null? x) #b00111111)))

;;; unary functions 1.3
(define *is-prim* (make-object-property))
(define *emitter* (make-object-property))
(define *arg-count* (make-object-property))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (set! (*is-prim* 'prim-name) #t)
       (set! (*arg-count* 'prim-name)
	     (length '(arg* ...)))
       (set! (*emitter* 'prim-name)
	     (lambda (arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (*is-prim* x)))

(define (primitive-emitter x)
  (or (*emitter* x)
      (error 'primitive-emitter "it's not a primitive or has no emitter")))

(define (arg-count? x)
  (or (*arg-count* x)
      (error 'count-error "no arg-count")))

(define (primcall? expr)
  (and (pair? expr)
       (primitive? (car expr))))

(define (emit-primcall expr)
  (let ([prim (car expr)]
	[args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (emit-expr expr)
  (cond
   [(immediata? expr) (emit-immediate expr)]
   [(primcall? expr) (emit-primcall expr)]
   [else (error 'emit-expr "expression does not match supported ones")]))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "ret"))

(define (compile-program x port)
  (unless (immediate? x) (error 'immediaterror "not an immediate"))
  (emit port ".text")
  (emit port ".globl scheme_entry")
  (emit port ".type scheme_entry, @function")
  (emit port "scheme_entry:")
  (emit port "movl $~s, %eax" (immediate-rep x))
  (emit port "ret"))

(test-all)
