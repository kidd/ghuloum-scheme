(load "tests/tests-driver.scm")
;; (load "tests/tests-1.1-req.scm")
;; (load "tests/tests-1.2-req.scm")
;; (load "tests/tests-1.3-req.scm")
;; (load "tests/tests-1.4-req.scm")
(load "tests/tests-1.5-req.scm")

(define *is-prim* (make-object-property))
(define *emitter* (make-object-property))
(define *arg-count* (make-object-property))
(define *port* #f)

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si arg* ...) b b* ...)
     (begin
       (set! (*is-prim* 'prim-name) #t)
       (set! (*arg-count* 'prim-name)
	     (length '(arg* ...)))
       (set! (*emitter* 'prim-name)
	     (lambda (si arg* ...) b b* ...)))]))

;;; immediates

(define fxshift 2)
(define charshift 8)

(define fxmask #x03)
(define fxtag #b00000000)

(define bool-f #x2F)
(define bool-t #x6F)
(define wordsize 4) ;byte
(define null-b  #b00111111)

(define fixnum-bits (- (* wordsize 8) fxshift))
(define bool-bit wordsize)
(define fxlower (- (expt 2 (- fixnum-bits 1))))	  ;min fixnum
(define fxupper (- (expt 2 (- fixnum-bits 1)) 1)) ;max fixnum
(define chartag #b00001111)


(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fxshift))
   ((boolean? x) (if x bool-t bool-f))
   ((char? x) (logior (ash (char->integer x) 8) #b1111))
   ((null? x) null-b)))

;;; unary functions 1.3

(define (primitive? x)
  (and (symbol? x)
       (*is-prim* x)))

(define (primitive-emitter x)
  (or (*emitter* x)
      (error 'primitive-emitter "it's not a primitive or has no emitter")))

(define (arg-count? x)
  (or (*arg-count* x)
      (error 'count-error "no arg-count")))

(define (primcall? expr)
  (and (pair? expr)
       (primitive? (car expr))))

(define (check-primcall-args prim args)
  (if (= (arg-count? prim) (length args)) #t
      (error 'check-primcall-args "incorrect number of actual parameters")))

;;; emitters
(define (emit-primcall si expr)
  (let ([prim (car expr)]
	[args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define (emit-immediate expr port)
  (emit port "movl $~s, %eax" (immediate-rep expr)))

(define (emit-expr port si expr)
  (set! *port* port)
  (cond
   [(immediate? expr) (emit-immediate expr port)]
   [(primcall? expr) (emit-primcall si expr)]
   [(if? expr) (emit-if si expr)]
   [else (error 'emit-expr "expression does not match supported ones")]))

(define (emit-program expr port)
  (emit port ".text")
  (emit-function-header port "L_scheme_entry")
  (emit-expr port (- wordsize) expr)
  (emit port "ret")
  (emit-function-header port "scheme_entry")
  (emit port "movl %esp, %ecx")
  (emit port "movl 4(%esp), %esp")
  (emit port "call L_scheme_entry")
  (emit port "movl %ecx, %esp")
  (emit port "ret"))

(define (emit-function-header port label)
  (emit port ".globl ~a" label)
  (emit port ".type ~a, @function" label)
  (emit port "~a:" label))

;; (define (compile-program x port)
;;   (unless (immediate? x) (error 'immediaterror "not an immediate"))
;;   (emit port ".text")
;;   (emit port ".globl scheme_entry")
;;   (emit port ".type scheme_entry, @function")
;;   (emit port "scheme_entry:")
;;   (emit port "movl $~s, %eax" (immediate-rep x))
;;   (emit port "ret"))

(define-primitive (fxadd1 si arg)
  (emit-expr *port* si arg)
  (emit *port* "addl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 si arg)
  (emit-expr *port* si arg)
  (emit *port* "subl $~s, %eax" (immediate-rep 1)))

(define-primitive (fixnum->char si arg)
  (emit-expr *port* si arg)
  (emit *port* "shll $~s, %eax" (- charshift fxshift))
  (emit *port* "orl $~s, %eax" chartag))

(define-primitive (char->fixnum si arg)
  (emit-expr *port* si arg)
  (emit *port* "shr $~s, %eax" (- charshift fxshift)))

(define-primitive (fixnum? si arg)
  (emit-expr *port* si arg)
  (emit *port* "and $~s, %al" fxmask)
  (emit *port* "cmp $0, %al" )
  (emit *port* "sete %al")
  (emit *port* "movzbl %al, %eax")
  (emit *port* "sal $~s, %al" 6)
  (emit *port* "or $~s, %al" bool-f))

(define-primitive (null? si arg)
  (emit-expr *port* si arg)
  (emit *port* "cmp $~s, %al" null-b)
  (emit *port* "sete %al")
  (emit *port* "movzbl %al, %eax")
  (emit *port* "sal $~s, %al" 6)
  (emit *port* "or $~s, %al" bool-f))

(define-primitive (char? si arg)
  (emit-expr *port* si arg)
  (emit *port* "and $~s, %al" #b11111111)
  (emit *port* "cmp $~s, %al" chartag )
  (emit *port* "sete %al")
  (emit *port* "movzbl %al, %eax")
  (emit *port* "sal $~s, %al" 6)
  (emit *port* "or $~s, %al" bool-f))

(define-primitive (boolean? si arg)
  (emit-expr *port* si arg)
  (emit *port* "or $~s, %al" #b01000000)
  (emit *port* "cmp $~s, %al" bool-t)

  (emit *port* "sete %al")
  (emit *port* "movzbl %al, %eax")
  (emit *port* "sal $~s, %al" 6)
  (emit *port* "or $~s, %al" bool-f))

(define-primitive (not si arg)
  (emit-expr *port* si arg)
  (emit *port* "cmp $~s, %al" bool-f )
  (emit *port* "sete %al")
  (emit *port* "movzbl %al, %eax")
  (emit *port* "sal $~s, %al" 6)
  (emit *port* "or $~s, %al" bool-f))

;;; we don't check fixnum type but we take for granted that fxtag is 00
(define-primitive (fxzero? si arg)
  (emit-expr *port* si arg)
  (emit *port* "test %eax, %eax")
  (emit *port* "setz %al")
  (emit *port* "movzbl %al, %eax")
  (emit *port* "sal $~s, %al" 6)
  (emit *port* "or $~s, %al" bool-f))

(define-primitive (fxlognot si arg)
  (emit-expr *port* si arg)
  (emit *port* "xor $~s, %eax" #xFFFFFFFF)
  (emit *port* "and $~s, %al" #b11111100))

(define-primitive (fx+ si arg1 arg2)
  (emit-expr *port* si arg1)
  (emit *port* "movl %eax, ~s(%esp)" si)
  (emit-expr *port* (- si wordsize) arg2)
  (emit *port* "addl ~s(%esp), %eax" si))

;;; HACK it evaluates the arguments in different order than fx+
(define-primitive (fx- si arg1 arg2)
  (emit-expr *port* si arg2)
  (emit *port* "movl %eax, ~s(%esp)" si)
  (emit-expr *port* (- si wordsize) arg1)
  (emit *port* "subl ~s(%esp), %eax" si))

(define-primitive (fx* si arg1 arg2)
  (emit-expr *port* si arg1)
  (emit *port* "sar $~s, %eax" 2)
  (emit *port* "movl %eax, ~s(%esp)" si)
  (emit-expr *port* (- si wordsize) arg2)
  (emit *port* "movl ~s(%esp), %ebx" si)
  (emit *port* "mul %ebx"))



;;; 1.4
(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((L (simple-format #f "L~s" count)))
	(set! count (+ 1 count))
	L))))

(define (if? expr)
  (eq? (car expr) 'if))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if si expr)
  (let ((alt-label (unique-label))
	(end-label (unique-label)))
    (emit-expr *port* si (if-test expr))
    (emit *port* "cmp $~s, %al" bool-f)
    (emit *port* "je ~a" alt-label)
    (emit-expr *port* si (if-conseq expr))
    (emit *port* "jmp ~a" end-label)
    (emit *port* "~a:" alt-label)
    (emit-expr *port* si (if-altern expr))
    (emit *port* "~a:" end-label)))

(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and test) test]
    [(and test test* ...)
     (if test (and test* ...) #f)]))

(define-syntax or
  (syntax-rules ()
    [(or) #t]
    [(or test) test]
    [(or test test* ...)
     (if test #t (or test* ...))]))

(test-all)
