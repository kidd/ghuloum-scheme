(load "tests/tests-driver.scm")
;(load "tests/tests-1.1-req.scm")
(load "tests/tests-1.2-req.scm")

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
   ((char? x) (logior (ash x 8) #b1111))
   ((null? x) #b00111111)))

(define (compile-program x port)
  (unless (immediate? x) (error 'immediaterror "not an immediate"))
  (emit port ".text")
  (emit port ".globl scheme_entry")
  (emit port ".type scheme_entry, @function")
  (emit port "scheme_entry:")
  (emit port "movl $~s, %eax" (immediate-rep x))
  (emit port "ret"))

(test-all)
