(load "tests/tests-driver.scm")
(load "tests/tests-1.1-req.scm")

;;; fixed integers
(define (compile-program x port)
  (unless (integer? x) (error 'interror "not an integer"))
  (emit port ".text")
  (emit port ".globl scheme_entry")
  (emit port ".type scheme_entry, @function")
  (emit port "scheme_entry:")
  (emit port "movl $~s, %eax" x)
  (emit port "ret"))

(test-all)
