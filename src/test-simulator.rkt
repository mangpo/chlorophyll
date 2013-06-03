#lang s-exp rosette

(require "compiler.rkt" "communication.rkt")

(define testdir "../tests/run")

(define (test name input)
  (compile (format "~a/~a.cll" testdir name) name)
  (define diff (simulate name input))
  
  (display (format "~a_~a\t" name input))
  (pretty-display (if (equal? diff "") "PASSED" "FAILED"))
  )

#|
(test "if" "4_1")
(test "add" "200")
(test "function" "4_1")
(test "function" "4_2")
|#

(test "cluster" "200")