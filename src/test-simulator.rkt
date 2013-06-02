#lang s-exp rosette

(require "compiler.rkt" "communication.rkt")

(define testdir "../tests/run")

(define (test name input)
  (compile (format "~a/~a.cll" testdir name) name)
  (define diff (simulate name input))
  
  (display (format "~a\t" name))
  (pretty-display (if (equal? diff "") "PASSED" "FAILED"))
  )

(test "if" "if.in")
(test "add" "add.in")