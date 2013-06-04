#lang s-exp rosette

(require "compiler.rkt" "communication.rkt")

(define result (list))

(define (test name input capacity)
  (set! result (cons 
                (cons name (test-simulate name input capacity))
                result)))

(test "if"       "4_1" 512)
(test "add"      "200" 512)
(test "function" "4_1" 512)
(test "function" "4_2" 512)
(test "matrixmult" "72" 300)

(reverse result)

;(test-simulate "cluster" "200")