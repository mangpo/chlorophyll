#lang racket

(require "compiler.rkt" "communication.rkt")

(define result (list))

(define (test name input capacity)
  (set! result (cons 
                (cons name (test-simulate name input capacity))
                result)))

(test "simple" "null" 256)

(test "if"       "4_1" 512)
(test "add"      "200" 512)
;(test "function" "4_1" 512)
;(test "function" "4_2" 512)
;(test "matrixmult" "72" 400) ;capacity issue
;(test "pair1" "null" 256)
;(test "pair2" "null" 256)
;(test "pair3" "null" 256)
;(test "cluster" "null" 256)

;(test "md5-full" "md5" 400)

(for ([res (reverse result)])
  (pretty-display res))