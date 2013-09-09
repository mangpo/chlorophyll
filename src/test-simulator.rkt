#lang racket

(require "compiler.rkt")

(define result (list))

(define (test name input capacity #:w [w 5] #:h [h 4])
  (set! result (cons 
                (cons name (test-simulate name input capacity w h))
                result)))

;(test "array" "10" 256)
;(test "simple" "null" 256)

;(test "assign" "null" 512)
(test "if"       "4_1" 512)
;(test "offset"   "4_1" 256)
;(test "add"       "200" 1024)
;(test "add2"       "200" 1024)
;(test "function" "4_1" 256)
;(test "function" "4_2" 256)
;(test "function-pair" "null" 256 #:w 2 #:h 2)

;(test "pair1" "null" 512)
;(test "pair2" "null" 512)
;(test "while" "10" 256)

;(test "matrixmult" "72" 400) ;capacity issue
;(test "matrixmult-noio" "null" 500)
;(test "cluster" "null" 256)

;(test "md5" "null" 600 #:w 10 #:h 5)

(for ([res (reverse result)])
  (pretty-display res))