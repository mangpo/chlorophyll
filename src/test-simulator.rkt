#lang racket

(require "compiler.rkt")

(define result (list))

(define (test name input capacity #:w [w 5] #:h [h 4])
  (set! result (cons 
                (cons name (test-simulate name input capacity w h))
                result)))
#|
(test "array" "10" 256)
(test "simple" "null" 256)

(test "assign" "null" 512)
(test "if"       "4_1" 512)
(test "offset"   "4_1" 512)
(test "add"       "200" 1024)
(test "add2"       "200" 1024)
(test "function" "4_1" 256)
(test "function" "4_2" 256)|#
;(test "function-out" "null" 256)
;(test "function-pair1" "null" 512 #:w 2 #:h 3)
;(test "pair1" "null" 512)
;(test "pair2" "null" 512)
;(test "while" "10" 256)
;(test "unroll" "null" 1024 #:w 2 #:h 3)

;(test "matrixmult6" "72" 400) ;capacity issue
;(test "matrixmult4-dup" "null" 500)
;(test "cluster" "null" 256)

;(test "md5-1" "null" 1024 #:w 8 #:h 8)
;(test "ssd_simple" "null" 512 #:w 8 #:h 5)
;(test "swap" "null" 512 #:w 8 #:h 8)
;(test "prefixsum" "null" 512 #:w 8 #:h 8)
;(test "convolution" "null" 10000 #:w 4 #:h 4)
;(test "convolution2" "null" 512 #:w 8 #:h 8)
;(test "fir" "1" 1000 #:w 3 #:h 3)
;(test "interp2" "1" 256 #:w 3 #:h 3)
;(test "poly" "null" 256 #:w 3 #:h 3)
;(test "cos2" "null" 300 #:w 3 #:h 3)
;(test "sqrt" "null" 300 #:w 3 #:h 3)
(test "gaussseidel" "null" 300 #:w 3 #:h 3)

(for ([res (reverse result)])
  (pretty-display res))