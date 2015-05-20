#lang racket

(require "compiler.rkt")

(define result (list))

(define (test file name input capacity #:w [w 5] #:h [h 4] #:partition [part #t])
  (set! result (cons 
                (cons name (test-simulate file name input capacity w h part))
                result)))

;(test "../examples/simple/hello.cll"     "hello" "null" 256)
;(test "../examples/simple/array.cll"     "array" "10" 256)
;(test "../examples/simple/assign.cll"    "assign" "null" 512)
;(test "../examples/simple/if.cll"        "if"   "4_1" 512)
;(test "../examples/simple/offset.cll"    "offset" "4_1" 512)
;(test "../examples/simple/add.cll"       "add" "200" 1024)
;(test "../examples/simple/function.cll"  "function" "null" 256)
;(test "../examples/simple/function-pair.cll" "function-pair" "null" 512 #:w 2 #:h 3)
;(test "../examples/simple/pair1.cll"     "pair1" "null" 512)
;(test "../examples/simple/pair2.cll"     "pair2" "null" 512)
;(test "../examples/simple/while.cll"     "while" "10" 256)
;(test "../examples/simple/unroll.cll"    "unroll" "null" 1024 #:w 2 #:h 3)
;(test "../examples/simple/extendshift.cll"     "extendshift" "null" 256)

;(test "../examples/matrixmultiply/matrixmult6.cll"     "matrixmult6" "72" 500) ;capacity issue
;(test "../examples/matrixmultiply/matrixmult4-dup.cll" "matrixmult4-dup" "null" 500)

;(test "../examples/mapreduce/map.cll"         "map" "null" 290)
;(test "../examples/mapreduce/map-pair.cll"    "map-pair" "null" 290)
;(test "../examples/mapreduce/reduce.cll"      "reduce" "null" 290)
;(test "../examples/mapreduce/reduce-pair.cll" "reduce-pair" "null" 290 #:partition #f)
;(test "../examples/mapreduce/mapreduce.cll"   "mapreduce" "null" 290 #:partition #f)

;(test "../examples/rom/interp.cll"      "interp" "1" 256 #:w 3 #:h 3)
;(test "../examples/rom/poly.cll"        "poly" "1" 256 #:w 3 #:h 3)
;(test "../examples/rom/cos.cll"         "cos" "1" 300 #:w 3 #:h 3)
;(test "../examples/rom/sqrt2.cll"       "sqrt2" "2" 400 #:w 3 #:h 3)
;(test "../examples/rom/complex3.cll"    "complex3" "1" 300 #:w 4 #:h 4)

(test "../examples/md5/md5-rightrot-sim.cll" "md5-rightrot-sim" "null" 1024 #:w 8 #:h 8)
;(test "../examples/md5/leftrotate.cll" "leftrotate" "null" 1024 #:w 3 #:h 4)

;(test "../examples/parallel/ssd.cll"  "ssd" "null" 512 #:w 8 #:h 8)
;(test "../examples/parallel/swap.cll" "swap" "null" 512 #:w 8 #:h 8)
;(test "../examples/parallel/prefixsum.cll" "prefixsum" "null" 512 #:w 8 #:h 8)
;(test "../examples/parallel/convolution.cll" "convolution" "null" 400 #:w 4 #:h 4 #:partition #f)
;(test "../examples/parallel/convolution-pair.cll" "convolution-pair" "null" 400 #:w 8 #:h 8 #:partition #f)

;(test "../examples/fixedpoint/inout.cll"   "fp_inout" "fp_1" 256 #:w 3 #:h 3)
;(test "../examples/fixedpoint/cos.cll"      "fp_cos" "null" 256 #:w 3 #:h 3)


(for ([res (reverse result)])
  (pretty-display res))
