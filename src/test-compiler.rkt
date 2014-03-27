#lang racket

(require "compiler.rkt")

;(compile-and-optimize "../examples/simple/test.cll" "test" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../examples/md5/assume.cll" "assume" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../examples/simple/function2.cll" "function2" 
;                      128 "null" #:opt #f #:soft-capacity 175 #:partition #t)
;(compile-and-optimize "../examples/rom/interp.cll" "interp" 
;                      300 "null" #:opt #t)

;; Simple
;(compile-and-optimize "../examples/simple/array.cll" "array" 
;                      256 "null" #:w 2 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/simple/offset.cll" "offset" 
;                      512 "null" #:w 2 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/simple/function.cll" "function" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../examples/simple/function-pair.cll" "function-pair" 
;                      512 "null" #:opt #f)
;(compile-and-optimize "../examples/simple/while.cll" "while" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../examples/simple/extendshift.cll" "extendshift" 
;                      256 "null" #:opt #f)

;(compile-and-optimize "../examples/matrixmultiply/matrixmult4-dup.cll" "matrixmult4-dup" 
;                      220 "null" #:w 5 #:h 4 #:opt #t)
;(compile-and-optimize "../examples/bithack/bithack3.cll" "bithack3"
;		      300 "null" #:w 2 #:h 3 #:opt #t)

;; Single-core
(compile-and-optimize "../examples/rom/fir.cll" "fir" 
                      512 "null" #:w 3 #:h 3 #:opt #t)
;(compile-and-optimize "../examples/rom/fir0.cll" "fir0" 
;                      256 "null" #:w 2 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/rom/fir-par2.cll" "fir-par2" 
;                      512 "null" #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../examples/rom/fir-par4.cll" "fir-par4" 
;                      512 "null" #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../examples/rom/interp.cll" "interp" 
;                      256 "null" #:w 3 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/rom/cos.cll" "cos" 
;                      300 "null" #:opt #f)
;(compile-and-optimize "../examples/rom/sin.cll" "sin" 
;                      290 "null" #:w 3 #:h 3 #:opt #f)

;; Multicore
;(compile-and-optimize "../examples/parallel/swap0.cll" "swap0" 
;                      256 "null" #:w 8 #:h 8 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/parallel/ssd.cll" "ssd" 
;                      310 "null" #:w 8 #:h 8 #:opt #t #:sliding #f)
;(compile-and-optimize "../examples/parallel/ssd-heu.cll" "ssd-heu" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f #:layout #t) ;; factor = 0.8
;(compile-and-optimize "../tests/run/prefixsum.cll" "prefixsum" 
;                      290 "null" #:w 8 #:h 8 #:opt #t)
;(compile-and-optimize "../examples/parallel/convolution.cll" "convolution" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f) ;; factor = 0.4
;(compile-and-optimize "../examples/parallel/convolution-pair.cll" "convolutionpair" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f)

;(compile-and-optimize "../examples/rom/sqrt0.cll" "sqrt0"
;		      290 "null" #:w 4 #:h 4 #:opt #t #:sliding #f) ;; factor = 0.4
;(compile-and-optimize "../examples/rom/complex3.cll" "complex" 
;                      290 "null" #:w 4 #:h 4 #:opt #t #:sliding #f)
;(compile-and-optimize "../examples/rom/complex0.cll" "complex0" 
;                      290 "null" #:w 5 #:h 5 #:opt #f #:partition #f #:layout #t) ;; factor = 0.4

;; MD5
;(compile-and-optimize "../examples/md5/leftrotate.cll" "leftrotate"
;		      1024 "null" #:w 4 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/md5/md1-rightrot.cll" "md1-rightrot"
;		      1024 "null" #:w 8 #:h 8 #:opt #t)
;(compile-and-optimize "../examples/md5/md1-circular2.cll" "md1-entire"
;		      1024 "null" #:w 8 #:h 8 #:opt #t)
;(compile-and-optimize "../examples/sha/sha.cll" "sha"
;		      512 "null" #:w 8 #:h 8 #:opt #f #:partition #f)

;; Map & Reduce
;(compile-and-optimize "../examples/mapreduce/map.cll" "map" 
;                      290 "null" #:w 3 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/map-pair.cll" "map-pair" 
;                      290 "null" #:w 3 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/map.cll" "reduce" 
;                      290 "null" #:w 4 #:h 4 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/reduce-pair.cll" "reduce-pair" 
;                      290 "null" #:w 4 #:h 4 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/mapreduce.cll" "mapreduce" 
;                      290 "null" #:w 5 #:h 4 #:opt #f #:partition #f)
