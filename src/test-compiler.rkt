#lang racket

(require "compiler.rkt")

(compile-and-optimize "../examples/test.cll" "test" 
                      256 "null" #:w 1 #:h 3 #:opt #t)

;; Simple
;(compile-and-optimize "../tests/run/array.cll" "array" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/offset.cll" "offset" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/function-out.cll" "function" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/function-pair1.cll" "function_pair" 
;                      512 "null" #:opt #t)
;(compile-and-optimize "../tests/run/while-noio.cll" "whilenoio" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/add-noio.cll" "addnoio" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/matrixmult4-dup.cll" "matrix" 
;                      220 "null" #:w 5 #:h 4 #:opt #t)
;(compile-and-optimize "../examples/leftrotate.cll" "leftrotate" 
;                     256 "null" #:w 10 #:h 5 #:opt #t)
;(compile-and-optimize "../tests/run/bithack3.cll" "bithack3"
;		      300 "null" #:w 2 #:h 3 #:opt #t)

;; Single-core
;(compile-and-optimize "../tests/run/fir2.cll" "firfix" 
;                      512 "null" #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../tests/run/fir-par.cll" "fir-par" 
;                      512 "null" #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../tests/run/fir-par2.cll" "fir-par2" 
;                      512 "null" #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../tests/run/interp2.cll" "interp2" 
;                      256 "null" #:w 3 #:h 3 #:opt #t)
;(compile-and-optimize "../tests/run/poly.cll" "poly" 
;                      256 "null" #:w 3 #:h 3 #:opt #t)
;(compile-and-optimize "../tests/run/cos2.cll" "cos2" 
;                      300 "null" #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../tests/run/sin.cll" "sin" 
;                      290 "null" #:w 3 #:h 3 #:opt #f)

;; Multicore
;(compile-and-optimize "../tests/run/swap.cll" "swap" 
;                      256 "null" #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../tests/run/ssd.cll" "ssdfix" 
;                      310 "null" #:w 8 #:h 8 #:opt #t #:sliding #f)
;(compile-and-optimize "../tests/run/ssd-heu.cll" "ssd2" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f #:layout #t) ;; factor = 0.8
;(compile-and-optimize "../tests/run/sqrt2.cll" "sqrtfix"
;		      290 "null" #:w 4 #:h 4 #:opt #t #:sliding #f) ;; factor = 0.4
;(compile-and-optimize "../tests/run/complex3.cll" "complexfix" 
;                      290 "null" #:w 4 #:h 4 #:opt #t #:sliding #f)
;(compile-and-optimize "../tests/run/complex0.cll" "complex5" 
;                      290 "null" #:w 5 #:h 5 #:opt #f #:partition #f #:layout #t) ;; factor = 0.4
;(compile-and-optimize "../tests/run/prefixsum.cll" "prefixsum" 
;                      290 "null" #:w 8 #:h 8 #:opt #t)
;(compile-and-optimize "../tests/run/convolution2.cll" "convolutionfix" 
;                      290 "null" #:w 8 #:h 8 #:opt #t #:sliding #f)
;(compile-and-optimize "../tests/run/convolution3.cll" "convolution" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f)
;(compile-and-optimize "../tests/run/convolution2-heu.cll" "convolution2nl" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f #:layout #f) ;; factor = 0.4
;(compile-and-optimize "../tests/run/convolution-pair.cll" "convolutionpair" 
;                      290 "null" #:w 8 #:h 8 #:opt #f #:partition #f)


;; MD5
;(compile-and-optimize "../tests/run/md5-1.cll" "md1"
;		      1024 "null" #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../tests/run/md5-nl.cll" "mdnl"
;		      1024 "null" #:w 8 #:h 8 #:opt #f #:partition #t #:layout #f)



;; Map & Reduce
;(compile-and-optimize "../tests/run/map.cll" "map" 
;                      290 "null" #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../tests/run/map-pair.cll" "mappair" 
;                      290 "null" #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../tests/run/reduce.cll" "reduce" 
;                      290 "null" #:w 4 #:h 4 #:opt #f #:partition #t)
;(compile-and-optimize "../tests/run/reduce-pair.cll" "reducepair" 
;                      290 "null" #:w 4 #:h 4 #:opt #f #:partition #f)
;(compile-and-optimize "../tests/run/mapreduce.cll" "mapreduce-compile" 
;                      290 "null" #:w 5 #:h 4 #:opt #f #:partition #f)
