#lang racket

(require "compiler.rkt")

;(compile-and-optimize "../examples/test.cll" "test" 
;                      1024 "null" #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../tests/run/array.cll" "array" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/offset.cll" "offset" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/function-out.cll" "function" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/function-pair1.cll" "function_pair" 
;                      512 "null" #:opt #f)
;(compile-and-optimize "../tests/run/while-noio.cll" "whilenoio" 
;                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/add-noio.cll" "addnoio" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/matrixmult4-dup.cll" "matrix" 
;                      220 "null" #:w 5 #:h 4 #:opt #t)
;(compile-and-optimize "../examples/leftrotate.cll" "leftrotate" 
;                     256 "null" #:w 10 #:h 5 #:opt #t)

;(compile-and-optimize "../tests/run/md5-3.cll" "md3" 
;                     1024 "null" #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../tests/run/md5-4.cll" "md4"
;		      1024 "null" #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../tests/run/md5-2.cll" "md2" 
;                     400 "null" #:w 10 #:h 5 #:opt #t)

;(compile-and-optimize "../tests/run/ssd_simple22.cll" "ssd_simple2" 
;                     256 "null" #:w 8 #:h 8 #:opt #t)
;(compile-and-optimize "../tests/run/swap.cll" "swap" 
;                      256 "null" #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../tests/run/fir3.cll" "fir3" 
;                      512 "null" #:w 3 #:h 3 #:opt #t)
;(compile-and-optimize "../tests/run/fir2.cll" "fir2" 
;                      512 "null" #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../tests/run/fir.cll" "fir" 
;                      512 "null" #:w 3 #:h 3 #:opt #t)
(compile-and-optimize "../tests/run/fir-par.cll" "fir-par" 
                      512 "null" #:w 4 #:h 4 #:opt #f)
(compile-and-optimize "../tests/run/fir-par2.cll" "fir-par2" 
                      512 "null" #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../tests/run/prefixsum.cll" "prefixsum" 
;                      512 "null" #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../tests/run/convolution.cll" "convolution" 
;                      5000 "null" #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../tests/run/convolution2.cll" "convolution2" 
;                      512 "null" #:w 8 #:h 8 #:opt #f)
