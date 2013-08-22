#lang racket

(require "compiler.rkt")

;(compile-to-IR "../examples/array.cll" "array" 256 "null" 4 5 #:verbose #t)
;(compile-to-IR "../tests/run/md5-noio.cll" "md5noio" 
;               530 "null" 7 6 #:verbose #f)
;(compile-to-IR "../tests/run/function.cll" "function"
;               256 "4_1" 4 5 #:verbose #t)
;(compile-to-IR "../tests/run/array.cll" "array"
;               256 "10" 4 5 #:verbose #t)

;(compile-and-optimize "../tests/run/test.cll" "test" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../examples/array.cll" "array" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/offset.cll" "offset" 
;                      256 "null" #:opt #f #:run #t)
;(compile-and-optimize "../tests/run/offset-noio.cll" "offsetnoio" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/function-noio.cll" "functionnoio" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/while-noio.cll" "whilenoio" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../examples/bug.cll" "bug" 
;                      256 "null" #:w 5 #:h 4 #:opt #f)
;(compile-and-optimize "../tests/run/matrixmult2-noio.cll" "matrix" 
;                      220 "null" #:w 5 #:h 4 #:opt #f)

(compile-and-optimize "../tests/run/md5.cll" "md5opt" 
                      600 "null" #:w 10 #:h 5 #:opt #t)

;(compile-and-optimize "../tests/run/md5-init.cll" "md5init" 
;                      600 "null" #:w 10 #:h 5 #:opt #t)

;(compile-percore "../examples/array.cll" 0 2 2)
;(compile-and-optimize-percore "../examples/array.cll" 0 2 2)