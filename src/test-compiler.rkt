#lang racket

(require "compiler.rkt")

;; Simple
;(compile-and-optimize "../examples/simple/hello.cll" "hello" 
;                      256 #:w 2 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/simple/part2core.cll" "part2core" 
;                      256 #:w 3 #:h 3 #:opt #f)
#;(compile-and-optimize "../examples/simple/array.cll" "array" 
                      256 #:w 2 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/simple/offset.cll" "offset" 
;                      512 #:w 2 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/simple/function.cll" "function" 
;                      256 #:opt #f)
;(compile-and-optimize "../examples/simple/function-pair.cll" "function-pair" 
;                      512 #:opt #f)
;(compile-and-optimize "../examples/simple/while.cll" "while" 
;                      256 #:opt #t)
;(compile-and-optimize "../examples/simple/extendshift.cll" "extendshift" 
;                      256 #:opt #f)
;(compile-and-optimize "../examples/simple/global.cll" "global" 
;                      256 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/simple/hello-io.cll" "hello-io" 
                      256 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/simple/square.cll" "square" 
                      256 #:opt #t #:partition #f)
#;(compile-and-optimize "../examples/simple/check_neg.cll" "check_neg" 
                      256 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/simple/precision.cll" "precision" 
                      256 #:opt #f #:partition #f)

;; Matrix multiply
;(compile-and-optimize "../examples/matrixmultiply/matrixmult4-dup.cll" "matrixmult4-dup" 
;                      220 #:w 5 #:h 4 #:opt #t)

;; Bithack
;(compile-and-optimize "../examples/bithack/bithack3.cll" "bithack3"
;		      300 #:w 2 #:h 3 #:opt #t)
;(compile-and-optimize "../examples/bithack/count.cll" "count"
;		      1000 #:w 2 #:h 3 #:opt #t)

;; Single-core
;(compile-and-optimize "../examples/rom/fir.cll" "fir" 
;                      512 #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/rom/fir0.cll" "fir0" 
;                      256 #:w 2 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/rom/fir-par2.cll" "fir-par2" 
;                      512 #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../examples/rom/fir-par4.cll" "fir-par4" 
;                      512 #:w 4 #:h 4 #:opt #f)
;(compile-and-optimize "../examples/rom/interp.cll" "interp" 
;                      256 #:w 3 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/rom/cos.cll" "cos" 
;                      300 #:opt #f)
;(compile-and-optimize "../examples/rom/sin.cll" "sin" 
;                      290 #:w 3 #:h 3 #:opt #f)

;; Multicore
;(compile-and-optimize "../examples/parallel/swap0.cll" "swap0" 
;                      256 #:w 8 #:h 8 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/parallel/ssd.cll" "ssd" 
;                      310 #:w 8 #:h 8 #:opt #t #:sliding #f)
;(compile-and-optimize "../examples/parallel/ssd-heu.cll" "ssd-heu" 
;                      290 #:w 8 #:h 8 #:opt #f #:partition #f #:layout #t) ;; factor = 0.8
;(compile-and-optimize "../examples/parallel/prefixsum.cll" "prefixsum" 
;                      290 #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../examples/parallel/convolution.cll" "convolution" 
;                      290 #:w 8 #:h 8 #:opt #f #:partition #f) ;; factor = 0.4
;(compile-and-optimize "../examples/parallel/convolution-pair.cll" "convolutionpair" 
;                      290 #:w 8 #:h 8 #:opt #f #:partition #f)

;(compile-and-optimize "../examples/rom/sqrt0.cll" "sqrt0"
;		      290 #:w 4 #:h 4 #:opt #t #:sliding #f) ;; factor = 0.4
;(compile-and-optimize "../examples/rom/complex3.cll" "complex" 
;                      290 #:w 4 #:h 4 #:opt #t #:sliding #f)
;(compile-and-optimize "../examples/rom/complex0.cll" "complex0" 
;                      290 #:w 5 #:h 5 #:opt #f #:partition #f #:layout #t) ;; factor = 0.4

;; MD5
;(compile-and-optimize "../examples/md5/leftrotate.cll" "leftrotate"
;		      1024 #:w 4 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/md5/md1-rightrot-noassume.cll" "md1-rightrot-noassume"
;		      1024 #:w 8 #:h 8 #:opt #f)
;(compile-and-optimize "../examples/md5/md1-circular2.cll" "md1-entire"
;		      1024 #:w 8 #:h 8 #:opt #t)
;(compile-and-optimize "../examples/sha/sha.cll" "sha"
;		      512 #:w 8 #:h 8 #:opt #f #:partition #f)

;; Map & Reduce
;(compile-and-optimize "../examples/mapreduce/map.cll" "map" 
;                      290 #:w 3 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/map-pair.cll" "map-pair" 
;                      290 #:w 3 #:h 3 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/map.cll" "reduce" 
;                      290 #:w 4 #:h 4 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/reduce-pair.cll" "reduce-pair" 
;                      290 #:w 4 #:h 4 #:opt #f #:partition #f)
;(compile-and-optimize "../examples/mapreduce/mapreduce.cll" "mapreduce" 
;                      290 #:w 4 #:h 4 #:opt #f #:partition #f)

;; Fixed point
;(compile-and-optimize "../examples/fixedpoint/cos_round.cll" "cos_round" 
;                      300 #:w 3 #:h 3 #:opt #f)
;(compile-and-optimize "../examples/fixedpoint/fft_inplace.cll" "fft_inplace" 
;                      300 #:w 8 #:h 8 #:opt #f #:partition #f)

;; Actor
#;(compile-and-optimize "../examples/actor/sactor_multicalls.cll" "sactor_multicalls" 
                      300 #:w 3 #:h 3 #:opt #f #:partition #f)
(compile-and-optimize "../examples/actor/sactor_v2.cll" "sactor_v2" 
                      300 #:w 3 #:h 3 #:opt #t #:partition #f)
#;(compile-and-optimize "../examples/sensors/i2c4.cll" "i2c4" 
                      512 #:w 18 #:h 8 #:opt #f #:partition #f)

;; Parallel Module
#;(compile-and-optimize "../examples/module/module_simple.cll" "module_simple" 
                      512 #:w 3 #:h 3 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/module/module_mapreduce.cll" "module_mapreduce" 
                      512 #:w 4 #:h 4 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/module/module_global.cll" "module_global" 
                      400 #:w 3 #:h 3 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/module/module_concrete.cll" "module_concrete" 
                      512 #:w 3 #:h 3 #:opt #f #:partition #f)
#;(compile-and-optimize "../examples/module/module_pinning.cll" "module_pinning" 
                      512 #:w 3 #:h 3 #:opt #f #:partition #f)

;; HMM Classifier
#;(compile-and-optimize "../examples/sensors/hmm_array_b.cll" "hmm_array_b" 
                      350 #:w 5 #:h 5 #:opt #f #:partition #f
                      #:original-format #f)
#;(compile-and-optimize "../examples/sensors/derive_group.cll" "derive_group" 
                      350 #:w 5 #:h 5 #:opt #f #:partition #f
                      #:original-format #f)
#;(compile-and-optimize "../examples/sensors/hmm_test2.cll" "hmm_test2"
                      350 #:w 18 #:h 8 #:opt #f #:partition #f
                      #:original-format #f)
#;(compile-and-optimize "../examples/sensors/hmm_bug.cll" "hmm_bug"
                      350 #:w 3 #:h 3 #:opt #f #:partition #f
                      #:original-format #f)
#;(compile-and-optimize "../examples/sensors/hmm_pinning.cll" "hmm_pinning"
                      350 #:w 18 #:h 8 #:opt #f #:partition #f
                      #:original-format #f)
#;(compile-and-optimize "../examples/sensors/hmm_pinning_real.cll" "hmm_pinning_real"
                      350 #:w 18 #:h 8 #:opt #f #:partition #f
                      #:original-format #t)
#;(compile-and-optimize "../examples/sensors/hmm_module_pinning.cll" "hmm_module_pinning"
                      350 #:w 18 #:h 8 #:opt #f #:partition #f
                      #:original-format #f)
