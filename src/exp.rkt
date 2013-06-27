#lang s-exp rosette

(require "partitioner.rkt" "partition-storage.rkt")

(define t 0)

(set! t (current-seconds))
(optimize-comm "examples/matrixmult_1.cll" #:cores 16 #:capacity 256)
(pretty-display (format "matrixmult_1 = ~a sec" (- (current-seconds) t)))

(set! t (current-seconds))
(optimize-comm "examples/matrixmult_2.cll" #:cores 16 #:capacity 256)
(pretty-display (format "matrixmult_2 = ~a sec" (- (current-seconds) t)))

(set! t (current-seconds))
(optimize-comm "examples/md5_2.cll" #:cores 16 #:capacity 256)
(pretty-display (format "md5_2 = ~a sec" (- (current-seconds) t)))
