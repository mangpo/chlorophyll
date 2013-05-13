#lang racket

(require "partitioner.rkt" "layout-sa.rkt")

(define partition (optimize-comm 
                       "tests/if_concrete.cll" 
                       #:cores 16 #:capacity 256 #:verbose #t))

(layout 
 (result-ast partition) (result-env partition)
 16 4 4)
