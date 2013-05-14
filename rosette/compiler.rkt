#lang racket

(require "partitioner.rkt" "layout-sa.rkt")

(define partition (optimize-comm 
                       "tests/function.cll" 
                       #:cores 2 #:capacity 256 #:verbose #t))

(layout 
 (result-ast partition) (result-env partition)
 4 2 2 "function")
