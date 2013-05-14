#lang racket

(require "partitioner.rkt" "layout-sa.rkt")

(define (compile file name)
  
  (define partition (optimize-comm file
                                   #:name name
                                   #:cores 4 #:capacity 256 #:verbose #t))
  
  (layout (result-ast partition) 
          (result-env partition)
          4 2 2 name)
  )

(compile "tests/array-known.cll" "array-known")
