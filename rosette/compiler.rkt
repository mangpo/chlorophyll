#lang racket

(require "partitioner.rkt" "layout-sa.rkt")

(define (compile file name)
  
  (define partition (optimize-comm file
                                   #:name name
                                   #:cores 8 #:capacity 256 #:verbose #t))
  
  (layout (result-ast partition) 
          (result-env partition)
          8 2 4 name)
  )

(compile "tests/while.cll" "add")
