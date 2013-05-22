#lang racket

(require "partitioner.rkt" "layout-sa.rkt" "communication.rkt")

(define (compile file name)
  
  (define partition (optimize-comm file
                                   #:name name
                                   #:cores 8 #:capacity 256 #:verbose #t))
  
  (define layout-res (layout (result-ast partition) 
                             8 2 4 name))
  
  (pretty-display `(routing-table ,(layoutinfo-routes layout-res)))
  (pretty-display `(part2core ,(layoutinfo-part2core layout-res)))
  
  (insert-comm (result-ast partition) 
               (layoutinfo-routes layout-res)
               (layoutinfo-part2core layout-res))
  
  (send (result-ast partition) pretty-print)
  )

(compile "tests/space_symbolic.cll" "space_symbolic")
