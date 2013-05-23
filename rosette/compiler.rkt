#lang racket

(require "partitioner.rkt" "layout-sa.rkt" "communication.rkt")

(define (compile file name)
  
  (define partition (optimize-comm file
                                   #:name name
                                   #:cores 20 #:capacity 512 #:verbose #f))
  
  (define layout-res (layout (result-ast partition) 
                             20 5 4 name))
  
  (pretty-display `(routing-table ,(layoutinfo-routes layout-res)))
  (pretty-display `(part2core ,(layoutinfo-part2core layout-res)))
  
  (insert-comm (result-ast partition) 
               (layoutinfo-routes layout-res)
               (layoutinfo-part2core layout-res))
  
  (send (result-ast partition) pretty-print)
  )

;(compile "examples/md5/md5_4.cll" "md5_4")
(compile "examples/if.cll" "if")
