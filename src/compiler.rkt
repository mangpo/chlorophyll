#lang racket

(require "partitioner.rkt" "layout-sa.rkt" "communication.rkt"
         "visitor-printer.rkt")

(provide compile)

(define (compile file name)
  (define concise-printer (new printer% [out #t]))
  
  (define partition (optimize-comm file
                                   #:name name
                                   #:cores 20 #:capacity 512 #:verbose #f))
  
  (define layout-res (layout (result-ast partition) 
                             20 5 4 name))
  
  (pretty-display `(routing-table ,(layoutinfo-routes layout-res)))
  (pretty-display `(part2core ,(layoutinfo-part2core layout-res)))

  (pretty-display "--- before unroll ---")
  (send (result-ast partition) pretty-print)
  
  (unroll (result-ast partition))
  
  (pretty-display "--- after unroll ---")
  (send (result-ast partition) pretty-print)
  
  (pretty-display "--- before insert communication ---")
  (insert-comm (result-ast partition) 
               (layoutinfo-routes layout-res)
               (layoutinfo-part2core layout-res)
               5 4)
  
  
  (pretty-display "--- after insert communication ---")
  (send (result-ast partition) pretty-print)

  (regenerate (result-ast partition) 5 4 name)
  ;(define diff (simulate name "if.in"))
  ;(if (equal? diff "")
  ;    "PASSED"
  ;    "FAILED")
  )

;(compile "examples/md5/md5_4-known.cll" "md5_4-known")
;(compile "../tests/run/if.cll" "if")
