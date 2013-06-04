#lang racket

(require "partitioner.rkt" "layout-sa.rkt" "communication.rkt"
         "visitor-printer.rkt")

(provide compile test-simulate)

(define (compile file name)
  (define concise-printer (new printer% [out #t]))
  
  (define partition (optimize-comm file
                                   #:name name
                                   #:cores 20 #:capacity 256 #:verbose #t))
  (result-msgs partition)
  #|
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

  (regenerate (result-ast partition) 5 4 name)|#
  )

(define testdir "../tests/run")

(define (test-simulate name input)
  (compile (format "~a/~a.cll" testdir name) name)
  (define diff (simulate name input))
  
  (display (format "~a_~a\t" name input))
  (pretty-display (if (equal? diff "") "PASSED" "FAILED"))
  )

(define t (current-seconds))
(compile "../tests/run/matrixmult_0.cll" "matrixmult_1")
(pretty-display (format "matrixmult_0 = ~a" (- (current-seconds) t)))

(set! t (current-seconds))
(compile "../tests/run/matrixmult_1.cll" "matrixmult_1")
(pretty-display (format "/matrixmult_1 = ~a" (- (current-seconds) t)))
;(test-simulate "matrixmult_1" "72")
