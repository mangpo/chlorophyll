#lang racket

(require "parser.rkt"
         "partitioner.rkt" "layout-sa.rkt" "communication.rkt"
         "visitor-printer.rkt")

(provide compile test-simulate)

(define (compile file name capacity [input #f] [w 5] [h 4] )
  (define n (* w h))
  (define my-ast (ast-from-file file))
  
  ;; generate sequantial simulation code
  (when input
    (simulate-onecore my-ast name input))
  
  (define concise-printer (new printer% [out #t]))
  
  ;; partition
  (define partition (optimize-comm my-ast
                                   #:name name
                                   #:cores 20 
                                   #:capacity capacity 
                                   #:verbose #f))
  
  ;; layout
  (define layout-res (layout my-ast
                             n w h name))
  
  (pretty-display `(routing-table ,(layoutinfo-routes layout-res)))
  (pretty-display `(part2core ,(layoutinfo-part2core layout-res)))

  ;; unroll
  ;pretty-display "--- before unroll ---")
  ;(send my-ast pretty-print)
  (unroll my-ast)
  ;(pretty-display "--- after unroll ---")
  ;(send my-ast pretty-print)
  
  ;; insert communication code
  ;(pretty-display "--- before insert communication ---")
  (insert-comm my-ast
               (layoutinfo-routes layout-res)
               (layoutinfo-part2core layout-res)
               w h)
  ;(pretty-display "--- after insert communication ---")
  ;(send my-ast pretty-print)

  ;; generate multicore ASTs and simuation code
  (regenerate my-ast w h name)
  )

(define testdir "../tests/run")

(define (test-simulate name input capacity)
  (compile (format "~a/~a.cll" testdir name) name capacity input)
  (define diff (simulate-multicore name input))
  
  (if (equal? diff "") "PASSED" "FAILED")
  )

;(compile "../tests/run/matrixmult.cll" "matrixmult" 300)
;(test-simulate "matrixmult" "72" 300)
