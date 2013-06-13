#lang racket

(require "parser.rkt"
         "partitioner.rkt" "layout-sa.rkt" "communication.rkt"
         "visitor-desugar.rkt"
         "visitor-printer.rkt"
         "visitor-linker.rkt" 
         "visitor-tempinsert.rkt" 
         "visitor-desugar.rkt")

(provide compile test-simulate parse)

(define (parse file)
  (define my-ast (ast-from-file file))
  (define need-temp (send my-ast accept (new linker%)))
  (when need-temp
    (send my-ast accept (new temp-inserter%))
    (send my-ast accept (new desugar%)))
  my-ast)

(define (compile file name capacity [input #f] [w 5] [h 4] )
  (define n (* w h))
  (define my-ast (parse file))
  
  ;; generate sequantial simulation code
  (when input
    (simulate-onecore my-ast name input))
  
  (define concise-printer (new printer% [out #t]))
  
  (send my-ast pretty-print)
  (pretty-display "---------------------------------------")
  
  ;; partition
  (define partition (optimize-comm my-ast
                                   #:name name
                                   #:cores 20 
                                   #:capacity capacity 
                                   #:verbose #f))
  
  (send my-ast pretty-print)
  
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

(compile "../tests/pair.cll" "pair" 256)
