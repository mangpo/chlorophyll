#lang racket

(require "parser.rkt"
         "ast-util.rkt"
         "partitioner.rkt" 
         "layout-sa.rkt" 
         "separator.rkt"
         "visitor-desugar.rkt"
         "visitor-printer.rkt"
         "visitor-workdesugar.rkt"
         "visitor-linker.rkt" 
         "visitor-runstatic.rkt"
         "visitor-tempinsert.rkt" 
         "visitor-desugar.rkt"
         "visitor-memory.rkt"
         "visitor-codegen.rkt")

(provide compile test-simulate parse)

;; Parse HLP from file to AST
(define (parse file)
  ;(define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (send my-ast pretty-print)
  ;(send my-ast accept (new implicit-main-inserter%))
  ;(send my-ast accept (new flatten%))
  (send my-ast accept (new work-desugarer%))
  (send my-ast accept (new desugar%))
  (send my-ast accept (new linker% [static #t]))
  (send my-ast accept (new static-runner%))
  (send my-ast accept (new linker% [static #f]))
  (send my-ast accept (new temp-inserter%))
  my-ast)

;; Compile IR to machine code.
(define (generate-code program i w h)
  (pretty-display `(-------------------- ,i -----------------------))
  (let* ([data-iter (send program accept (new memory-mapper%))]
         [code-gen (new code-generator% [data-size (car data-iter)]
                        [iter-size (cdr data-iter)]
                        [core i] [w w] [h h])])
    (pretty-display ">>> code gen")
    (define res (send program accept code-gen))
    (pretty-display ">>> result")
    (codegen-print res)))

;; Compile per-core IRs to per-core machine codes.
(define (generate-codes programs w h)
  (for ([i (in-range (add1 (add1 (* w h))))])
    (let ([program (vector-ref programs i)])
      (generate-code program i w h))))

;; Compile per-core HLP read from file to machine code.
(define (compile-percore file core w h)
  (define my-ast (parse file))
  ;(send my-ast pretty-print)
  (generate-code my-ast core w h))

;(compile-percore "../tests/while.cll" 0 2 4)

;; Compile HLP read from file to per-core machine codes.
(define (compile file name capacity input [w 5] [h 4] 
                       #:verbose [verbose #t])
  
  (define n (* w h))
  (define my-ast (parse file))
  (define concise-printer (new printer% [out #t]))
  
  ;; generate sequantial simulation code
  (when input (simulate-onecore my-ast name input))
  
  (when verbose
    (pretty-display "--- before partition ---")
    (send my-ast pretty-print))
  
  ;; partition
  (define partition (optimize-comm my-ast
                                   #:name name
                                   #:cores 20 
                                   #:capacity capacity 
                                   #:verbose #f))
  (when verbose
    (pretty-display "--- after partition ---")
    (send my-ast pretty-print))
  
  ;; layout
  (define layout-res (layout my-ast
                             n w h name))
  
  (when verbose
    (pretty-display "--- after layout ---"))

  ;; generate multicore ASTs and output equivalent cpp simuation code to file
  (define programs (sep-and-insertcomm name my-ast w h 
                                       (layoutinfo-routes layout-res)
                                       (layoutinfo-part2core layout-res)
                                       #:verbose #t))
  ;; generate machine code for each core
  (generate-codes programs w h)
  )

(define testdir "../tests/run")

(define (test-simulate name input capacity)
  (compile (format "~a/~a.cll" testdir name) name capacity input)
  (pretty-display (format "running ~a ..." name))
  (define diff (simulate-multicore name input))
  
  (cond
    [(= diff 0) "PASSED"]
    [(= diff 1) "FAILED"]
    [(= diff 2) "NOT-FOUND"]))
