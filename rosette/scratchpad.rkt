#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "parser.rkt" 
         "visitor-interpreter.rkt" 
         "visitor-collector.rkt" 
         "visitor-rename.rkt"
         "visitor-printer.rkt")

(configure [bitwidth 10])

(define (optimize-comm file 
                        #:cores [best-num-cores 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [best-num-msg 256])
  
  ;; Define printer
  (define concise-printer (new printer%))
  
  ;; Easy inference happens here
  (define my-ast (ast-from-file file))
  (pretty-display "=== Original AST ===")
  (send my-ast pretty-print)
  
  ;; Collect real pysical places
  #|(define collector (new place-collector% 
                         [collect? (lambda(x) (and (number? x) (not (symbolic? x))))]))
  (define place-set (send my-ast accept collector))
  (pretty-display "\n=== Places ===")
  (pretty-print place-set)
  
  ;; Convert distinct abstract partitions into distinct numbers
  ;; and different symbolic vars for different holes
  (define converter (new partition-to-number% [num-core 16] [real-place-set place-set]))
  (send my-ast accept converter)
  (pretty-display "\n=== After string -> number ===")
  (send my-ast pretty-print)
  
  ;; Count number of messages
  (define interpreter (new count-msg-interpreter% [core-space capacity] [num-core best-num-cores]))
  (define best-sol #f)
  
  (define num-msg (send my-ast accept interpreter))
  (define num-cores (send interpreter num-cores))
  
  (define (loop)
    ;(solve (assert (< num-cores best-num-cores)))
    ;(set! best-num-cores (evaluate num-cores))
    (solve (assert (< num-msg best-num-msg)))
    (set! best-num-msg (evaluate num-msg))
    
    (set! best-sol (current-solution))
    
    ;; display
    ;(send my-ast accept concise-printer)
    ;(send interpreter display-used-space)
    (pretty-display (format "# messages = ~a" (evaluate num-msg)))
    (pretty-display (format "# cores = ~a" (evaluate num-cores)))
    (loop)
  )
  
  ;void
  (with-handlers* ([exn:fail? (lambda (e) 
                                (pretty-display "\n=== Solution ===")
                                (send my-ast accept concise-printer) 
                                (pretty-display best-sol)
				(evaluate num-msg))])
                  (loop))|#
  void
  )

(optimize-comm "examples/for-test.cll" #:cores 16 #:capacity 256 #:max-msgs 15)
