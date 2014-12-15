#lang racket

(require "compiler.rkt")

(define (test file name input capacity w h part)
  (pretty-display (test-simulate file name input capacity w h part)))

(define width (make-parameter 2)) ;; minimum
(define height (make-parameter 3)) ;; minimum
(define memory (make-parameter 256)) ;; 64 words x 4 slots
(define syn-part (make-parameter #t))

(define-values (input-file input)
  (command-line
   #:once-each
   [("-c" "--columns")  c
                        "Number of columns of nodes [default=2]" 
                        (width (string->number c))]
   
   [("-r" "--rows")     r
                        "Number of rows of nodes [default=3]"
                        (height (string->number r))]
   
   [("-m" "--memory")   m
                        "Size of memory in each node in slots (# of words times 4) [default=256]"
                        (memory (string->number m))]
   
   [("--heu-partition") "Use heuristic partitioner instead of synthesizing partitioner [default=synthesizing]"
                        (syn-part #f)]

   #:args (filename in)
   (values filename in)))
                
(test input-file 
      (first (string-split (last (string-split input-file "/")) "."))
      input
      (memory) (width) (height) (syn-part))