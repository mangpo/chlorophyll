#lang racket

(require "compiler.rkt")

(define width (make-parameter 2)) ;; minimum
(define height (make-parameter 3)) ;; minimum
(define memory (make-parameter 256)) ;; 64 words x 4 slots
(define opt (make-parameter #f))
(define syn-part (make-parameter #t))

(define input-file
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
   
   [("-o" "--optimize") "Superoptimization on [default=off]"
                        (opt #t)]
   
   [("--heu-partition") "Use heuristic partitioner instead of synthesizing partitioner [default=synthesizing]"
                        (syn-part #f)]

   #:args (filename)
   filename))
                
(compile-and-optimize input-file 
                      (first (string-split (last (string-split input-file "/")) "."))
                      (memory) #:w (width) #:h (height) #:opt (opt) 
                      #:partition (syn-part))