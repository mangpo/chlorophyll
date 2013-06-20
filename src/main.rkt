#lang s-exp rosette

; INCOMPLETE.  Significant amount of work to be done.
; See TODO above the compile procedure.

(require "partitioner.rkt" "layout.rkt")

(provide compile)

; Currently output-file is ignored.
; TODO: Bridge between the partitioner and the layout.
; Need to actually partition the code into different functions.
; Then find the dependencies between functions.
; Finally estimate the size of each function
(define (compile input-file output-file)
  (optimize-comm input-file #:cores 16 #:capacity 256 #:max-msgs 15)
  'done)

(compile "examples/simple-hole.cll" "examples/simple-hole.out")
