#lang racket

(require "visitor-comminsert.rkt" "visitor-unroll.rkt" "visitor-divider.rkt" "visitor-cprinter.rkt")

(provide (all-defined-out))

;; Unroll for loop according to array distributions of variables inside its body.
;; Note: given AST is mutated.
(define (unroll ast)
  (define for-unroller (new loop-unroller%))
  (send ast accept for-unroller)
  )

;; 1) Insert communication route to send-path field.
;; 2) Convert partition ID to actual core ID.
;; Note: given AST is mutate.
(define (insert-comm ast routing-table part2core)
  (define commcode-inserter (new commcode-inserter% 
                                 [routing-table routing-table]
                                 [part2core part2core]))

  (send ast accept commcode-inserter))

(define (regenerate ast w h name)
  (define divider (new ast-divider% [w w] [h h]))
  (define programs (send ast accept divider))

  (define cprinter (new cprinter% [w w] [h h]))
  
  (with-output-to-file #:exists 'truncate (format "output/~a.c" name)
    (lambda ()
      (pretty-display "#include \"communication.cpp\"\n")
      (for ([i (in-range (* w h))])
        (pretty-display (format "//----------------------- CORE ~a(~a,~a) ------------------------"
                                i (floor (/ i w)) (modulo i w)))
        (send cprinter set-core i)
        (send (vector-ref programs i) accept cprinter)
        (newline)))))
       

