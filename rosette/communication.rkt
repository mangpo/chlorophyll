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

  (define (print-main)
    (pretty-display "int main() {")
    (display "  pthread_t")

    ;; declare pthread
    (define first #t)
    (for ([i (in-range (* w h))])
         (unless (empty? (get-field stmts (vector-ref programs i)))
           (if first 
               (set! first #f)
               (display ","))
           (display (format " t_~a" i))))
    (pretty-display ";")
  
    ;; pthread_crate
    (for ([i (in-range (* w h))])
         (unless (empty? (get-field stmts (vector-ref programs i)))
           (pretty-display (format "  pthread_create(&t_~a, NULL, main_~a, NULL);" i i))))

    ;; pthread_join
    (for ([i (in-range (* w h))])
         (unless (empty? (get-field stmts (vector-ref programs i)))
           (pretty-display (format "  pthread_join(t_~a, NULL);" i))))

    ;; return
    (pretty-display "  return 0;")
    (pretty-display "}"))


  (with-output-to-file #:exists 'truncate (format "output/~a.cpp" name)
    (lambda ()
      (pretty-display "#include \"communication.cpp\"\n")
      (for ([i (in-range (* w h))])
        (pretty-display (format "//----------------------- CORE ~a(~a,~a) ------------------------"
                                i (floor (/ i w)) (modulo i w)))
        (send cprinter set-core i)
        (send (vector-ref programs i) accept cprinter)
        (newline))
      (print-main))))
       

