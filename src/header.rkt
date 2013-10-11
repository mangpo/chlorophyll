#lang s-exp rosette

(require (only-in rosette [sym? symbolic?]))
(require (only-in racket foldl log))

(provide symbolic? foldl log
         (all-defined-out))

(define global-sol (sat (hash)))
(define (set-global-sol sol)
  (set! global-sol sol))

(define-syntax-rule (evaluate-with-sol x)
  ;(evaluate x))
  (evaluate x global-sol))

(define max-bit 18)
(define n-bit 16)
(define node-offset 10)
(define block-offset 800)
(define procs 4)
(define check-interval 600)
(define distributed #t)

(define outdir "/home/mangpo/work/greensyn/output")
(define outdir-rel "../output")
(define datadir "/home/mangpo/work/greensyn/testdata")

(struct meminfo (addr virtual data))

