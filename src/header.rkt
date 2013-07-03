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

(define outdir "../output")
(define datadir "../testdata")

(struct meminfo (addr virtual data))
