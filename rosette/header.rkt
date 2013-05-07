#lang s-exp rosette

(require (only-in rosette [sym? symbolic?]))
(require (only-in racket foldl log))

(provide symbolic? foldl log
         global-sol set-global-sol
         evaluate-with-sol)

(define global-sol (sat (hash)))
(define (set-global-sol sol)
  (set! global-sol sol))

(define-syntax-rule (evaluate-with-sol x)
  (evaluate x global-sol))
