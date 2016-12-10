#lang s-exp rosette

(require (only-in rosette [term? symbolic?]))
(require (only-in racket foldl log))
(require "path.rkt")

(provide symbolic? foldl log rosette-number?
         (all-defined-out))

(define global-sol (sat (hash)))
(define (set-global-sol sol)
  (set! global-sol sol))

(define (rosette-number? x) (or (number? x) (symbolic? x)))

(define-syntax-rule (evaluate-with-sol x)
  ;(evaluate x))
  (let ([xx (evaluate x global-sol)])
    (if (bv? xx)
        (bitvector->integer xx)
        xx)))

(define-syntax-rule (solution->list sol)
  (hash->list (model sol)))


(define max-bit 18)
(define n-bit 16)

(define node-offset 0);10)
(define block-offset 800)
(define procs 4)
(define check-interval 60)
(define distributed #t)
(define max-unroll 20)
(define accurate-flow #t)

(define srcdir srcpath)
(define datadir datapath)
(define outdir #f)
;; (define path2src #f)

(define (set-outdir filepath name)
  (set! outdir (string-append
                (substring filepath 0 (cdr (last (regexp-match-positions* #rx"/" filepath))))
                "output-" name))
  (system (format "mkdir -p ~a" outdir))

  ;; (define outdir-split (string-split outdir "/"))
  ;; (define outdir-up-count (count (lambda (x) (equal? x "..")) outdir-split))
  ;; (define outdir-down-count (- (length outdir-split) outdir-up-count))

  ;; (define srcdir-split (string-split srcdir "/"))

  ;; (define path-list (append (for/list ([i (in-range outdir-down-count)]) "..")
  ;;       		    (reverse (take (reverse srcdir-split) outdir-up-count))))
  ;; (set! path2src (string-join path-list "/"))
  )

(struct meminfo (addr virtual data))

