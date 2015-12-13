#lang racket

;; Adapted from http://jeapostrophe.github.io/2013-04-15-astar-post.html

(require rackunit
         racket/unit
         racket/match
         racket/list
         data/heap
         2htdp/image
         racket/runtime-path)

(provide (all-defined-out))
 
(define-signature graph^
  (node? edge? node-edges edge-src edge-cost edge-dest))
 
(define (A* graph@ initial node-cost)
  (define-values/invoke-unit graph@ (import) (export graph^))
  (define count 0)

  (define node->best-path (make-hash))
  (define node->best-path-cost (make-hash))
   
  (hash-set! node->best-path      initial empty)
  (hash-set! node->best-path-cost initial 0)

  (define (node-total-estimate-cost n)
    (+ (node-cost n) (hash-ref node->best-path-cost n)))
  (define (node-cmp x y)
    (<= (node-total-estimate-cost x)
        (node-total-estimate-cost y)))
  (define open-set (make-heap node-cmp))
  (heap-add! open-set initial)
 
  (begin0
    (let/ec esc
      (for ([x (in-heap/consume! open-set)])
        (set! count (add1 count))
        (define h-x (node-cost x))
        (define path-x (hash-ref node->best-path x))
         
        (when (zero? h-x)
          (esc (reverse path-x)))
         
        (define g-x (hash-ref node->best-path-cost x))
        (for ([x->y (in-list (node-edges x))])
          (define y (edge-dest x->y))
          (define new-g-y (+ g-x (edge-cost x->y)))
          (define old-g-y
            (hash-ref node->best-path-cost y +inf.0))
          (when (< new-g-y old-g-y)
            (hash-set! node->best-path-cost y new-g-y)
            (hash-set! node->best-path y (cons x->y path-x))
            (heap-add! open-set y))))
      #f)
 
    ;;(printf "visited ~a nodes\n" count)
    ))
 
