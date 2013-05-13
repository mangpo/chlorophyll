#lang s-exp rosette

(require "header.rkt" "visitor-flow.rkt")

(provide (all-defined-out))

(define (display-edges edges)
  (for ([e edges])
       (pretty-display (format "~a--~a , w = ~a" (edge-x e) (edge-y e) (edge-w e)))))

(define (layout ast env num-cores w h)
  (define flow-gen (new flow-generator% [env env]))
  (define flow-graph (send ast accept flow-gen))
  (display-edges flow-graph)
  ;(define distance-graph (gen-distance-graph w h))
  
  )

  
  