#lang s-exp rosette

(require "header.rkt" "visitor-flow.rkt")

(provide (all-defined-out))

(define (display-edges edges n w h)
  (pretty-display (format "~a ~a ~a" n w h))
  (for ([e edges])
       (pretty-display (format "~a ~a ~a" (edge-x e) (edge-y e) (edge-w e)))))


(define (layout ast env num-cores w h name)
  (define flow-gen (new flow-generator% [env env]))
  (define flow-graph (send ast accept flow-gen))
  ;(display-edges flow-graph)
  
  (with-output-to-file #:exists 'truncate (format "output/~a.graph" name)
    (lambda () (display-edges flow-graph num-cores w h)))
  
  (with-output-to-string 
   (lambda () (system (format "./qap/graph2matrix.py output/~a.graph > output/~a.dat" name name))))
  
  (define layout
    (string-split
     (last (string-split
	    (with-output-to-string
	      (lambda () (system (format "./qap/sa_qap output/~a.dat 10000000 3" name))))
	    "\n"))))

  (with-output-to-file #:exists 'truncate (format "output/~a.layout" name)
    (lambda () (display layout)))

  layout
  )

  
  
