#lang s-exp rosette

(require "header.rkt" "ast-util.rkt" "visitor-flow.rkt")

(provide (all-defined-out))

(define (display-edges edges n w h)
  (pretty-display (format "~a ~a ~a" n w h))
  (for ([e edges])
       (pretty-display (format "~a ~a ~a" (edge-x e) (edge-y e) (edge-w e)))))

;;test
;;(gen-route (list (edge 0 1 1) (edge 1 2 1) (edge 2 3 1))
;;           (list 0 1 2 3) 2 2)

(define (gen-part2core ast num-cores w h name)
  ;; Generate flow graph represented by a list of edges
  (define flow-gen (new flow-generator%))
  (define flow-graph (send ast accept flow-gen))
  
  (with-output-to-file #:exists 'truncate (format "~a/~a.graph" outdir name)
    (lambda () (display-edges flow-graph num-cores w h)))
  
  ;; Convert a list of edges into a matrix
  (with-output-to-string 
   (lambda () (system (format "./qap/graph2matrix.py ~a/~a.graph > ~a/~a.dat" outdir name outdir name))))
  
  ;; Mapping from cores to partitions
  (define core2part
    ;; Output of sa_qap starts from 1, but we want to start from 0.
    (map (lambda (x)
           (sub1 (string->number x)))
         (string-split
          (last (string-split
                 (with-output-to-string
                  (lambda () (system (format "./qap/sa_qap ~a/~a.dat 10000000 3" outdir name))))
                 "\n")))))

  (with-output-to-file #:exists 'truncate (format "~a/~a.layout" outdir name)
    (lambda () (display core2part)))

  (define n (* w h))
  (define part2core (make-vector n #f))
  (for ([partition core2part]
        [index (range n)])
       (vector-set! part2core partition index))
  
  part2core
  )
