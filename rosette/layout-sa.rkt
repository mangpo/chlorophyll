#lang s-exp rosette

(require "header.rkt" "visitor-flow.rkt")

(provide (all-defined-out))

(define (display-edges edges n w h)
  (pretty-display (format "~a ~a ~a" n w h))
  (for ([e edges])
       (pretty-display (format "~a ~a ~a" (edge-x e) (edge-y e) (edge-w e)))))

(define (vector-2d-set! vector n a b val)
  (unless (vector-ref vector a)
     (vector-set! vector a (make-vector n #f)))
  (vector-set! (vector-ref vector a) b val))

(define (index x y w)
  (+ (* x w) y))

(define (route core-a core-b w)
  (let ([a-x (floor (/ core-a w))]
        [a-y (modulo core-a w)]
        [b-x (floor (/ core-b w))]
        [b-y (modulo core-b w)])
    
    (define (move-y x y)
      (cond 
        [(< y b-y)
         (cons (index x y w) (move-y x (add1 y)))]
        [(> y b-y)
         (cons (index x y w) (move-y x (sub1 y)))]
        [else
         (list (index x y w))]))
    
    (define (move-x x y)
      (cond 
        [(< x b-x)
         (cons (index x y w) (move-x (add1 x) y))]
        [(> x b-x)
         (cons (index x y w) (move-x (sub1 x) y))]
        [else
         (move-y x y)]))
    
    (move-x a-x a-y)))
  

(define (gen-route flow-graph core2part w h)
  ;; Mapping partitions to cores in form of x*w + y
  (define n (* w h))
  (define part2core (make-vector n #f))
  (for ([partition core2part]
        [index (range n)])
    (vector-set! part2core partition index))
  
  ;; Mapping pair of partitions to route
  (define part2route (make-vector n #f))
  (for ([comm flow-graph])
    (let* ([a (edge-x comm)]
           [b (edge-y comm)]
           [path (route (vector-ref part2core a) (vector-ref part2core b) w)])
      (unless (vector-ref part2route a)
        (vector-set! part2route a (make-vector n #f)))
      (unless (vector-ref part2route b)
        (vector-set! part2route b (make-vector n #f)))
      
      (vector-2d-set! part2route n a b path)
      (vector-2d-set! part2route n b a (reverse path))))
  
  (for ([i (range n)])
    (pretty-display (vector-ref part2route i)))
  
  part2route)

;;test
;;(gen-route (list (edge 0 1 1) (edge 1 2 1) (edge 2 3 1))
;;           (list 0 1 2 3) 2 2)

(define (layout ast env num-cores w h name)
  ;; Generate flow graph represented by a list of edges
  (define flow-gen (new flow-generator% [env env]))
  (define flow-graph (send ast accept flow-gen))
  
  (with-output-to-file #:exists 'truncate (format "output/~a.graph" name)
    (lambda () (display-edges flow-graph num-cores w h)))
  
  ;; Convert a list of edges into a matrix
  (with-output-to-string 
   (lambda () (system (format "./qap/graph2matrix.py output/~a.graph > output/~a.dat" name name))))
  
  ;; Mapping from cores to partitions
  (define core2part
    ;; Output of sa_qap starts from 1, but we want to start from 0.
    (map (lambda (x)
           (sub1 (string->number x)))
         (string-split
          (last (string-split
                 (with-output-to-string
                  (lambda () (system (format "./qap/sa_qap output/~a.dat 10000000 3" name))))
                 "\n")))))

  (with-output-to-file #:exists 'truncate (format "output/~a.layout" name)
    (lambda () (display core2part)))
  
  ;; Create map from pair of core (x1,y1) (x2,y2) to routing
  (define part2route (gen-route flow-graph core2part w h))

  core2part
  )
