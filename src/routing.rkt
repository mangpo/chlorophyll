#lang racket
(require "header.rkt" "a-star.rkt"
         math/matrix)

(provide route-obs coords->index index->coords)

(define debug #f)

(define (coords->index x y w)
  (+ (* y w) x))

(define (index->coords core w)
  (cons (index-x core w) (index-y core w)))

(define (index-x core w) (modulo core w))
(define (index-y core w) (floor (/ core w)))

(define (make-map w h obstacles)
  (build-matrix h w (λ (y x)
		       (when 
			debug
			(pretty-display `(,x ,y ,(coords->index x y w)
					     ,(not (set-member? obstacles (coords->index x y w))))))
                       (not (set-member? obstacles (coords->index x y w))))))

(struct map-node (M x y) #:transparent)
(struct map-edge (src dx dy dest))

(define ((make-node-cost GX GY) n)
  (match-define (map-node M x y) n)
  (+ (abs (- x GX))
     (abs (- y GY))))

(define-unit map@
  (import) (export graph^)
 
  (define node? map-node?)
  (define edge? map-edge?)
  (define edge-src map-edge-src)
  (define edge-dest map-edge-dest)
 
  (define (edge-cost e) 1) ;; TODO: optimize routing by playing with cost

  (define (node-edges n)
    (match-define (map-node M x y) n)
    (append*
     (for*/list ([dx (in-list '(1 0 -1))]
                 [dy (in-list '(1 0 -1))]
                 #:when
                 (and (not (and (zero? dx) (zero? dy)))
                      (or (zero? dx) (zero? dy))))
       (cond
         [(and (<= 0 (+ dx x) (sub1 (matrix-num-cols M)))
               (<= 0 (+ dy y) (sub1 (matrix-num-rows M)))
               (matrix-ref M (+ dy y) (+ dx x)))
          (define dest (map-node M (+ dx x) (+ dy y)))
          (list (map-edge n dx dy dest))]
         [else
          empty])))))

(define (route-edges core-a core-b w h obstacles)
  ;; (pretty-display `(route ,core-a ,core-b ,w ,h ,obstacles))
  (define a-x (index-x core-a w))
  (define a-y (index-y core-a w))
  (define b-x (index-x core-b w))
  (define b-y (index-y core-b w))
  (when debug (pretty-display (list 'inside-route a-x a-y b-x b-y)))

  (define map (make-map w h obstacles))
  (when debug
	(for ([row (matrix-rows map)])
	     (pretty-display row)))
  (define ret (A* map@
                  (map-node map a-x a-y)
                  (make-node-cost b-x b-y)))
  ;; (unless ret
  ;;   (raise (format "routing: no available route between cores ~a and ~a"
  ;;                  core-a core-b)))
  ret)

(define (route-indices core-a core-b w h obstacles)
  (if (or (equal? core-a (* w h)) (equal? core-a (add1 (* w h)))
          (equal? core-b (* w h)) (equal? core-b (add1 (* w h))))
    (list core-a core-b)
    (let ([edges (route-edges core-a core-b w h obstacles)])
      (and edges
           (let ([nodes (if (empty? edges) empty
                            (cons (map-edge-src (first edges))
                                  (for/list ([edge edges])
                                            (map-edge-dest edge))))])
             (for/list ([node nodes])
                       (coords->index (map-node-x node) (map-node-y node) w)))))))

(define (route-obs core-a core-b w h obstacles)
  (define indices (route-indices core-a core-b w h obstacles))
  (pretty-display `(route-obs ,core-a ,core-b ,indices ,obstacles))
  (and indices (not (empty? indices)) indices))

