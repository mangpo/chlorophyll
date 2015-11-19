#lang s-exp rosette

(require "path.rkt" "header.rkt" "ast-util.rkt" "visitor-flow.rkt" "ast.rkt"
         "routing.rkt"
         )

(provide (all-defined-out) (struct-out layoutinfo))

(struct layoutinfo (routes part2core core2part))

(define (display-edges edges n w h)
  (pretty-display (format "~a ~a ~a" n w h))
  (for ([e edges])
       (pretty-display (format "~a ~a ~a" (edge-x e) (edge-y e) (edge-w e)))))

(define (get-partitions edges)
  (define ans (set))
  (for ([e edges])
       (set! ans (set-add ans (edge-x e)))
       (set! ans (set-add ans (edge-y e))))
  ans)

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

(define (set-remove* x a b)
  (when (set-member? x a) (set! x (set-remove x a)))
  (when (set-member? x b) (set! x (set-remove x b)))
  x)
  
;; Gnerate (w x h + 1) x (w x h + 1) table
;; w x h corresponds to io
(define (gen-route flow-graph part2core ast w h)
  (pretty-display "Generating routes...")
  (define noroute (get-field noroute ast))
  (define actors (get-field actors ast))
  (define obs? (> (+ (set-count noroute) (hash-count actors)) 0))
              
  ;; Mapping partitions to cores in form of x*w + y
  (define n-1 (* w h))
  (define n (add1 n-1))
  (define cores
    (list->set
     (for/list ([part (get-partitions flow-graph)])
               (vector-ref part2core part))))
  (define obstacles
    (list->set (for/list ([part noroute]) (vector-ref part2core part))))
  
  ;; Mapping pair of partitions to route
  (define core2route (make-vector n #f))
  (for ([i (in-range n-1)])
       (vector-set! core2route i (make-vector n #f)))
  
  ;; Convert actors to physcial cores
  (define new-actors (make-hash))
  (for ([func-lst (hash->list actors)])
       (let* ([func (car func-lst)]
              [lst (cdr func-lst)]
              [new-lst (map (lambda (pair)
                              (cons (vector-ref part2core (car pair))
                                    (vector-ref part2core (cdr pair))))
                            lst)]
              [all-actors (list->set (map car new-lst))]
              [new-obstacles obstacles]
              ;; don't route through other compute nodes.
              [my-obstacles
               (set-union obstacles (set-subtract cores all-actors))]
              )
         (pretty-display `(func ,func ,obstacles ,all-actors ,my-obstacles))
         (hash-set! new-actors func new-lst)
         (for* ([i (list->set (map car new-lst))]
                [j (list->set (map cdr new-lst))])
              ;; (let* ([i (car pair)]
               ;;        [j (cdr pair)]
               (when
                (and (not (= i j))
		     (not (vector-2d-ref core2route i j)))
                (let ([path
                       (route-obs
                        i j w h (set-remove* my-obstacles i j))])
                  (unless
                   path
                   (raise (format "routing: no available route between cores ~a and ~a"
                                  i j)))
                  ;; (pretty-display `(path ,i ,j ,path))
                  (vector-2d-set! core2route n i j path)
                  (vector-2d-set! core2route n j i (reverse path))
                  (set! new-obstacles (set-union new-obstacles (list->set path))))))
         (set! obstacles new-obstacles)))
  (set-field! actors ast new-actors)
  (set-field! noroute ast obstacles)
  (pretty-display `(gen-route ,cores ,new-actors ,obstacles))
  
  (for* ([i cores]
         [j cores])
         (when (and (< i j)
                    (not (vector-2d-ref core2route i j)))
               (let ([path 
                      (if obs?
                          (route-obs i j w h (set-remove* obstacles i j))
                          (route i j w))])
                 (when
                  path
                  ;; (pretty-display `(path ,i ,j ,path))
                  (vector-2d-set! core2route n i j path)
                  (vector-2d-set! core2route n j i (reverse path))))))

  (vector-set! core2route n-1 (make-vector n #f))
  (for ([i (in-range n)])
       (vector-2d-set! core2route n i n-1 (list i n-1))
       (vector-2d-set! core2route n n-1 i (list n-1 i)))
  
  core2route)

(define (layout ast num-cores w h name weight)
  ;; Generate flow graph represented by a list of edges
  (define flow-gen (new flow-generator%))
  (define flow-graph (send ast accept flow-gen))
  
  (with-output-to-file #:exists 'truncate (format "~a/~a.graph" outdir name)
    (lambda () (display-edges flow-graph num-cores w h)))
  
  ;; Convert a list of edges into a matrix
  (with-output-to-string 
   (lambda () (system (format "~a/qap/graph2matrix.py ~a/~a.graph ~a > ~a/~a.dat" 
                              srcpath
			      outdir name 
			      (if weight "--weight" "--noweight")
			      outdir name))))

  (with-output-to-file #:exists 'append (format "~a/~a.dat" outdir name)
    (lambda () 
      (define fix (make-vector (* w h)))
      (when (= w 18)
        ;; IO functions only works, if w = 18.
        (for ([core (hash-keys node-to-symbolic-core)])
             (let ([n (evaluate-with-sol (hash-ref node-to-symbolic-core core))])
               (when (and (not (term? n)) (>= n 0) (< n (* w h)))
                 (vector-set! fix (+ (* (quotient core 100) w) (modulo core 100))
                              (add1 n))))))

      ;; Set to talk to Polyforth
      ;; (vector-set! fix (* w 2) (* w h)) ;; index [physical] (no +1), value [logical] (+1)

      (for ([mapping (get-field fixed-parts ast)])
	   (let ([part (car mapping)]
		 [core (cdr mapping)])
	     (vector-set! fix (+ (* (quotient core 100) w) (modulo core 100))
			  (add1 part))))
	   
      (for ([i (in-range (* w h))])
        (display (vector-ref fix i)) (display " "))
      (newline)))
  
  ;; Mapping from cores to partitions
  (define start (current-seconds))
  (define core2part
    ;; Output of sa_qap starts from 1, but we want to start from 0.
    (map (lambda (x)
           (sub1 (string->number x)))
         (string-split
          (last (string-split
                 (with-output-to-string
                   ;; Use this line for fast but low quality layout.
                   (lambda () (system (format "~a/qap/sa_qap ~a/~a.dat 10000000 3" 
                                              srcpath outdir name)))
                   ;; Use this line if we need better layout.
                   ;; (lambda () (system (format "~a/qap/sa_qap ~a/~a.dat 100000000 3" 
                   ;;                            srcpath outdir name)))
                  )
                 "\n")))))
  (define stop (current-seconds))
  (with-output-to-file #:exists 'append (format "~a/~a.time" outdir name)
    (lambda ()
      (pretty-display (format "layout time: ~a s" (- stop start)))))

  (with-output-to-file #:exists 'truncate (format "~a/~a.layout" outdir name)
                       (lambda () (display core2part)))

  (define n (* w h))
  (define part2core (make-vector n #f))
  (for ([partition core2part]
        [index (range n)])
       (vector-set! part2core partition index))
  
  ;; Create map from pair of core (x1,y1) (x2,y2) to routing
  (define routing-table
    (gen-route flow-graph part2core ast w h))

  (layoutinfo routing-table part2core (list->vector core2part))
  )
