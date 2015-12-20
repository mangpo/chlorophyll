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

(define (gen-route-i-j i j w h obstacles cores
                       conflicts conflict-index
                       actors-map actor-index)
  (define my-obs obstacles)
  (for ([more-obs (hash-values actors-map)])
       (set! my-obs (set-union my-obs more-obs)))

  (let* ([conflict-i (vector-ref conflict-index i)]
         [conflict-j (vector-ref conflict-index j)]
         [actor-i (vector-ref actor-index i)]
         [actor-j (vector-ref actor-index j)]
         [conflict-indices (list)]
         [update-obs
          (lambda (index-i update)
            (when update
                  (set! conflict-indices (cons index-i conflict-indices)))
            ;; adding obstacle from the other paralle units.
            (for ([group (vector-ref conflicts (car index-i))]
                  [id (in-naturals)])
                 (unless (= id (cdr index-i))
                         (set! my-obs (set-union my-obs group)))))]
         [actor-indices (list)]
         )

    ;; substract my actors
    (for ([index-i actor-i])
         (set! my-obs (set-subtract my-obs (hash-ref actors-map index-i))))
    (for ([index-j actor-j])
         (set! my-obs (set-subtract my-obs (hash-ref actors-map index-j))))
    (for* ([index-i actor-i]
           [index-j actor-j])
          (when (equal? index-i index-j)
                (set! actor-indices (cons index-i actor-indices))))

    ;; add parallel constraints
    (cond
     ;; [(and (empty? conflict-i) (empty? conflict-j)) (void)]
     [(empty? conflict-i) (for ([index-j conflict-j]) (update-obs index-j #f))]
     [(empty? conflict-j) (for ([index-i conflict-i]) (update-obs index-i #f))]
     [else
      (for* ([index-i conflict-i]
             [index-j conflict-j])
            ;; if in the same parallel unit.
            (when (equal? index-i index-j)
                  (update-obs index-i #t)))
      ])

    (let ([path (list)])
      (cond
       [(> (set-count my-obs) 0)
        (pretty-display `(my-obs ,my-obs))
        (set! path (route-obs i j w h (set-remove* my-obs i j)))
        (unless path (pretty-display `(conflicts ,conflicts)))
        ;; update date conflict-list & conflict-index
        (for ([index conflict-indices])
             (let* ([conflict (vector-ref conflicts (car index))]
                    [current (vector-ref conflict (cdr index))])
               (for ([core path])
                    (unless
                     (set-member? current core)
                     ;; add this core to the parallel unit.
                     (vector-set! conflict-index core
                                  (cons index
                                        (vector-ref conflict-index core)))))
               (vector-set! conflict (cdr index)
                            (set-union (list->set path) current))))
        ;; update actors-map & actor-index
        (pretty-display `(actor-indices ,actor-indices))
        (let ([additions (set-subtract (list->set path) cores)])
          (for ([index actor-indices])
               (let ([current (hash-ref actors-map index)])
                 (for ([core additions])
                      (unless
                       (set-member? current core)
                       (vector-set! actor-index core
                                    (cons index
                                          (vector-ref actor-index core)))))
                 (hash-set! actors-map index
                            (set-union additions current)))))
        ]
       [else
        (set! path (route i j w))])
      (set-union! cores (list->set path))
      (pretty-display `(done))
      path
      )))
  
;; Gnerate (w x h + 1) x (w x h + 1) table
;; w x h corresponds to io
(define (gen-route flow-graph part2core ast w h)
  (pretty-display "Generating routes...")
  (define n-1 (* w h))
  (define n (add1 n-1))
  
  (define conflict-list (get-field conflict-list ast))
  (define conflicts (car conflict-list))
  (define conflict-index (cdr conflict-list))
  
  (define obstacles (get-field noroute ast))
              
  ;; Mapping partitions to cores in form of x*w + y
  (define cores
    (list->mutable-set
     (for/list ([part (get-partitions flow-graph)])
               (vector-ref part2core part))))
  
  ;; Mapping pair of partitions to route
  (define core2route (make-vector n #f))
  (for ([i (in-range n-1)])
       (vector-set! core2route i (make-vector n #f)))

  ;; (define (keep-routing func group my-obstacles)
  ;;   (define queue (for*/list ([x group] [y group]) (cons x y)))
  ;;   (define new-group group)
  ;;   (define visited (set))
  ;;   (define (loop)
  ;;     (unless
  ;;      (empty? queue)
  ;;      (define i (caar queue))
  ;;      (define j (cdar queue))
  ;;      (pretty-display `(loop ,i ,j))
  ;;      (set! queue (cdr queue))
  ;;      (set! visited (set-union visited (set (cons i j) (cons j i))))
       
  ;;      (when (and (not (= i j)) (not (vector-2d-ref core2route i j)))
  ;;            (define path
  ;;              (gen-route-i-j i j w h obs? (set-remove* my-obstacles i j)
  ;;                             conflicts conflict-index))
  ;;            (unless path (raise (format "routing: no available route between cores ~a and ~a" i j)))
  ;;            ;; (pretty-display `(path ,i ,j ,path))
  ;;            (vector-2d-set! core2route n i j path)
  ;;            (vector-2d-set! core2route n j i (reverse path))
  ;;            (for* ([x path]
  ;;                   [y new-group])
  ;;                  (when (and (not (set-member? visited (cons x y)))
  ;;                             (not (member (cons x y) queue))
  ;;                             (not (member (cons y x) queue)))
  ;;                        (set! queue (cons (cons x y) queue))))
  ;;            (set! new-group (set-union new-group (list->set path))))
  ;;      (loop)))
  ;;   (loop)
  ;;   new-group)

  (define actors (get-field actors ast))
  (define actors* (get-field actors* ast))
  (define actors*-no-cf-map (get-field actors*-no-cf-map ast))
  
  (define new-actors*-no-cf-map (make-hash))
  (define actor-index (make-vector n (list)))
  
  (define new-actors (make-hash))
  (define (route-caller-actor func actors)
    (let* ([lst (hash-ref actors func)]
           [new-lst (map (lambda (pair)
                           (cons (vector-ref part2core (car pair))
                                 (vector-ref part2core (cdr pair))))
                         lst)]
           [all-actors (list->set (map car new-lst))]
           [all-callers (list->set (map cdr new-lst))]
           [cores-before (for/set ([x cores]) x)])
      (hash-set! new-actors func new-lst)
      (for ([j (map car new-lst)]  ;; actor
            [i (map cdr new-lst)]) ;; caller
           (when
            (and (not (= i j)) (not (vector-2d-ref core2route i j)))
            (pretty-display `(caller-actor ,i ,j))
            (let ([path (gen-route-i-j i j w h obstacles cores
                                       conflicts conflict-index
                                       new-actors*-no-cf-map actor-index)])
              (unless path (raise (format "routing: no available route between cores ~a and ~a" i j)))
              ;; (pretty-display `(path ,i ,j ,path))
              (vector-2d-set! core2route n i j path)
              (vector-2d-set! core2route n j i (reverse path))
              (let ([additions
                     (set-subtract (list->set (drop path 1)) cores-before)])
                ;;(pretty-display `(additions ,additions ,(drop path 1) ,cores-before))
                (hash-set! new-actors*-no-cf-map func
                           (set-union
                            (hash-ref new-actors*-no-cf-map func)
                            additions))))))))

  ;; actors
  (for ([name (hash-keys actors*-no-cf-map)])
       (let ([group (hash-ref actors*-no-cf-map name)])
         (hash-set!
          new-actors*-no-cf-map name
          (for/set ([x group])
                   (let ([core (vector-ref part2core x)])
                     (vector-set! actor-index core
                                  (cons name (vector-ref actor-index core)))
                     core)))))
  (for ([name (hash-keys actors)])
       (hash-set!
        new-actors*-no-cf-map name
        (list->set
         (map (lambda (x)
                (let ([core (vector-ref part2core (car x))])
                  (vector-set! actor-index core
                               (cons name (vector-ref actor-index core)))
                  core))
              (hash-ref actors name)))))
  
  (pretty-display `(cores-before ,cores))
  (pretty-display `(actors*-map-before ,new-actors*-no-cf-map))
  
  (for ([name (hash-keys actors*)])
       (route-caller-actor name actors*))
  (for ([name (hash-keys actors)])
       (route-caller-actor name actors))

  ;; update
  (set-field! actors ast new-actors) ;; store routing from caller -> actor
  (set-field! actors* ast actor-index)
  (set-field! actors*-no-cf-map ast new-actors*-no-cf-map) ;; store no-cf nodes
  (pretty-display `(actors*-map-after ,new-actors*-no-cf-map))
  (pretty-display `(actors ,new-actors))
  (pretty-display `(actor-index ,actor-index))
  
  (set-field! noroute ast obstacles)
  (set-field! cores ast cores)
  (pretty-display `(gen-route ,cores ,obstacles))

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

      (define parts-fixed (mutable-set))
      (define cores-fixed (mutable-set))
      (for ([mapping (get-field fixed-parts ast)])
	   (let* ([part (car mapping)]
                  [core (cdr mapping)]
                  [core-id (+ (* (quotient core 100) w) (modulo core 100))])
             (set-add! parts-fixed part)
             (set-add! cores-fixed core)
	     (vector-set! fix core-id (add1 part))))

      (newline)
      (for ([i (in-range (* w h))])
        (display (vector-ref fix i)) (display " "))
      (newline) (newline)

      ;; Pinning clusters.
      (define clusters (get-field module-inits ast))
      (pretty-display (length clusters))
      (for ([cluster clusters])
           (let* ([parts (set-subtract (car cluster) parts-fixed)]
                  [cores (set-subtract (list->set (cdr cluster)) cores-fixed)]
                  [n-parts (set-count parts)]
                  [n-cores (set-count cores)]
                  )
             (when (> n-parts n-cores)
                   (raise "Cannot pin a module instance because # of logical partitions inside the instance exceeds # of provided cores."))
             (display n-parts) (display " ")
             (for ([p parts]) (display (add1 p)) (display " "))
             (newline)
             (display n-cores) (display " ")
             (for ([core cores])
                  (display (+ 1 (* (quotient core 100) w) (modulo core 100)))
                  (display " "))
             (newline)
             ))
      ))
  
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
                   (lambda () (system (format "~a/qap/sa_qap ~a/~a.dat 30000000 5" 
                                              srcpath outdir name)))
                   ;; Use this line if we need better layout.
                   ;; (lambda () (system (format "~a/qap/sa_qap ~a/~a.dat 10000000 3" 
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

  (with-output-to-file #:exists 'truncate (format "~a/~a.part2core" outdir name)
     (lambda ()
       (pretty-display "part core")              
       (for ([part n])
            (pretty-display (format "~a ~a"
                                    part (vector-ref part2core part))))))

  ;; Convert obstacles and conflict-list.
  (set-field! noroute ast
              (for/set ([part (get-field noroute ast)])
                       (vector-ref part2core part)))
  (define conflict-index (make-vector n (list)))
  (define conflicts
    (for/vector
     ([conflict (get-field conflict-list ast)]
      [id1 (in-naturals)])
     (for/vector
      ([group conflict]
       [id2 (in-naturals)])
      (for/set ([part group])
               (let ([core (vector-ref part2core part)])
                 (vector-set! conflict-index core
                              (cons (cons id1 id2)
                                    (vector-ref conflict-index core)))
                 core)))))
  (set-field! conflict-list ast (cons conflicts conflict-index))
  (pretty-display `(init-conflict-list ,conflicts))
  
  ;; Create map from pair of core (x1,y1) (x2,y2) to routing
  (define routing-table
    (gen-route flow-graph part2core ast w h))

  (layoutinfo routing-table part2core (list->vector core2part))
  )
