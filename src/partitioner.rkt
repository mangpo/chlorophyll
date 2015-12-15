#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "partition-storage.rkt"
         "visitor-collector.rkt" 
         "visitor-evaluator.rkt"
         "visitor-flow.rkt"
         "visitor-heupartition.rkt"
         "visitor-interpreter.rkt" 
         "visitor-loopbound.rkt"
         "visitor-placeset.rkt"
         "visitor-placetype.rkt"
	 "visitor-postunroll.rkt"
         "visitor-printer.rkt"
         "visitor-rename.rkt"
         "visitor-unroll.rkt"
         )

(require rosette/solver/kodkod/kodkod)
(require rosette/solver/smt/z3)

(provide optimize-comm (struct-out result))

;; struct used to return result from optimize-comm
(struct result (msgs cores ast))

(define (optimize-comm my-ast #:name [name "temp"]
                        #:cores [num-core 144] 
                        #:capacity [capacity 256] 
                        #:refine-capacity [refine-capacity (make-vector num-core #f)]
                        #:refine-info [refine-info #f]
                        #:max-msgs [max-msgs #f]
			#:synthesis [synthesis #t]
                        #:verbose [verbose #f])
  
  ;; Define printer
  (define concise-printer (new printer% [out #t]))
  
  ;; Collect real physical places
  ;; (define collector (new place-collector% 
  ;;                        [collect? (lambda(x) (and (number? x) (not (symbolic? x))))]))
  ;; (define place-set (send my-ast accept collector))
  ;; (when verbose
  ;;   (pretty-display "\n=== Places ===")
  ;;   (pretty-print place-set))
  
  ;; Convert distinct abstract partitions into distinct numbers
  ;; and different symbolic vars for different holes
  ;; (define converter (new partition-to-number% [num-core num-core] [real-place-set place-set]))
  ;; (send my-ast accept converter)
  ;; (when verbose
  ;;   (pretty-display "\n=== After string -> number ===")
  ;;   (send my-ast pretty-print)
  ;;   (send my-ast accept concise-printer))

  ;; Place type linker
  (send my-ast accept (new placetype-linker%))
  (when verbose
    (pretty-display "=== After placetype linker  ===")
    (send my-ast pretty-print)
    )

  ;; Unroll
  (define start-loopbound (current-seconds))
  (send my-ast accept (new loopbound-computer%))
  (define stop-loopbound (current-seconds))
  (with-output-to-file #:exists 'truncate (format "~a/~a.time" outdir name)
    (lambda ()
      (pretty-display (format "loopbound time: ~a s" (- stop-loopbound start-loopbound)))))

  (when verbose
    (pretty-display "=== After bound compute  ===")
    (send my-ast pretty-print)
    )
  (send my-ast accept (new loop-unroller% [program my-ast]))
  (when verbose
    (pretty-display "=== After unroll  ===")
    (send my-ast pretty-print)
    )
  (send my-ast accept (new post-unroller%))
  (when verbose
    (pretty-display "=== After func-remover  ===")
    (send my-ast pretty-print)
    )

  (current-solver (new kodkod-incremental%))
  ;(current-solver (new z3%))
  ;(current-solver (new kodkod%))

  (current-bitwidth 32)
  
  ;; Count number of messages
  (define cores (make-cores #:capacity capacity #:max-cores num-core))
  (define interpreter (new count-msg-interpreter% [places cores]))
  
  ;; Place holder for solution
  (define num-msg #f)
  (define partial-hash (make-hash))

  
  (define (solve-function func-ast refine-capacity refine-part2capacity)
    (define start (current-seconds))
    (define num-msg (send func-ast accept interpreter))

    (let* ([nodes (list->vector (set->list (get-field used-io-nodes interpreter)))]
	   [len (vector-length nodes)])
      (for ([i (in-range len)])
	(for ([j (in-range (add1 i) len)])
	  (assert (not (equal? (hash-ref node-to-symbolic-core
					 (vector-ref nodes i))
			       (hash-ref node-to-symbolic-core
					 (vector-ref nodes j))))))))

    (cores-refine cores part2capacity)
    
    (when verbose
      (pretty-display "\n=== After interpreter ===")
      (send my-ast pretty-print))
      ;(send my-ast accept concise-printer))

    (when verbose (pretty-display `(num-msg , num-msg)))
    
    (define num-cores (cores-count cores)) 
    (define lowerbound 0)
    (define upperbound max-msgs)
    (define middle (if upperbound 
                       (floor (/ (+ lowerbound upperbound) 2))
                       #f))
    (define best-sol #f)
    
    (define (update-global-sol)
      ;; Unify solutions symbolically. Use this when solve function by function
      
      ;; Use this when solve the entire program at once.
      (set-global-sol best-sol)
      (cores-evaluate cores)
      (define stop (current-seconds))
      
      
      (when verbose
        (pretty-display "\n=== Update Global Solution ===")
        (send func-ast accept concise-printer) 
        (pretty-display global-sol)
        ;; (display-cores cores)
        (pretty-display (format "synthesis time = ~a sec" (- stop start)))
	(with-output-to-file #:exists 'append (format "~a/~a.time" outdir name)
	  (lambda ()
	    (pretty-display (format "partition time: ~a s" (- stop start)))))
      )
      )
    
    (define t 0)

    (define (inner-loop)
      (set! t (current-seconds))
      (assert (< num-msg (evaluate num-msg)))
      (solve #t)

      (pretty-display `(solve-time ,(- (current-seconds) t)))
      (set! best-sol (current-solution))
      
      ;; display
      (pretty-display (format "# messages = ~a" (evaluate num-msg)))
      (pretty-display (format "# cores = ~a" (evaluate num-cores)))
      
      (inner-loop))

    (define (outter-loop)
      (with-handlers* 
       ([exn:fail? (lambda (e) 
                     (pretty-display `(solve-time ,(- (current-seconds) t)))
                     (if (or (equal? (exn-message e)
                                     "solve: no satisfying execution found")
                             (equal? (exn-message e)
                                     "assert: failed"))
                         (begin
                           (update-global-sol)
                           (clear-asserts)
			   (current-solution (empty-solution))
                           )
                         (begin
                           (pretty-display e)
                           (raise e))))])
       (solve #t)
       (set! best-sol (current-solution))
       (inner-loop)))
    
    (outter-loop)
    )

  ;; For iterative refinement.
  (define refine-part2sym (if refine-info (car refine-info) (make-vector num-core #f)))
  (define part2capacity (if refine-info (cdr refine-info) (make-hash)))
  (define new-part2sym (make-vector num-core #f))
 
  ;; Construct part2capacity
  (for ([part (in-range num-core)])
       (let ([sym (vector-ref refine-part2sym part)]
	     [cap (vector-ref refine-capacity part)])
	 (when (and (not (equal? sym #f))
		    (or (not (hash-has-key? part2capacity sym))
			(> (hash-ref part2capacity sym) cap)))
               (hash-set! part2capacity sym cap))))
  
  ;; (define placeset-collector
  ;;   (new placeset-collector% [save #t]
  ;;        [actors (get-field actors my-ast)]
  ;;        [actors* (get-field actors* my-ast)]
  ;;        [need-cf (set)]))
  ;; (send my-ast accept placeset-collector)
  
  (placeset-collector-loop my-ast)
  (pretty-display "=== After bound compute  ===")
  (send my-ast pretty-print)
    
  (cond
   [synthesis
    (pretty-display "Running partitioning synthesizer ...")
    (send interpreter assert-conflict my-ast)

    (for ([decl (get-field stmts my-ast)])
	 (if 
	  ;;(is-a? decl FuncDecl%) ;; Use this for solving function by function
	  (and (is-a? decl FuncDecl%) (equal? (get-field name decl) "main"))
	  (begin
	    (solve-function decl refine-capacity part2capacity)
	    (when verbose (pretty-display "------------------------------------------------")))
	  (send decl accept interpreter)))

    ;; Construct new-part2sym.
    (define placeset (set))
    (for ([decl (get-field stmts my-ast)])
         (when (is-a? decl FuncDecl%)
               (set! placeset (set-union placeset (get-field body-placeset decl)))))
         
    (for ([p placeset])
         (unless (symbolic? p) (vector-set! new-part2sym p p)))

    (pretty-display global-sol)
    (for ([sol-pair (solution->list global-sol)])
         (let ([node (cdr sol-pair)])
           (when (and (>= node 0) (< node num-core))
                 (equal? (vector-ref new-part2sym (cdr sol-pair)) #f)
                 (vector-set! new-part2sym (cdr sol-pair) (car sol-pair)))))
    ]

   [else
    (pretty-display "Running heuristic partitioner ...")
    (define heu-start (current-seconds))
    (define-values (space network conflict-list) 
      (send my-ast accept (new heuristic-partitioner%)))
    (define result (merge-sym-partition num-core space network capacity 
					refine-capacity part2capacity
					conflict-list my-ast #:name name))
    (set-global-sol (sat (make-immutable-hash (hash->list (car result)))))
    (set! new-part2sym (cdr result))
    
    (with-output-to-file #:exists 'append (format "~a/~a.time" outdir name)
      (lambda ()
	(pretty-display (format "heuristic partition time: ~a s" 
				(- (current-seconds) heu-start)))))])

  (let ([evaluator (new symbolic-evaluator% [num-cores num-core])])
    (send my-ast pretty-print)
    (send my-ast accept evaluator)
    )

  (with-output-to-file #:exists 'truncate (format "~a/~a.part" outdir name)
    (lambda () (send my-ast accept concise-printer)))
  
  ;; (pretty-display "\n=== Final Solution ===")
  ;; (send my-ast accept concise-printer)
  ;; (display-cores cores)
  
  (when verbose
    (pretty-display "\n=== After evaluate ===")
    (send my-ast accept concise-printer)
    (send my-ast pretty-print)
    )

  (placeset-collector-loop my-ast)
  (send my-ast pretty-print)
  
  (cons new-part2sym part2capacity)
  )

(define (placeset-collector-loop my-ast)
  ;; Handle actors
  (define actors (get-field actors my-ast))
  (define actors* (get-field actors* my-ast))
  (define all-main-actors (set))
  (pretty-display `(all-main-actors ,all-main-actors))
  (for* ([actor-list (hash-values actors)]
         [pair actor-list])
        (set! all-main-actors (set-add all-main-actors (car pair))))
  (for* ([actor-list (hash-values actors*)]
         [pair actor-list])
        (set! all-main-actors (set-add all-main-actors (car pair))))
  
  (define (loop need-cf)
    (define placeset-collector
      (new placeset-collector% [save #t]
           [actors actors]
           [actors* actors*]
           [need-cf need-cf]))
    (send my-ast accept placeset-collector)

    ;; extra actors that need cf
    (define actors*-need-cf
      (send placeset-collector get-actors*-need-cf my-ast all-main-actors))
    (pretty-display `(actors*-need-cf ,need-cf ,actors*-need-cf))
    (if (set-empty? actors*-need-cf)
        (send placeset-collector get-actors*-no-cf-map need-cf)
        (loop (set-union need-cf actors*-need-cf)))
    )
  (set-field! actors*-no-cf-map my-ast (loop (set)))
  (pretty-display `(actors*-no-cf-map ,(get-field actors*-no-cf-map my-ast)))
  )

#|
(define t (current-seconds))
(define my-ast (parse "../tests/add-pair.cll"))
(result-msgs (optimize-comm my-ast #:cores 16 #:capacity 300 #:verbose #t))
(pretty-display (format "partitioning time = ~a" (- (current-seconds) t)))
|#
