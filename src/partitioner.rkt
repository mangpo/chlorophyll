#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "parser.rkt" 
         "symbolic-dict.rkt"
         "visitor-interpreter.rkt" 
         "visitor-collector.rkt" 
         "visitor-rename.rkt"
         "visitor-printer.rkt"
         "visitor-evaluator.rkt")

(provide optimize-comm (struct-out result))

;; Set bidwidth for rosette
;(configure [bitwidth bitwidth])

;; struct used to return result from optimize-comm
(struct result (msgs cores ast))

;; Concrete version
(define (concrete)
  (define my-ast (ast-from-file "examples/test.cll"))
  (define collector (new place-collector% 
                         [collect? (lambda(x) 
                                     (and (and (number? x) (not (symbolic? x)))
                                          (not (is-a? x Place%))))
                                   ]))
  (define place-set (send my-ast accept collector))
  (pretty-print place-set)
  (define converter (new partition-to-number% [num-core 16] [real-place-set place-set]))
  (send my-ast accept converter)
  (send my-ast pretty-print)
  
  (define interpreter (new count-msg-interpreter% [core-space 256] [num-core 4]))
  (define num-msg (send my-ast accept interpreter))
  ;(send my-ast pretty-print)
  (pretty-display (format "# messages = ~a" num-msg))
  (send interpreter display-used-space)
  )

;(concrete)

;; this part verify that solve should be able to find a solution.
(define (foo)
  (define my-ast (ast-from-file "examples/symbolic.mylang"))
  (define interpreter (new count-msg-interpreter% [core-space 256] [num-core 3]))
  (define num-msg (send my-ast accept interpreter))
  (send my-ast pretty-print)
  ;(send interpreter display-used-space)
  (pretty-display (format "# messages = ~a" num-msg))
  ;(send interpreter assert-capacity)
  (assert (= num-msg 3))
)

(define (unsat-core)
  (define-values (out asserts) (with-asserts (foo)))
  asserts

  (send (current-solver) clear)
  (send/apply (current-solver) assert asserts)
  (send (current-solver) debug)
  )

;(unsat-core)

(define (simple-syn2)
  (define my-ast (ast-from-file "examples/simple-hole.cll"))
  (define interpreter (new count-msg-interpreter% [core-space 256] [num-core 3]))
  (define num-msg (send my-ast accept interpreter))
  (send my-ast pretty-print)
  (pretty-display (format "# messages = ~a" num-msg))
  
  (let ([collector (new place-collector% [collect? symbolic?])])
    (pretty-display (send my-ast accept collector))
    (synthesize #:forall (set->list (send my-ast accept collector))
                #:assume #t
                #:guarantee (assert #t))
    )
  
  ;(send interpreter display-used-space)
  ;(solve (assert (= num-msg 3)))
  (current-solution)
  )

;;; AST printer
;;; (send my-ast pretty-print)

;;; Concise printer
;;; (define concise-printer (new printer%))
;;; (send my-ast accept concise-printer)

(define (optimize-comm my-ast #:name [name "temp"]
                        #:cores [num-core 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [max-msgs #f]
                        #:verbose [verbose #f])

  (configure [bitwidth 32])
  
  ;; Define printer
  (define concise-printer (new printer% [out #t]))
  
  ;; Easy inference happens here
  (when verbose
    (pretty-display "=== Original AST ===")
    (send my-ast pretty-print)
    )
  
  ;; Collect real physical places
  (define collector (new place-collector% 
                         [collect? (lambda(x) (and (number? x) (not (symbolic? x))))]))
  (define place-set (send my-ast accept collector))
  (when verbose
    (pretty-display "\n=== Places ===")
    (pretty-print place-set))
  
  ;; Convert distinct abstract partitions into distinct numbers
  ;; and different symbolic vars for different holes
  (define converter (new partition-to-number% [num-core num-core] [real-place-set place-set]))
  (send my-ast accept converter)
  (when verbose
    (pretty-display "\n=== After string -> number ===")
    (send my-ast pretty-print)
    (send my-ast accept concise-printer))
  
  ;; Count number of messages
  (define cores (make-cores #:capacity capacity #:max-cores num-core))
  (define interpreter (new count-msg-interpreter% [places cores]))
  
  ;; Place holder for solution
  (define num-msg #f)
  (define partial-hash (make-hash))
  
  (define (solve-function func-ast)
    (define start (current-seconds))
    (set! num-msg (evaluate-with-sol (comminfo-msgs (send func-ast accept interpreter))))

    (when verbose (pretty-display `(num-msg , num-msg)))
    
    (define num-cores (evaluate-with-sol (cores-count cores)))  
    (define lowerbound 0)
    (define upperbound max-msgs)
    (define middle (if upperbound 
                       (floor (/ (+ lowerbound upperbound) 2))
                       #f))
    (define best-sol #f)
    
    (define (update-global-sol)
      ;; Unify solutions symbolically. Use this when solve function by function
      #|(define unified-hash (make-hash))
      (define concrete-to-sym (make-hash))
      
      (for ([mapping (solution->list global-sol)])
        (let ([key (car mapping)]
              [val (evaluate (cdr mapping) best-sol)])
          (hash-set! concrete-to-sym val key)
          (hash-set! unified-hash key val)))
      
      (for ([mapping (solution->list best-sol)])
        (let ([key (car mapping)]
              [val (evaluate (cdr mapping) best-sol)])
          (when (not (hash-has-key? unified-hash key))
            (hash-set! concrete-to-sym val key)
            (hash-set! unified-hash key val))))
      
      (if (equal? (get-field name func-ast) "main")
          (set-global-sol (sat (make-immutable-hash (hash->list unified-hash))))
          (let ([global-hash (make-hash)])
            (hash-for-each unified-hash 
                           (lambda (key val) 
                             (hash-set! global-hash key (hash-ref concrete-to-sym val))))
            (set-global-sol (sat (make-immutable-hash (hash->list global-hash))))))   

      ;; Unify solutions concretely. Don't use this
      (for ([mapping (solution->list best-sol)])
        (let ([key (car mapping)]
              [val (cdr mapping)])
          (when (not (hash-has-key? partial-hash key))
            (hash-set! partial-hash key val))))
        
      (set-global-sol (sat (make-immutable-hash (hash->list partial-hash)))|#
      
      ;; Use this when solve the entire program at once.
      (set-global-sol best-sol)
      
      (cores-evaluate cores)
      
      (define stop (current-seconds))
      
      (with-output-to-file #:exists 'truncate (format "~a/~a.bestsofar" outdir name)
        (lambda () (send my-ast accept concise-printer)))
      
      (when verbose
        (pretty-display "\n=== Update Global Solution ===")
        (send func-ast accept concise-printer) 
        (pretty-display global-sol)
        (display-cores cores)
        (pretty-display (format "synthesis time = ~a sec" (- stop start)))
        )
      )
    
    (define t 0)
      
                
    (define (inner-loop)
      (pretty-display (format "num-msg <= ~a" middle))
      #|(if middle
          (solve-with-sol (assert (<= num-msg middle)) global-sol)
          (solve-with-sol (assert #t) global-sol))|#
      (set! t (current-seconds))
      (if middle
          (solve (assert (<= num-msg middle)))
          (solve (assert #t)))
      (pretty-display `(solve-time ,(- (current-seconds) t)))
      (set! upperbound (evaluate num-msg))
      (set! middle (floor (/ (+ lowerbound upperbound) 2)))
      
      (set! best-sol (current-solution))
      
      ;; display
      (pretty-display (format "# messages = ~a" (evaluate num-msg)))
      (pretty-display (format "# cores = ~a" (evaluate num-cores)))
      
      (if (< lowerbound upperbound)
          (inner-loop)
          (update-global-sol))
      )
    
    ;void
    (define (outter-loop)
      (with-handlers* ([exn:fail? (lambda (e) void)
                      (inner-loop)))))
    
    (outter-loop)
    )
    
  (for ([decl (get-field stmts my-ast)])
    (if 
     ;(is-a? decl FuncDecl%) ;; Use this for solving function by function
     (and (is-a? decl FuncDecl%) (equal? (get-field name decl) "main"))
        (begin
          (solve-function decl)
          (when verbose (pretty-display "------------------------------------------------")))
        (send decl accept interpreter)))

  (with-output-to-file #:exists 'truncate (format "~a/~a.part" outdir name)
    (lambda () (send my-ast accept concise-printer)))
  
  (pretty-display "\n=== Final Solution ===")
  (send my-ast accept concise-printer)
  (display-cores cores)
  
  (let ([evaluator (new symbolic-evaluator%)])
    (send my-ast accept evaluator)
    ;(send my-ast pretty-print)
    )
  
  (result (evaluate-with-sol num-msg) 
          cores 
          my-ast)
)

(require "visitor-linker.rkt" "visitor-tempinsert.rkt" "visitor-desugar.rkt")
 
(define (parse file)
  (define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (pretty-display "--------------- ast ------------------")
  (send my-ast pretty-print)
  
  (define need-temp (send my-ast accept (new linker%)))
  (pretty-display "--------------- linker ------------------")
  (send my-ast pretty-print)
  
  (send my-ast accept (new temp-inserter%))
  (pretty-display "--------------- temp ------------------")
  (send my-ast pretty-print)
  
  (send my-ast accept (new desugar%))
  (pretty-display "--------------- desugar ------------------")
  (send my-ast pretty-print)
  
  my-ast)
