#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "parser.rkt" 
         "symbolic-dict.rkt"
         "visitor-interpreter.rkt" 
         "visitor-collector.rkt" 
         "visitor-rename.rkt"
         "visitor-printer.rkt")

(provide optimize-comm (struct-out result))

;; Set bidwidth for rosette
;(configure [bitwidth bitwidth])

;; struct used to return result from optimize-comm
(struct result (msgs cores))

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

(define (optimize-comm file 
                        #:cores [best-num-cores 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [upperbound #f]
                        #:verbose [verbose #f])
  
  (if upperbound
      (let ([bitwidth (+ (inexact->exact (ceiling 
                                          (/ (log (max best-num-cores capacity upperbound)) 
                                             (log 2)))) 2)])
      
        ;; Set bidwidth for rosette
        (pretty-display (format "bidwidth = ~a" bitwidth))
        (configure [bitwidth bitwidth]))
      (configure [bitwidth 16]))
  
  ;; Define printer
  (define concise-printer (new printer%))
  
  ;; Easy inference happens here
  (define my-ast (ast-from-file file))
  (when verbose
    (pretty-display "=== Original AST ===")
    (send my-ast pretty-print))
  
  ;; Collect real physical places
  (define collector (new place-collector% 
                         [collect? (lambda(x) (and (number? x) (not (symbolic? x))))]))
  (define place-set (send my-ast accept collector))
  (when verbose
    (pretty-display "\n=== Places ===")
    (pretty-print place-set))
  
  ;; Convert distinct abstract partitions into distinct numbers
  ;; and different symbolic vars for different holes
  (define converter (new partition-to-number% [num-core 16] [real-place-set place-set]))
  (send my-ast accept converter)
  (when verbose
    (pretty-display "\n=== After string -> number ===")
    (send my-ast pretty-print))
  
  ;; Count number of messages
  (define cores (make-cores #:capacity capacity #:max-cores best-num-cores))
  (define interpreter (new count-msg-interpreter% [places cores]))
  (define best-sol #f)
  
  (define num-msg (comminfo-msgs (send my-ast accept interpreter)))
  (define num-cores (cores-count cores))
  (define lowerbound 0)
  (define middle #f)
  
  (define (inner-loop)
    (if middle
      (solve (assert (<= num-msg middle)))
      (solve (assert #t)))
    (set! upperbound (evaluate num-msg))
    (set! middle (floor (/ (+ lowerbound upperbound) 2)))
    
    (set! best-sol (current-solution))
    
    ;; display
    (pretty-display (format "# messages = ~a" (evaluate num-msg)))
    (pretty-display (format "# cores = ~a" (evaluate num-cores)))
    
    (if (< lowerbound upperbound)
        (inner-loop)
        (result (evaluate num-msg) (cores-evaluate cores)))
  )
  
  ;void
  (define (outter-loop)
    (with-handlers* ([exn:fail? (lambda (e) 
                                  #|(when verbose
                                    (pretty-display "\n=== Solution ===")
                                    (send my-ast accept concise-printer) 
                                    (pretty-display best-sol)
                                    (display-cores cores))|#
                                  
                                  (set! lowerbound (add1 middle))
                                  (set! middle (floor (/ (+ lowerbound upperbound) 2)))
                                  (if (< lowerbound upperbound)
                                      (outter-loop)
                                      (result (evaluate num-msg) (cores-evaluate cores))))])
                    (inner-loop)))
  (let ([res (outter-loop)])
       (when verbose
         (pretty-display "\n=== Solution ===")
         (send my-ast accept concise-printer) 
         (pretty-display best-sol)
         (display-cores cores))
       res)
  )

#|
(define t (current-seconds))
(result-msgs 
 (optimize-comm "tests/function.cll" #:cores 16 #:capacity 256 #:verbose #t))

(pretty-display (format "partitioning time = ~a" (- (current-seconds) t)))|#
