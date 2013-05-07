#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
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

#|(define (optimize-comm-backup file 
                        #:cores [best-num-cores 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [best-num-msg #f]
                        #:verbose [verbose #f])
  
  #|(let ([bitwidth (+ (inexact->exact (ceiling 
                    (/ (log (max best-num-cores capacity best-num-msg)) (log 2)))) 10)])
      
    ;; Set bidwidth for rosette
    (pretty-display (format "bidwidth = ~a" bitwidth))
    (configure [bitwidth bitwidth]))|#
  (configure [bitwidth 32])
  
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
  (define interpreter (new count-msg-interpreter% [core-space capacity] [num-core best-num-cores]))
  (define best-sol #f)
  
  (define num-msg (comminfo-msgs (send my-ast accept interpreter)))
  (define num-cores (send interpreter num-cores))
  
  (define (loop)
    ;(solve (assert (< num-cores best-num-cores)))
    ;(set! best-num-cores (evaluate num-cores))
    (if best-num-msg
      (solve (assert (< num-msg best-num-msg)))
      (solve (assert #t)))
    (set! best-num-msg (evaluate num-msg))
    
    (set! best-sol (current-solution))
    
    ;; display
    ;(send my-ast accept concise-printer)
    ;(send interpreter display-used-space)
    (pretty-display (format "# messages = ~a" (evaluate num-msg)))
    (pretty-display (format "# cores = ~a" (evaluate num-cores)))
    (loop)
  )
  
  ;void
  (with-handlers* ([exn:fail? (lambda (e) 
                                (when verbose
                                  (pretty-display "\n=== Solution ===")
                                  (send my-ast accept concise-printer) 
                                  (pretty-display best-sol)
                                  (send interpreter display-used-space))
				(result (evaluate num-msg) (send interpreter get-concrete-cores)))])
                  (loop))
  )|#

(define (optimize-comm file 
                        #:cores [num-cores 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [best-num-msg #f]
                        #:verbose [verbose #f])
  
  #|(let ([bitwidth (+ (inexact->exact (ceiling 
                    (/ (log (max num-cores capacity best-num-msg)) (log 2)))) 10)])
      
    ;; Set bidwidth for rosette
    (pretty-display (format "bidwidth = ~a" bitwidth))
    (configure [bitwidth bitwidth]))|#
  (configure [bitwidth 32])
  
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
  
  ;; Create abstract interpreter
  (define cores (make-cores #:capacity capacity #:max-cores num-cores))
  (define interpreter (new count-msg-interpreter% 
                           [places cores]))
  (define evaluator (new symbolic-evaluator%))
  (define best-sol #f)
  (define partial-hash (make-hash))
  (define partial-sol (sat (hash)))
  
  ;(define (assert-partial-solution sol)
    ;; Assert all symbolic variables that are already in the solution to the current value.
    ;; TODO
    ;)
  
  (define (solve-function func-ast [min-num-msg #f])
    ;; Count number of messages
    (define num-msg-before (comminfo-msgs (send func-ast accept interpreter)))
    (define num-msg 
      (if partial-sol
          (evaluate num-msg-before partial-sol)
          num-msg-before))
    (define num-cores (evaluate (cores-count cores) partial-sol))
    
    (pretty-display (format "partial-sol: ~a" partial-sol))
    (pretty-display (format "before: ~a" num-msg-before))
    (pretty-display (format "after: ~a" num-msg))
  
    (define (loop)
      (if min-num-msg
          (solve (assert (< num-msg min-num-msg)))
          (solve (assert #t)))
      (set! min-num-msg (evaluate num-msg))
      
      (set! best-sol (current-solution))
      
      ;; display
      (pretty-display (format "# messages = ~a" (evaluate num-msg)))
      (pretty-display (format "# cores = ~a" (evaluate num-cores)))
      (loop)
      )
    
    (define (add-to-partial)
      ;; Add to hash map
      (for ([mapping (solution->list best-sol)])
        (when (not (hash-has-key? partial-hash (car mapping)))
          (hash-set! partial-hash (car mapping) (cdr mapping))))
      
      ;; Create partial solution
      (set! partial-sol (sat (make-immutable-hash (hash->list partial-hash))))
      )
  
    (with-handlers* ([exn:fail? (lambda (e) 
                                  (when verbose
                                    (pretty-display "\n=== Solution ===")
                                    (add-to-partial)
                                    (cores-evaluate cores)
                                    ;(send my-ast accept evaluator)
                                    (send func-ast accept concise-printer)
                                    (send my-ast pretty-print)
                                    (pretty-display partial-sol)
                                    ;(assert-partial-sol best-sol)
                                    ;(send interpreter display-used-space))
                                    ;(result (evaluate num-msg) (send interpreter get-concrete-cores)
                                    ))])
                    (loop)))
  
  (for ([decl (get-field decls my-ast)])
    (when verbose (pretty-display "------------------------------------------------"))
    (if (is-a? decl FuncDecl%)
      (solve-function decl)
      (send decl accept interpreter)))
  
  (when verbose
    (pretty-display "\n=== Final Solution ===")
    ;(send my-ast accept concise-printer)
    (pretty-display partial-sol)
    ;(send interpreter display-used-space)
    )
  )

(optimize-comm "examples/function.cll" #:cores 16 #:capacity 256 #:verbose #t)
