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

(define (optimize-comm file 
                        #:cores [num-cores 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [best-num-msg #f]
                        #:verbose [verbose #f])
  (if best-num-msg
      (let ([bitwidth (+ (inexact->exact (ceiling 
                                          (/ (log (max num-cores capacity best-num-msg)) (log 2)))) 5)])
      
        ;; Set bidwidth for rosette
        (pretty-display (format "bidwidth = ~a" bitwidth))
        (configure [bitwidth bitwidth]))
      (configure [bitwidth 32]))
  
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
  ;(define evaluator (new symbolic-evaluator%))
  
  ;; Place holder for solution
  (define best-sol #f)
  (define partial-hash (make-hash))
  (define total-msg 0)
  
  (define (solve-function func-ast [min-num-msg #f])
    ;; Count number of messages
    (define num-msg (evaluate (comminfo-msgs (send func-ast accept interpreter)) global-sol))
    (define num-cores (evaluate (cores-count cores) global-sol))
  
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
      (set-global-sol (sat (make-immutable-hash (hash->list partial-hash))))
      )
  
    (with-handlers* ([exn:fail? (lambda (e) 
                                  (add-to-partial)
                                  (set! total-msg (+ total-msg (evaluate num-msg)))
                                  (when verbose
                                    (pretty-display "\n=== Solution ===")
                                    (pretty-display (format "num-msg = ~a" (evaluate num-msg)))
                                    (send func-ast accept concise-printer)
                                    (pretty-display global-sol)
                                    ))])
                    (loop)))
  
  (for ([decl (get-field decls my-ast)])
    (if (is-a? decl FuncDecl%)
        (begin
          (solve-function decl)
          (when verbose (pretty-display "------------------------------------------------")))
        (send decl accept interpreter)))
  
  (when verbose
    (pretty-display "\n=== Final Solution ===")
    (send my-ast accept concise-printer)
    (pretty-display global-sol)
    (display-cores cores)
    )
  
  (pretty-display (format "total-msg = ~a" total-msg))
  (result total-msg (cores-evaluate cores))
  )

(result-msgs
 (optimize-comm "examples/function.cll" #:cores 16 #:capacity 256 #:verbose #t #:max-msgs 8))
