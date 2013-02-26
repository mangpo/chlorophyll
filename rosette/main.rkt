#lang s-exp rosette

(require "ast.rkt" "parser.rkt" "visitor-interpreter.rkt" "visitor-collector.rkt" "visitor-rename.rkt")

(configure [bitwidth 10])

;; Concrete version
(define (concrete)
  (define my-ast (ast-from-file "examples/test.lego"))
  (define collector (new place-collector% 
                         [collect? (lambda(x) (and (number? x) (not (symbolic? x))))]))
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

;; current-solution doesn't like me :(
(define (simple-syn)
  (define my-ast (ast-from-file "examples/symbolic.mylang"))
  (define interpreter (new count-msg-interpreter% [core-space 256] [num-core 3]))
  (define num-msg (send my-ast accept interpreter))
  (send my-ast pretty-print)
  (pretty-display (format "# messages = ~a" num-msg))
  ;(send interpreter display-used-space)
  (solve (assert (= num-msg 3)))
  (current-solution)
  )

;(simple-syn)

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
  (define my-ast (ast-from-file "examples/simple-hole.lego"))
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

(define (optimize-space)
  (define my-ast (ast-from-file "examples/baby-md5.lego"))
  (send my-ast pretty-print)
  
  ;; collect real pysical places
  (define collector (new place-collector% 
                         [collect? (lambda(x) (and (number? x) (not (symbolic? x))))]))
  (define place-set (send my-ast accept collector))
  (pretty-print place-set)
  
  ;; convert abstract partition (string except ??) into number
  (define converter (new partition-to-number% [num-core 16] [real-place-set place-set]))
  (send my-ast accept converter)
  (send my-ast pretty-print)
  
  #|(let* ([collector (new place-collector% [collect? number?])]
         [place-set (send my-ast accept collector)]
         [converter (new partition-to-number% [num-core 16] [real-place-set place-set])])
    (send my-ast accept convertor))|#
    
  
  (define interpreter (new count-msg-interpreter% [core-space 256] [num-core 16]))
  (define best-num-msg 256)
  (define best-num-cores 144)
  (define best-sol #f)
  
  (define num-msg (send my-ast accept interpreter))
  (define num-cores (send interpreter num-cores))
  (send my-ast pretty-print)
  
  (define (loop)
    ;(solve (assert (< num-cores best-num-cores)))
    ;(set! best-num-cores (evaluate num-cores))
    (solve (assert (< num-msg best-num-msg)))
    (set! best-num-msg (evaluate num-msg))
    
    (set! best-sol (current-solution))
    
    ;; display
    ;(pretty-print best-sol)
    (send my-ast pretty-print)
    (send interpreter display-used-space)
    (pretty-print (format "# messages = ~a" (evaluate num-msg)))
    (pretty-print (format "# cores = ~a" (evaluate num-cores)))
    (loop)
  )
  
  (loop)
  )

(optimize-space)
