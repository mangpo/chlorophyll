#lang s-exp rosette

(require "ast.rkt" "parser.rkt" "visitor-interpreter.rkt" "visitor-variables.rkt")

(configure [bitwidth 10])

;; Concrete version
(define (concrete)
  (define my-ast (ast-from-file "examples/concrete.mylang"))
  (define interpreter (new count-msg-interpreter%))
  (define num-msg (send my-ast accept interpreter))
  (send my-ast pretty-print)
  (pretty-display (format "# messages = ~a" num-msg))
  (send interpreter display-used-space)
  )

;(concrete)

;; current-solution doesn't like me :(
(define (synthesize)
  (define my-ast (ast-from-file "examples/symbolic.mylang"))
  (define interpreter (new count-msg-interpreter%))
  (define num-msg (send my-ast accept interpreter))
  (send my-ast pretty-print)
  (pretty-display (format "# messages = ~a" num-msg))
  ;(send interpreter display-used-space)
  (solve (assert (= num-msg 3)))
  (current-solution)
  )

(synthesize)

;; this part verify that solve should be able to find a solution.
(define (foo)
  (define my-ast (ast-from-file "examples/symbolic.mylang"))
  (define interpreter (new count-msg-interpreter%))
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

;(define (validity-of num-msg limit)
;  (assert (<= num-msg limit)))

#|(let ([collector (new var-collector%)])
  (synthesize #:forall (set->list (send my-ast accept collector))
              #:assume #t
              #:guarantee (validity-of num-msg 100))
  (pretty-print (map (current-solution) neg?))
  )|#
