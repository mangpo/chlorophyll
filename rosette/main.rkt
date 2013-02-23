#lang s-exp rosette

(require "ast.rkt" "parser.rkt" "visitor-interpreter.rkt" "visitor-variables.rkt")

(configure [bitwidth 8])

;(define test "known int@4 x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@10 -1@10 *@10 2@10;")
;(define my-ast (ast-from-string test))

(define (foo)
  (define my-ast (ast-from-file "examples/program.mylang"))
  (define num-msg (send my-ast accept (new count-msg-interpreter%)))
  (pretty-display (format "# messages = ~a" num-msg))
  (assert (= num-msg 5)))

(define-values (out asserts) (with-asserts (foo)))

asserts

(send (current-solver) clear)
(send/apply (current-solver) assert asserts)
(send (current-solver) debug)

;(send my-ast pretty-print)
;(define interpreter (new count-msg-interpreter%))
;(define num-msg (send my-ast accept interpreter))
;(pretty-display (format "# messages = ~a" num-msg))
;(newline)
;(send my-ast pretty-print)

;(send interpreter display-used-space)

;(define (validity-of num-msg limit)
;  (assert (<= num-msg limit)))

#|(let ([collector (new var-collector%)])
  (synthesize #:forall (set->list (send my-ast accept collector))
              #:assume #t
              #:guarantee (validity-of num-msg 100))
  (pretty-print (map (current-solution) neg?))
  )|#

;(let ([collector (new var-collector%)])
;  (pretty-print (send my-ast accept collector)))

;(solve (validity-of num-msg 100))

;; failed
;(solve (assert (= num-msg 5)))

;; sym-place = 2 when fully-specify?
;(solve (assert (= num-msg 3)))
;(current-solution)
