#lang s-exp rosette

(require "ast.rkt" "parser.rkt" "visitor-interpreter.rkt" "visitor-variables.rkt")

(configure [bitwidth 8])

;(define test "known int@4 x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@10 -1@10 *@10 2@10;")
;(define my-ast (ast-from-string test))

(define my-ast (ast-from-file "examples/2.mylang"))

;(send my-ast pretty-print)
(define interpreter (new count-msg-interpreter%))
(define num-msg (send my-ast accept interpreter))
(pretty-display (format "# messages = ~a" num-msg))
;(newline)
;(send my-ast pretty-print)

;(send interpreter display-used-space)

(define (validity-of num-msg limit)
  (assert (<= num-msg limit)))

#|(let ([collector (new var-collector%)])
  (synthesize #:forall (set->list (send my-ast accept collector))
              #:assume #t
              #:guarantee (validity-of num-msg 100))
  (pretty-print (map (current-solution) neg?))
  )|#

;(solve (validity-of num-msg 100))

;; failed
(solve (assert (= num-msg 5)))

;; sym-place = 2 when fully-specify?
;(solve (assert (= num-msg 3)))
(current-solution)
