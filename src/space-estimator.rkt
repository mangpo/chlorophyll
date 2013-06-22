#lang s-exp rosette

(provide (all-defined-out))

(define est-data 4)
(define est-num 5)
(define est-var 8) ; TODO: better estimation
(define est-comm 8) ; @p a! ! . up
(define est-acc-arr 2) ; a! @
(define est-for 16)
(define est-if 8) ;; call => 4 ;; :def if _ ; ] then _ ; => 4
(define est-while 12) ;; :while _ if _ while ; then ;
(define est-funccall 4)

(define space-map 
  #hash(("~" . 1)
	("!" . 1)
        ("*" . 7) ; @p 17 for +* unext
        ("/" . 52) ; estimate from chuck's blog
        ("%" . 52) ; estimate from chuck's blog
        ("+" . 2)  ; . +
        ("-" . 10) ; - @p . + 1 . +
        (">>" . 1)
        ("<<" . 1)
        ("<" . 11) ; a < b --> a - b < 0       - @p . + num . + -if
        ("<=" . 9) ; a <= b --> a - b - 1 < 0  - @p . + num -if
        (">=" . 10)
        (">" . 12)
        ("==" . 11) ; a == b   - @p . + num . + if
        ("!=" . 11)
        ("&" . 1)
        ("^" . 1)
        ("|" . 5) ; over - and . +
        ("&&" . 2) ; if(c1 && c2) { a } --> c1 if c2 if a then then
        ("||" . 16) ; if(c1 || c2) { a } --> c1 if call ;
))

(define (est-space x)
  (dict-ref space-map x))

