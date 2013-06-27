#lang s-exp rosette

(require "../nmap.rkt" "../layout-map.rkt" "../layout.rkt" "test-layout.rkt")

(define layout-map (parse-input "md5.in"))
(define core1 (val->core (id->val layout-map 0)))
(define core2 (val->core (id->val layout-map 1)))
; (size-of-core layout-map core1) returns 256.  It should return a
; formula.
; Debugged to this point:

(filter (lambda (x) (equal? (list-ref x 2) core1))
	(get-all-values layout-map)) ; Formula, as expected
(get-list-by-nth-val layout-map 2 core1) ; Not a formula!!

; The first piece of code (i.e. the filter) is just the body of
; get-list-by-nth-val (with the arguments substituted in), so the two
; expressions *should* return the same thing...
