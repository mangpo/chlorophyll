#lang racket

(require "header.rkt" "arrayforth.rkt")

(provide aforth-syntax-print string-converter%)

(define w #f)
(define h #f)
(define id 0)

;; true -> orginal arrayforth format
;; false -> rohin's interpreter
(define original #t)

(define (aforth-syntax-print code my-w my-h #:id [my-id 0] #:format [format #t])
  (set! w my-w)
  (set! h my-h)
  (set! id my-id)
  (set! original format)
  (print code))

(define (print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))

  (cond
   [(list? x)
    (for ([i x])
         (print i indent))
    ]
   
   [(block? x)
    (when original (display "| cr"))
    (newline)
    (display indent)

    (define inst-list
      (if (list? (block-body x))
          (block-body x)
          (string-split (block-body x))))
    (set! inst-list (filter (lambda (a) (and (not (equal? "nop" a)) (not (equal? "." a)))) 
                            inst-list))

    (for ([i inst-list])
         (when (equal? "+" i)
               (display ".") (display " "))
         (display i) (display " "))
    ]
   
   [(mult? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (pretty-display "17 for +* unext ")]
   
   [(funccall? x)
    (display (funccall-name x))
    (display " ")]
   
   [(forloop? x)
    (print (forloop-init x) indent)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display "for ")
    (print (forloop-body x) (inc indent))
    (display "next ")]
   
   [(ift? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display "if ")
    (print (ift-t x) (inc indent))
    (display "then ")]
   
   [(iftf? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display "if ")
    (print (iftf-t x) (inc indent))
    (display "; ] then ")
    (print (iftf-f x) (inc indent))]
   
   [(-ift? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display "-if ")
    (print (-ift-t x) (inc indent))
    (display "then ")]
   
   [(-iftf? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display "-if ")
    (print (-iftf-t x) (inc indent))
    (display "; ] then ")
    (print (-iftf-f x) (inc indent))]
    
   [(funcdecl? x)
    (display (format ": ~a " (funcdecl-name x)))
    (when original (display "= $0 "))

    (print (funcdecl-body x) "  ")

    (when original (display "= $0 "))
    (display "; ")
    (when original (display "| cr"))
    (newline)
    ]
   
   [(vardecl? x)
    (when original
          (for ([val (vardecl-val x)])
               (display val)
               (display " , "))
          (pretty-display "| br"))
    ]

   [(aforth? x)
    (define memsize (aforth-memsize x))
    (define node (+ (* 100 (floor (/ id w))) (modulo id w)))

    (if original
        (begin
          (pretty-display (format "{block ~a}" (+ 930 (* 2 id))))
          (pretty-display (format "( -) # ~a ( mem ~a) 0 org | cr" node memsize)))
        (begin
          (pretty-display (format "yellow ~a node" node))
          (pretty-display (format "~a org green" memsize))))
    
    (print (aforth-code x))

    (unless original
            (pretty-display (format ".. start main .ns 0 ~a .mem" memsize)))

    (newline)
    ]

   [(vector? x)
    (define size (sub1 (vector-length x)))
    (for ([i (in-range size)])
         (set! id i)
	 (print (vector-ref x i)))
    ]

   [(equal? x #f) void]
   
   [else (raise (format "arrayforth-syntax-print: unimplemented for ~a" x))]))


(define string-converter%
  (class object%
    (super-new)

    (define/public (visit ast)
      (cond
       [(linklist? ast)
        (define str "")
        (when (linklist-entry ast)
          (set! str (send this visit (linklist-entry ast))))
	(when (linklist-next ast)
	      (set! str (string-append str " " (send this visit (linklist-next ast)))))
	(string-trim str)]

       [(block? ast)
	(define body (block-body ast))
	(if (string? body)
	    body
	    (string-join body))]

       [(forloop? ast)
	(string-append (send this visit (forloop-init ast))
		       " for "
		       (send this visit (forloop-body ast))
		       " next")]

       [(ift? ast)
	(string-append "if " (send this visit (ift-t ast)))]

       [(iftf? ast)
	(string-append "if " (send this visit (iftf-t ast))
		       " then " (send this visit (iftf-f ast)))]

       [(-ift? ast)
	(string-append "-if " (send this visit (-ift-t ast)))]

       [(-iftf? ast)
	(string-append "-if " (send this visit (-iftf-t ast))
		       " then " (send this visit (-iftf-f ast)))]

       [(mult? ast)
	"mult"]

       [(funccall? ast)
	(funccall-name ast)]

       [else #f]))))