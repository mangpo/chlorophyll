#lang racket

(require "header.rkt" "arrayforth.rkt")

(provide aforth-syntax-print aforth-syntax-info)

(define w #f)
(define h #f)
(define id #f)

(define (aforth-syntax-info my-w my-h [my-id #f])
  (set! w my-w)
  (set! h my-h)
  (set! id my-id))

(define (aforth-syntax-print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))

  (cond
   [(list? x)
    (for ([i x])
         (aforth-syntax-print i indent))
    ]
   
   [(block? x)
    (pretty-display "| br")
    (display indent)

    (if (list? (block-body x))
        (for ([i (block-body x)])
          (display i)
          (display " "))
        (begin
          (display (block-body x))
          (display " ")))]
   
   [(mult? x)
    (pretty-display "| br")
    (display indent)
    (pretty-display "17 for +* unext ")]
   
   [(funccall? x)
    (display (funccall-name x))
    (display " ")]
   
   [(forloop? x)
    (aforth-syntax-print (forloop-init x) indent)
    (pretty-display "| br")
    (display indent)
    (display "for ")
    (aforth-syntax-print (forloop-body x) (inc indent))
    (display "next ")]
   
   [(ift? x)
    (pretty-display "| br")
    (display indent)
    (display "if ")
    (aforth-syntax-print (ift-t x) (inc indent))
    (display "then ")]
   
   [(iftf? x)
    (pretty-display "| br")
    (display indent)
    (display "if ")
    (aforth-syntax-print (iftf-t x) (inc indent))
    (display "; ] then ")
    (aforth-syntax-print (iftf-f x) (inc indent))]
   
   [(-ift? x)
    (pretty-display "| br")
    (display indent)
    (display "-if ")
    (aforth-syntax-print (-ift-t x) (inc indent))
    (display "then ")]
   
   [(-iftf? x)
    (pretty-display "| br")
    (display indent)
    (display "-if ")
    (aforth-syntax-print (-iftf-t x) (inc indent))
    (display "; ] then ")
    (aforth-syntax-print (-iftf-f x) (inc indent))]
    
   [(funcdecl? x)
    (display (format ": ~a = $0 " (funcdecl-name x)))
    (aforth-syntax-print (funcdecl-body x) "  ")
    (pretty-display "; | cr")]

   [(aforth? x)
    (define memsize (aforth-memsize x))
    (pretty-display (format "{block ~a}" (+ 930 id)))
    (pretty-display (format "( -) # ~a ( mem ~a) 0 org | cr" 
                            (+ (* 100 (/ id w)) (modulo id w)) memsize))

    (for ([i (in-range memsize)])
         (display "0 , "))
    (pretty-display "| cr")
    
    (aforth-syntax-print (aforth-code x))
    (newline)
    ]

   [(vector? x)
    (define size (sub1 (vector-length x)))
    (for ([i (in-range size)])
         (set! id i)
	 (aforth-syntax-print (vector-ref x i)))
    ]
   
   [else (raise (format "arrayforth-syntax-print: unimplemented for ~a" x))]))