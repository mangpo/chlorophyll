#lang racket

(require "arrayforth.rkt")

(provide (all-defined-out))

(define (aforth-struct-print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))
  
    (cond
   [(list? x)
    (pretty-display (format "~a(list " indent))
    (for ([i x])
	 (aforth-struct-print i (inc indent)))
    (pretty-display (format "~a)" indent))
    ]
   
   [(block? x)
    (display (format "~a(block \"" indent))
    (if (list? (block-body x))
        (for ([i (block-body x)])
          (display i)
          (display " "))
        (display (block-body x)))
    (pretty-display (format "\" ~a ~a ~a)" (block-in x) (block-out x) (block-mem x)))]
   
   [(mult? x)
    (pretty-display (format "~a(mult)" indent))]
   
   [(funccall? x)
    (pretty-display (format "~a(funccall \"~a\")"  indent (funccall-name x)))]
   
   [(forloop? x)
    (pretty-display (format "~a(for "  indent))
    (aforth-struct-print (forloop-init x) (inc indent))
    (aforth-struct-print (forloop-body x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(ift? x)
    (pretty-display (format "~a(ift "  indent))
    (aforth-struct-print (ift-t x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(iftf? x)
    (pretty-display (format "~a(iftf "  indent))
    (aforth-struct-print (iftf-t x) (inc indent))
    (aforth-struct-print (iftf-f x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(-ift? x)
    (pretty-display (format "~a(-ift "  indent))
    (aforth-struct-print (-ift-t x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(-iftf? x)
    (pretty-display (format "~a(-iftf "  indent))
    (aforth-struct-print (-iftf-t x) (inc indent))
    (aforth-struct-print (-iftf-f x) (inc indent))
    (pretty-display (format "~a)" indent))]
    
   [(funcdecl? x)
    (pretty-display (format "~a(funcdecl \"~a\""  indent (funcdecl-name x)))
    (aforth-struct-print (funcdecl-body x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(aforth? x)
    (pretty-display (format "~a(aforth " indent))
    (aforth-struct-print (aforth-code x) (inc indent))
    (pretty-display (format "~a~a ~a)" indent (aforth-memsize x) (aforth-bit x)))]

   [(vector? x)
    (pretty-display "#lang racket")
    (pretty-display "(require \"../src/arrayforth.rkt\")")
    (pretty-display "(define programs")
    (pretty-display "  (vector")
    (define size (vector-length x))
    (for ([i (in-range size)])
	 (aforth-struct-print (vector-ref x i) (inc (inc indent))))
    (pretty-display "  ))")
    (pretty-display "(superoptimize-programs programs \"foo\")")
    ]
   
   [else (raise (format "arrayforth-print: unimplemented for ~a" x))]))
