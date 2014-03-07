#lang racket

(require "header.rkt" "arrayforth.rkt")

(provide (all-defined-out))

(define (print-generic x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))
  
   [(list? x)
    (pretty-display (format "~a(list " indent))
    (for ([i x])
	 (aforth-struct-print i (inc indent)))
    (pretty-display (format "~a)" indent))
    ]

   [(linklist? x)
    (unless (linklist-prev x)
	    (pretty-display (format "~a(list " indent)))
    (when (linklist-entry x)
	  (aforth-struct-print (linklist-entry x) (inc indent)))
    (if (linklist-next x)
	(aforth-struct-print (linklist-next x) indent)
	(pretty-display (format "~a)" indent)))
    ]
   
   [(block? x)
    (pretty-display (format "~a(block" indent))

    ;; BODY
    (display (format "~a\"" (inc indent)))
    (define body (if (list? (block-body x))
                      (string-join (block-body x))
                      (block-body x)))
    (pretty-display (format "~a\"" body))

    ;; ORG
    (display (format "~a\"" (inc indent)))
    (if (list? (block-org x))
        (display (string-join (block-org x)))
        (display (block-org x)))
    (pretty-display "\"")

    (display (format "~a(blockinfo '(" (inc indent)))
    ;; INFO
    ;; cnstr
    (define cnstr (block-cnstr x))
    (display (format "(data . ~a) " (block-out x)))  ;; out = data stack
    (display (format "(return . ~a) " (restrict-r cnstr))) ;; in = return stack
    (when (restrict-mem cnstr) (display "memory "))
    (when (restrict-a cnstr) (display "a "))
    (when (restrict-b cnstr) (display "b "))
    (display ") ")
    ;; assume
    (if (block-incnstr x)
        (let ([inst (first (string-split body))])
          (cond
           [(member inst (list "!b" "@b"))
            (display (format "'(b . ~a) " (block-incnstr x)))]
           [(member inst (list "!" "!+" "@" "@+"))
            (display (format "'(a . ~a) " (block-incnstr x)))]
           [(member inst (list "b!" "a!"))
            (display (format "'(t . ~a) " (block-incnstr x)))]
           [else 
            (raise (format "block with assume start with illegal instruction ~a" inst))]))
        (display "#f "))
    ;; recv (in)
    (pretty-display (format "~a))" (block-in x)))]
   
   [(mult? x)
    (pretty-display (format "~a(special \"mult\")" indent))]

   [(funccall? x)
    (pretty-display (format "~a(call \"~a\")"  indent (funccall-name x)))]

   [(forloop? x)
    (pretty-display (format "~a(forloop "  indent))
    (aforth-struct-print (forloop-init x) (inc indent))
    (aforth-struct-print (forloop-body x) (inc indent))
    (pretty-display (format "~a~a)" (inc indent) (- (forloop-to x) (forloop-from x))))]

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
    (pretty-display (format "~a(label \"~a\""  indent (funcdecl-name x)))
    (aforth-struct-print (funcdecl-body x) (inc indent))

    (define info (funcdecl-simple x))
    (pretty-display (format "~a(labelinfo ~a ~a ~a))"
                            (labelinfo-data info) 
                            (labelinfo-return info) 
                            (labelinfo-simple info)))

    ;; TODO: precondition
    ]

   



;; (struct blockinfo (cnstr assume recv))
;; (struct labelinfo (data return simple))