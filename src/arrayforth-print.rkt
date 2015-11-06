#lang racket

(require "header.rkt" "arrayforth.rkt")

(provide aforth-syntax-print string-converter%)

(define w #f)
(define h #f)
(define id 0)

;; true -> orginal arrayforth format
;; false -> new interpreter
(define original #t)

(define (aforth-syntax-print code my-w my-h #:id [my-id 0] #:original-format [format #t])
  (set! w my-w)
  (set! h my-h)
  (set! id my-id)
  (set! original format)
  (print code))

(define (print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))

  (define (micro-next loopbody)
    ;(pretty-display `(micro-next ,loopbody))

    (define (list-length a)
      (define (linklist-length l)
	;(pretty-display `(linklist-length ,l))
	(if l
	    (if (linklist-entry l)
		(add1 (linklist-length (linklist-next l)))
		(linklist-length (linklist-next l)))
	    0))
      (if (list? a) (length a) (linklist-length a)))
      
    (or (= (list-length loopbody) 0)
        (and (= (list-length loopbody) 1)
                                        ; head linklist is empty
             (let ([entry (if (list? loopbody)
                              (car loopbody)
                              (linklist-entry (linklist-next loopbody)))])
               (and (block? entry)
                    (let* ([program (block-body entry)]
                           [inst-list (if (string? program) (string-split program) program)]
                           [total (length inst-list)]
                           [count-number
                            (count (lambda (x)
                                     (or (string->number x)
                                         (member x (list "up" "down" "left" "right" "io"))))
                                   inst-list)])
                      (< (+ total (* 4 count-number)) 4)))))))

  (cond
   [(list? x)
    (for ([i x])
         (print i indent))
    ]

   [(linklist? x)
    (when (linklist-entry x)
          (print (linklist-entry x) indent))
    (when (linklist-next x)
          (print (linklist-next x) indent))]
    
   
   [(block? x)
    ;; (when original (display "| cr"))
    ;; (newline)
    ;; (display indent)

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
    (display "a! dup dup or 17 for +* unext drop drop a ")]
   
   [(funccall? x)
    (define name (funccall-name x))
    (when (or original (not (member name (list "in" "out"))))
          (display (funccall-name x))
          (display " "))
    ]
   
   [(forloop? x)
    (when original (display "| cr"))
    (newline)
    (display indent)

    (print (forloop-init x) indent)
    (display "for ")
    (when original (display "| cr"))
    (newline)
    (display (inc indent))
    ;(pretty-display `(forloop-body ,(forloop-body x)))
    ;(aforth-struct-print x)
    (print (forloop-body x) (inc indent))
    ;(pretty-display `(forloop-body ,(forloop-body x)))
    (if (micro-next (forloop-body x))
	(display "unext ")
	(display "next "))]
   
   [(ift? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display ".. if ")
    (print (ift-t x) (inc indent))
    (display "then ")]
   
   [(iftf? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display ".. if ")
    (print (iftf-t x) (inc indent))
    (if original
        (display "; ] *then ")
        (display "; then "))
    (print (iftf-f x) (inc indent))]
   
   [(-ift? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display ".. -if ")
    (print (-ift-t x) (inc indent))
    (display "then ")]
   
   [(-iftf? x)
    (when original (display "| cr"))
    (newline)
    (display indent)
    (display ".. -if ")
    (print (-iftf-t x) (inc indent))
    (if original
        (display "; ] then ")
        (display "; then "))
    (print (-iftf-f x) (inc indent))]
    
   [(funcdecl? x)
    (display (format ": ~a " (funcdecl-name x)))
    (when original (display "= $0 "))

    (print (funcdecl-body x) "  ")

    (when original 
          (when (equal? (funcdecl-name x) "main")
	  (display "warm "))
          (display "= $0 "))
    (display "; ")
    (when original (display "| cr"))
    (newline)
    ]
   
   [(vardecl? x)
    (if original
        (begin
          (for ([val (vardecl-val x)])
            (display val)
            (display " , "))
          (pretty-display "| br"))
        (begin
          (for ([val (vardecl-val x)])
            (display " , ")
            (pretty-display val))))
    ]

   [(aforth? x)
    (define memsize (aforth-memsize x))
    (define node (core-id id w))

    (if original
        (begin
          (pretty-display (format "{block ~a}" (+ block-offset (* 2 id))))
          (pretty-display (format "( -) # ~a ( id ~a mem ~a) 0 org | cr" node id memsize)))
        (begin
          (pretty-display (format "node ~a" (core-id id w)))
          (when (and (aforth-a x)
                     (not (void? (aforth-a x))))
            (printf "/a ~a\n" (aforth-a x)))
          (pretty-display (format "org 0"))))
    (print (aforth-code x))

    ;; (unless original
    ;;         (pretty-display (format ".. start main .ns 0 ~a .mem" memsize)))

    (newline)
    ]

   [(vector? x)
    ;(define size (sub1 (vector-length x)))
    (when original
      (pretty-display "{block 790}")
      (pretty-display "host target | cr")
      (for ([id (* w h)])
        (when (vector-ref x id)
          (pretty-display (format "~a node ~a load"
                                  (core-id id w) (+ block-offset (* 2 id))))))

      (newline)
      (pretty-display "{block 792}")
      (pretty-display ": /node dup +node /ram ; | cr")
      (for ([id (* w h)])
        (when (vector-ref x id)
          (pretty-display (format "~a /node $0 /p" (core-id id w)))))
      (newline))
                               
    (for ([i (* w h)])
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
