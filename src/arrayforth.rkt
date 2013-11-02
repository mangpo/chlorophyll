#lang racket

(require "header.rkt")

(provide (all-defined-out))

(struct block (body in out cnstr incnstr org) #:mutable)
(struct mult ()) ;; : mult (x y -> z) a! 0 17 for +* next drop drop a ;
(struct funccall (name))
(struct funcdecl (name body) #:mutable)
(struct vardecl (val))
(struct forloop (init body iter from to) #:mutable)
(struct ift (t))
(struct iftf (t f))
(struct -ift (t))
(struct -iftf (t f))
(struct aforth (code memsize bit indexmap))
(struct restrict (mem a b r) #:mutable)

(struct linklist (prev entry next) #:mutable)

(define ga-bit 1)
(define block-limit 16)
(define reduce-limit 5)

(define-syntax new-block
  (syntax-rules ()
    [(new-block body in out cnstr org)
     (block body in out cnstr #f org)]
    [(new-block body in out cnstr incnstr org)
     (block body in out cnstr incnstr org)]))

(define (core-id id w)
  (+ (* 100 (floor (/ id w))) (modulo id w) node-offset))

(define (inout inst)
  (cond
   [(or (member inst (list "@+" "@b" "@" "pop" "a" "up" "down" "left" "right"))
        (string->number inst))
    (cons 0 1)]

   [(member inst (list "!+" "!b" "!" "drop" "push" "b!" "a!"))
    (cons 1 0)]

   [(member inst (list "+*"))
    (cons 2 2)]

   [(member inst (list "2*" "2/" "-"))
    (cons 1 1)]

   [(member inst (list "+" "and" "or"))
    (cons 2 1)]

   [(member inst (list "dup"))
    (cons 1 2)]

   [(member inst (list "over"))
    (cons 2 3)]

   [else
    (raise (format "arrayforth-inout: unimplemented for ~a" inst))]))

(define (combine-inout a-pair b-pair)
  (define a-in  (car a-pair))
  (define a-out (cdr a-pair))
  (define b-in  (car b-pair))
  (define b-out (cdr b-pair))
  (if (>= a-out b-in)
      (cons a-in 
            (- (+ a-out b-out) b-in))
      (cons (- (+ a-in b-in) a-out)
            b-out)))

(define (estimate-inout insts)
  (when (string? insts)
        (set! insts (string-split insts)))
  (foldl (lambda (inst res)
           (combine-inout res (inout inst)))
         (cons 0 0) insts))

(define (estimate-a insts)
  (when (string? insts)
        (set! insts (string-split insts)))
  (foldl (lambda (inst res)
           (cond 
	    [(equal? inst "a!") #t]
	    [(member inst (list "@" "@+" "!" "!+" "a")) #f]
	    [else res]))
         #f insts))

(define (estimate-b insts)
  (when (string? insts)
        (set! insts (string-split insts)))
  (foldl (lambda (inst res)
           (cond 
	    [(equal? inst "b!") #t]
	    [(member inst (list "@b" "!b")) #f]
	    [else res]))
         #f insts))

(define (estimate-r insts)
  (when (string? insts)
        (set! insts (string-split insts)))
  (foldl (lambda (inst res)
           (cond 
	    [(equal? inst "push") #t]
	    [(equal? inst "pop") #f]
	    [else res]))
         #f insts))

(define-syntax prog-append
  (syntax-rules ()
    [(prog-append a b) (program-append a b)]
    [(prog-append a b c ...)
     (prog-append (program-append a b) c ...)]))

;; merge b-block into a-block
(define (merge-block a-block b-block)
  ;; (pretty-display "MERGE:")
  ;; (codegen-print a-block)
  ;; (codegen-print b-block)
  (define a-body (block-body a-block))
  (define b-body (block-body b-block))
  (define a-list (if (list? a-body) a-body (string-split a-body)))
  (define b-list (if (list? b-body) b-body (string-split b-body)))

  (define a-body-org (block-org a-block))
  (define b-body-org (block-org b-block))
  (define a-list-org (if (list? a-body-org) a-body-org (string-split a-body-org)))
  (define b-list-org (if (list? b-body-org) b-body-org (string-split b-body-org)))

  (set-block-body! a-block (append a-list b-list))
  (set-block-org! a-block (append a-list-org b-list-org))
  
  (define a-cnstr (block-cnstr a-block))
  (define b-cnstr (block-cnstr b-block))
  (set-block-cnstr! a-block (restrict (and (restrict-mem a-cnstr) (restrict-mem b-cnstr))
                                      (or (restrict-a a-cnstr) (restrict-a b-cnstr))
                                      (restrict-b b-cnstr)
                                      (or (restrict-r a-cnstr) (restrict-r b-cnstr))
                                      ))

  (when (empty? a-list)
        (set-block-incnstr! a-block (block-incnstr b-block)))
  
  (define a-in  (block-in a-block))
  (define a-out (block-out a-block))
  (define b-in  (block-in  b-block))
  (define b-out (block-out  b-block))
  (if (>= a-out b-in)
      (set-block-out! a-block (- (+ a-out b-out) b-in))
      (begin
        (set-block-in! a-block (- (+ a-in b-in) a-out))
        (set-block-out! a-block b-out)))
  a-block)


(define (empty-block? x)
  (and (block? x) 
       (or (equal? (block-body x) "") (empty? (block-body x)))))

(define (program-append a-list b-list [no-limit #f])

  (define (merge-forloop a-for b-for)
    ;; (pretty-display `(merge-forloop ,(forloop-body a-for) ,(forloop-body b-for)))
    (and (equal? (forloop-iter a-for) (forloop-iter b-for))
         (equal? (forloop-to a-for) (forloop-from b-for))
         (aforth-eq? (forloop-body a-for) (forloop-body b-for))
         (let* ([addr (car (forloop-iter a-for))]
		[addr-org (cdr (forloop-iter a-for))]
		[from (forloop-from a-for)]
		[to   (forloop-to b-for)]
		[bound-str (number->string (- to from 1))])
           (set-forloop-to! a-for to)
           (set-forloop-init! a-for
			      (cond
			       [(equal? addr #f)
				(new-block (list bound-str) 0 1 (restrict #t #t #f #t) (list bound-str))]

			       [(pair? addr)
				(new-block (list (car addr) "a!" bound-str)
				       0 1 (restrict #t #t #f #t)
				       (list (car addr-org) "a!" bound-str))]

			       [else
				(new-block (list (number->string from) addr "b!" "!b" bound-str)
				       0 1 (restrict #t #t #f #t)
				       (list (number->string from) addr-org "b!" "!b" bound-str))]))
           #t)))
  
  (cond
   [(empty? a-list) b-list]
   [(empty? b-list) a-list]
   [(and (= (length a-list) 1) (empty-block? (car a-list)))
    b-list]
   [(and (= (length b-list) 1) (empty-block? (car b-list)))
    a-list]

   [(and (forloop? (last a-list)) (forloop? (car b-list)))
    (define merge (merge-forloop (last a-list) (car b-list)))
    (if merge
        (append a-list (cdr b-list))
        (append a-list b-list))]

   [else
    (define a-last (last a-list))
    (define b-first (car b-list))

    ;; remove "pop a! a push"
    (when (and (block? a-last) (block? b-first))
	  (define a-code (block-body a-last))
	  (define b-code (block-body b-first))
	  (when (and (>= (length a-code) 2) (equal? (take-right a-code 2) (list "pop" "a!"))
		     (>= (length b-code) 2) (equal? (take b-code 2) (list "a" "push")))
		(set-block-body! a-last (take a-code (- (length a-code) 2)))
		(set-block-body! b-first (take-right b-code (- (length b-code) 2)))))

    ;; (if (and (block? a-last) (block? b-first)
    ;;          (or no-limit
    ;;              (<= (+ (length (block-body a-last)) (length (block-body b-first))) 
    ;;                  block-limit)))
    ;;     (begin
    ;;       (merge-block a-last b-first)
    ;;       (append a-list (cdr b-list)))
    ;;     (append a-list b-list))]))

    (append a-list b-list)]))

(define (aforth-eq? a b)
  ;; (pretty-display `(aforth-eq? ,a ,b ,(block? a) ,(block? b)))
  (or
   (and (list? a) (list? b) (= (length a) (length b)) (andmap aforth-eq? a b))
   (and (linklist? a) (linklist? b)
        (aforth-eq? (linklist-entry a) (linklist-entry b))
        (aforth-eq? (linklist-next a) (linklist-next b)))
   (and (block? a) (block? b) (equal? (block-body a) (block-body b)))
   (and (mult? a) (mult? b))
   (and (funccall? a) (funccall? b) (equal? (funccall-name a) (funccall-name b)))
   (and (forloop? a) (forloop? b) (equal? (forloop-iter a) (forloop-iter b))
        (aforth-eq? (forloop-body a) (forloop-body b)))
   (and (ift? a) (ift? b) (aforth-eq? (ift-t a) (ift-t b)))
   (and (iftf? a) (iftf? b) 
        (aforth-eq? (iftf-t a) (iftf-t b)) (aforth-eq? (iftf-f a) (iftf-f b)))
   (and (-ift? a) (-ift? b) (aforth-eq? (-ift-t a) (-ift-t b)))
   (and (-iftf? a) (-iftf? b) 
        (aforth-eq? (-iftf-t a) (-iftf-t b)) (aforth-eq? (-iftf-f a) (-iftf-f b)))
   (equal? a b)))

;; casual printting
(define (codegen-print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))

  (cond
   [(equal? x #f) 
    (pretty-display (format "~a#f" indent))]

   [(list? x)
    (for ([i x])
	 (codegen-print i indent))]
   
   [(linklist? x)
    (when (linklist-entry x)
          (codegen-print (linklist-entry x) indent))
    (when (linklist-next x)
          (codegen-print (linklist-next x) indent))]

   [(block? x)
    (pretty-display (format "~a(block" indent))
    (display (format "~acode:" (inc indent)))
    (if (list? (block-body x))
        (for ([i (block-body x)])
          (display i)
          (display " "))
        (display (block-body x)))
    (newline)
    (display (format "~aorg:" (inc indent)))
    (if (list? (block-org x))
        (for ([i (block-org x)])
          (display i)
          (display " "))
        (display (block-org x)))
    (define cnstr (block-cnstr x))
    (pretty-display (format "~ain:~a out:~a mem:~a a:~a b:~a r:~a)" (inc indent) 
                            (block-in x) (block-out x) 
			    (restrict-mem cnstr) 
                            (restrict-a cnstr) (restrict-b cnstr)
                            (restrict-r cnstr)
                            ))]
   
   [(mult? x)
    (pretty-display (format "~a(mult)" indent))]
   
   [(funccall? x)
    (pretty-display (format "~a(funccall: ~a)"  indent (funccall-name x)))]
   
   [(forloop? x)
    (pretty-display (format "~a(for:"  indent))
    (codegen-print (forloop-init x) (inc indent))
    (codegen-print (forloop-body x) (inc indent))]
   
   [(ift? x)
    (pretty-display (format "~a(if:"  indent))
    (pretty-display (format "~a>> true"  indent))
    (codegen-print (ift-t x) (inc indent))]
   
   [(iftf? x)
    (pretty-display (format "~a(if:"  indent))
    (pretty-display (format "~a>> true"  indent))
    (codegen-print (iftf-t x) (inc indent))
    (pretty-display (format "~a>> false"  indent))
    (codegen-print (iftf-f x) (inc indent))]
   
   [(-ift? x)
    (pretty-display (format "~a(-if:"  indent))
    (pretty-display (format "~a>> true"  indent))
    (codegen-print (-ift-t x) (inc indent))]
   
   [(-iftf? x)
    (pretty-display (format "~a(-if:"  indent))
    (pretty-display (format "~a>> true"  indent))
    (codegen-print (-iftf-t x) (inc indent))
    (pretty-display (format "~a>> false"  indent))
    (codegen-print (-iftf-f x) (inc indent))]
    
   [(funcdecl? x)
    (pretty-display (format "~a(funcdecl: ~a"  indent (funcdecl-name x)))
    (codegen-print (funcdecl-body x) (inc indent))]

   [(vardecl? x)
    (pretty-display (format "~a(vardecl: ~a)" indent (vardecl-val x)))]

   [(aforth? x)
    (codegen-print (aforth-code x))]
   
   [else (raise (format "codegen-print: unimplemented for ~a" x))]))

;; racket syntax printting
(define (aforth-struct-print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))
  
  (cond
   [(equal? x #f)
    (pretty-display (format "~a#f" indent))]

   [(list? x)
    (pretty-display (format "~a;; list" indent))
    (pretty-display (format "~a(list " indent))
    (for ([i x])
	 (aforth-struct-print i (inc indent)))
    (pretty-display (format "~a)" indent))
    ]

   [(linklist? x)
    (unless (linklist-prev x)
	    (pretty-display (format "~a;; linklist" indent))
	    (pretty-display (format "~a(list " indent)))
    (when (linklist-entry x)
	  (aforth-struct-print (linklist-entry x) (inc indent)))
    (if (linklist-next x)
	(aforth-struct-print (linklist-next x) indent)
	(pretty-display (format "~a)" indent)))
    ]
   
   [(block? x)
    (pretty-display (format "~a(block" indent))

    (display (format "~a\"" (inc indent)))
    (if (list? (block-body x))
        (for ([i (block-body x)])
          (display i)
          (display " "))
        (display (block-body x)))
    (pretty-display "\"")

    (define cnstr (block-cnstr x))
    (display (format "~a~a ~a (restrict ~a ~a ~a ~a) " (inc indent) 
                     (block-in x) (block-out x)
                     (restrict-mem cnstr) 
                     (restrict-a cnstr) (restrict-b cnstr)
                     (restrict-r cnstr)
                     ))

    (define incnstr (block-incnstr x))
    (if (string? incnstr)
        (pretty-display (format "\"~a\"" incnstr))
        (pretty-display incnstr))

    (display (format "~a\"" (inc indent)))
    (if (list? (block-org x))
        (for ([i (block-org x)])
          (display i)
          (display " "))
        (display (block-org x)))
    (pretty-display "\")")]
   
   [(mult? x)
    (pretty-display (format "~a(mult)" indent))]
   
   [(funccall? x)
    (pretty-display (format "~a(funccall \"~a\")"  indent (funccall-name x)))]
   
   [(forloop? x)
    (pretty-display (format "~a(forloop "  indent))
    (aforth-struct-print (forloop-init x) (inc indent))
    (aforth-struct-print (forloop-body x) (inc indent))

    (display (inc indent))
    (when (forloop-iter x)
          (display "'"))
    (pretty-display (format "~a ~a ~a)" 
                            (forloop-iter x) (forloop-from x) (forloop-to x)))]
   
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

   [(vardecl? x)
    (pretty-display (format "~a(vardecl '~a)" indent (vardecl-val x)))]

   [(aforth? x)
    (pretty-display (format "~a(aforth " indent))
    (aforth-struct-print (aforth-code x) (inc indent))
    (pretty-display (format "~a~a ~a ~a)" 
			    indent (aforth-memsize x) (aforth-bit x) (aforth-indexmap x)))]

   [(vector? x)
    (pretty-display "(define programs")
    (pretty-display "  (vector")
    (define size (vector-length x))
    (for ([i (in-range size)])
         (pretty-display (format "    ;; core ~a" i))
	 (aforth-struct-print (vector-ref x i) (inc (inc indent))))
    (pretty-display "  ))")
    ]
   
   [else (raise (format "arrayforth-print: unimplemented for ~a" x))]))

(define (print-header)
  (pretty-display "#lang racket")
  (pretty-display "(require \"../src/header.rkt\" \"../src/arrayforth.rkt\" \"../src/arrayforth-optimize.rkt\" \"../src/arrayforth-print.rkt\")"))

(define (print-optimize name w h [core #f])
  (if core
      (begin
        (pretty-display (format "(define name \"~a-~a\")" name core))
        (pretty-display (format "(define real-opts (superoptimize program name ~a ~a #:id ~a))" w h core))
        (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a/~a-opt.rkt\" outdir name) (lambda () (aforth-struct-print real-opts)))")
        (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a/~a-opt.aforth\" outdir name)")
        (pretty-display (format "(lambda () (aforth-syntax-print real-opts ~a ~a #:id ~a)))" w h core)))
      (begin
        (pretty-display (format "(define name \"~a_cont\")" name))
        (pretty-display (format "(define real-opts (superoptimize programs name ~a ~a))" 
                                w h))
        (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a-opt.rkt\"  name) (lambda () (aforth-struct-print real-opts)))")
        (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a-opt.aforth\"  name)")
        (pretty-display (format "(lambda () (aforth-syntax-print real-opts ~a ~a)))" w h))
        )))    
        
        
  
