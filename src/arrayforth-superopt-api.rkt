#lang racket

(require "header.rkt" "arrayforth.rkt")

(provide (except-out (all-defined-out) stack-precond))

(define stack-precond #f)

(define (print-generic x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))
  
  (cond
   [(list? x)
    (pretty-display (format "~a(list " indent))
    (for ([i x])
	 (print-generic i (inc indent)))
    (pretty-display (format "~a)" indent))
    ]

   [(linklist? x)
    (unless (linklist-prev x)
	    (pretty-display (format "~a(list " indent)))
    (when (linklist-entry x)
	  (print-generic (linklist-entry x) (inc indent)))
    (if (linklist-next x)
	(print-generic (linklist-next x) indent)
	(pretty-display (format "~a)" indent)))
    ]

   [(and (block? x) 
         (or (and (list? (block-body x)) (empty? (block-body x)))
             (and (string? (block-body x)) (equal? (string-trim (block-body x)) ""))))
    (void)]
   
   [(block? x)
    (define body (if (list? (block-body x))
                      (string-join (block-body x))
                      (block-body x)))
    ;; STACK PRECONDITION from funcdecl
    (when stack-precond
	  (pretty-display (format "~a(assumption '(stack . ~a))" indent stack-precond))
	  (set! stack-precond #f)
	  )

    ;; EXTRA ASSUMPTION
    (define assume (block-incnstr x))
    ;(pretty-display (format "PRINT-GENERIC: block, body = ~a, incnstr = ~a" body assume))
    (define-syntax-rule (print-assume reg)
      (pretty-display (format "~a(assumption '(~a . (= . ~a)))" indent reg assume)))
    (when assume
	  (let ([inst (first (string-split body))])
	    (cond
	     [(member inst (list "!b" "@b"))
	      (print-assume 'b)]
	     [(member inst (list "!" "!+" "@" "@+"))
	      (print-assume 'a)]
	     [(member inst (list "b!" "a!"))
	      (print-assume 't)])))

    (pretty-display (format "~a(block" indent))

    ;; BODY
    (display (format "~a\"" (inc indent)))
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
    (display (format "(return . ~a) " (restrict-r cnstr))) ;; restrict-r = return stack
    (when (restrict-mem cnstr) (display "memory "))
    (when (restrict-a cnstr) (display "a "))
    (when (restrict-b cnstr) (display "b "))
    (display ") ")
    ;; recv (in)
    (pretty-display (format "~a))" (block-in x)))]
   
   [(mult? x)
    (pretty-display (format "~a(special \"mult\")" indent))]

   [(funccall? x)
    (pretty-display (format "~a(call \"~a\")" indent (funccall-name x)))]

   [(forloop? x)
    (pretty-display (format "~a(forloop "  indent))
    (print-generic (forloop-init x) (inc indent))
    (print-generic (forloop-body x) (inc indent))
    (pretty-display (format "~a~a)" 
                            (inc indent) 
                            (if (forloop-to x) 
                                (- (forloop-to x) (forloop-from x))
                                #f)))]

   [(ift? x)
    (pretty-display (format "~a(ift "  indent))
    (print-generic (ift-t x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(iftf? x)
    (pretty-display (format "~a(iftf "  indent))
    (print-generic (iftf-t x) (inc indent))
    (print-generic (iftf-f x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(-ift? x)
    (pretty-display (format "~a(-ift "  indent))
    (print-generic (-ift-t x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [(-iftf? x)
    (pretty-display (format "~a(-iftf "  indent))
    (print-generic (-iftf-t x) (inc indent))
    (print-generic (-iftf-f x) (inc indent))
    (pretty-display (format "~a)" indent))]
    
   [(funcdecl? x)
    (define info (funcdecl-info x))
    (define simple (labelinfo-simple info))
    (pretty-display (format "~a(label \"~a\""  indent (funcdecl-name x)))
    ;; PRECOND
    (when (and (list? (labelinfo-simple info)) 
               (ormap (lambda (x) x) (labelinfo-simple info)))
          (set! stack-precond (labelinfo-simple info)))

    ;; BODY
    (print-generic (funcdecl-body x) (inc indent))

    ;; INFO
    (pretty-display (format "~a(labelinfo ~a ~a ~a))"
			    (inc indent)
                            (labelinfo-data info) 
                            (labelinfo-return info)
			    (if simple #t #f)))
    ]

   [(vardecl? x)
    (pretty-display (format "~a(vardecl '~a)" indent (vardecl-val x)))]

   [(aforth? x)
    (pretty-display (format "~a(program " indent))
    (print-generic (aforth-code x) (inc indent))
    (pretty-display (format "~a~a ~a)" 
			    indent (aforth-memsize x) (aforth-indexmap x)))]
   
   [else (pretty-display (format "~a~a" indent x))]))

(define (print-generic-header)
  (define modular-dir "/home/mangpo/work/modular-optimizer")
  (pretty-display "#lang racket")
  (pretty-display "(require")
  (pretty-display (format "  (file \"~a/ast.rkt\")" modular-dir))
  (pretty-display (format "  (file \"~a/controller.rkt\")" modular-dir))
  (pretty-display (format "  (file \"~a/f18a.rkt\")" modular-dir))
  (pretty-display "  )"))

(define (print-generic-optimize name w h sliding [core #f])
  ;; Always use sliding windows.
  (if core
      (begin
        (pretty-display (format "(define name \"~a-~a\")" name core))
        (pretty-display (format "(define dir \"~a\")" outdir))
        (pretty-display (format "(define real-opts (optimize code))"))
        ;; (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a/~a-opt.rkt\" dir name) (lambda () (print-struct real-opts)))")
        (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a/~a-opt.aforth\" dir name)")
        (pretty-display (format "(lambda () (print-syntax real-opts ~a ~a ~a)))" w h core)))
      (begin
        (pretty-display (format "(define name \"~a_cont\")" name))
        (pretty-display (format "(define real-opts (optimize code))"))
        ;; (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a-opt.rkt\"  name) (lambda () (print-struct real-opts)))")
        (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a-opt.aforth\"  name)")
        (pretty-display (format "(lambda () (print-syntax real-opts ~a ~a)))" w h))
        )))

(define (generic-form programs)
  (pretty-display `(generic-form ,programs))

  (define func-dict (make-hash))
  (define stack-graph (make-hash))

  (struct edge (to data return))
  (define data-cnstr (make-hash))
  (define return-cnstr (make-hash))

  (define built-in (list (list "in" 0 1)
                         (list "out" 1 0)
                         (list "--u/mod" 3 2)
                         (list "*.17" 2 2)
                         (list "*." 2 2)))

  (for ([f built-in])
       (let ([name (first f)])
         (hash-set! func-dict name (funcinfo (second f) (third f) #f))
         ;(hash-set! data-cnstr name 0)
         ;(hash-set! return-cnstr name 0)
         ))

  (define (generic-func my-name x)
    (define dstack 0) ;; TODO: init to #reg on stack (no need)
    (define rstack 0)
    (define recv 0)
    (define start #t)
    
    (define (f x)
      (cond
       [(list? x)
	(for ([i x]) (f i))]
       
       [(linklist? x)
	(when (linklist-entry x) (f (linklist-entry x)))
	(when (linklist-next x) (f (linklist-next x)))]
       
       [(block? x)
	(define body (if (string? (block-body x)) 
			 (string-split (block-body x)) 
			 (block-body x)))
	(set! dstack (+ dstack (- (block-out x) (block-in x))))
	(set-block-out! x dstack) ;; dstack
	(for ([i body])
	     (cond
	      [(equal? i "push") (set! rstack (add1 rstack))]
	      [(equal? i "pop") (set! rstack (sub1 rstack))]))
	(set-restrict-r! (block-cnstr x) rstack) ;; rstack

	(define index (index-of body "@b"))
	(cond
         [(equal? index #f)
          (set-block-in! x 0)]
	 [(and (>= index 2) 
	       (equal? (list-ref body (- index 1)) "b!")
	       (member (list-ref body (- index 2)) (list "up" "down" "left" "right" "io")))
	  (set-block-in! x 1)]
	 [(and (< index 2) start)
	  (set-block-in! x 1)
	  (unless (block-incnstr x) (set-block-incnstr! x "uplr"))
	  ]
	 [else
	  (set-block-in! x 0)])
	 
	(set! start #f)
	]

       [(mult? x)
	(set! start #f)
	(set! dstack (sub1 dstack))]

       [(funccall? x)
	(define name (funccall-name x))
	(define info (hash-ref func-dict name))
	;; Collect how deep the current stack is for funcdecl x.

	;; (when (< (hash-ref data-cnstr name) dstack)
	;;       (hash-set! data-cnstr name dstack))
	;; (when (< (hash-ref return-cnstr name) rstack)
	;;       (hash-set! return-cnstr name rstack))

        ;; Only collect when the dest is not special function.
        (when (hash-has-key? stack-graph name)
              (pretty-display (format "Add to stack graph: (~a,~a) ~a ~a" my-name name dstack rstack))
              (hash-set! stack-graph my-name (cons (edge name dstack rstack) 
                                                   (hash-ref stack-graph my-name))))

	;; Update dstack.
	(set! dstack (+ (- dstack (funcinfo-in info)) (funcinfo-out info)))

	(if (regexp-match #rx"rep" (funccall-name x))
	    (set! start #t)
	    (set! start #f))
	]

       [(forloop? x)
	(f (forloop-init x))

        ;; Compress loopbound.
        (aforth-struct-print x)
        (define init-block (linklist-entry (linklist-next (forloop-init x))))
        (when init-block
              (define init-list (block-body init-block))
              (define c (string->number (last init-list)))
              (when (and c (> c 1))
                    (set-block-body! 
                     init-block 
                     (append (take init-list (sub1 (length init-list))) (list "1")))))
              
	(set! dstack (sub1 dstack))
	(set! rstack (add1 rstack))
	(f (forloop-body x))
	(set! rstack (sub1 rstack))
	]
       
       [(ift? x)
	(f (ift-t x))]
       
       [(iftf? x)
	(define my-dstack dstack)
	(f (iftf-t x))
	(set! dstack my-dstack)
	(f (iftf-f x))]
       
       [(-ift? x)
	(f (-ift-t x))]
       
       [(-iftf? x)
	(define my-dstack dstack)
	(f (-iftf-t x))
	(set! dstack my-dstack)
	(f (-iftf-f x))]))
    (f x)
    dstack)

  (define (generic-program x)
    (define (modify-body i)
      (define name (funcdecl-name i))
      (define info (funcdecl-info i))
      (hash-set! func-dict name info)
      (hash-set! stack-graph name (list))
      ;; (hash-set! data-cnstr name 0)
      ;; (hash-set! return-cnstr name 0)

      ;; Call generic-func
      (define depth (generic-func name (funcdecl-body i)))
      (if (funcinfo-in info)
          (unless (or (= depth (- (funcinfo-out info) (funcinfo-in info)))
                      (and (equal? name "main") (= depth 1)))
            (raise (format "arrayforth-superopt-api: modify-body: at function ~a, depth != out - in, depth = ~a, out = ~a, in = ~a" name depth (funcinfo-out info) (funcinfo-in info))))
          ;; update funcinfo for Xrep functions
          (hash-set! func-dict name (funcinfo 0 depth #f)))
      )

    (define (update-funcdecl i)
      (define name (funcdecl-name i))
      (pretty-display (format "Lebalinfo: ~a ~a ~a" 
                              name (hash-ref data-cnstr name) (hash-ref return-cnstr name)))
      (set-funcdecl-info! i (labelinfo (hash-ref data-cnstr name)
                                       (hash-ref return-cnstr name)
                                       (funcinfo-simple (funcdecl-info i)))))
    (define func-names (list))
    (define (traverse-linklist ll lam)
      (define (inner ll)
        (when (and (linklist-entry ll) (funcdecl? (linklist-entry ll)))
              (lam (linklist-entry ll)))
        (when (linklist-next ll)
              (inner (linklist-next ll))))
      (inner ll))
            
    ;; Modify body.
    (traverse-linklist x modify-body)
    ;; Get all function names (in topo order)
    (traverse-linklist x (lambda (i) (set! func-names (cons (funcdecl-name i) func-names))))
    ;; Construct data-cnstr, return-cnstr for each function
    (pretty-display (format "Topo sort: ~a" func-names))
    (for ([func func-names])
         (hash-set! data-cnstr func 0)
         (hash-set! return-cnstr func 0))
    (for ([func func-names])
         (let ([my-data (hash-ref data-cnstr func)]
               [my-return (hash-ref return-cnstr func)])
           (pretty-display (format "Construct: ~a ~a" func (hash-ref stack-graph func)))
           (for ([e (hash-ref stack-graph func)])
                (let ([to (edge-to e)])
                  (when (> (+ my-data (edge-data e)) (hash-ref data-cnstr to))
                        (hash-set! data-cnstr to (+ my-data (edge-data e))))
                  (when (> (add1 (+ my-return (edge-return e))) (hash-ref return-cnstr to))
                        (hash-set! return-cnstr to (add1 (+ my-return (edge-return e)))))))
           ))
    ;; Update funcinfo to labelinfo
    (traverse-linklist x update-funcdecl))

  (for ([program programs]
        [i (in-range (vector-length programs))])
       (pretty-display (format "generic-form ~a >>>" i))
       (when program (generic-program (aforth-code program)))))
