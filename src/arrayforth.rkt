#lang racket

(require "header.rkt"
         "../../forth-interpreter/machine/cegis.rkt" 
         "../../forth-interpreter/machine/state.rkt" 
         "../../forth-interpreter/machine/track-constant.rkt")

(provide (all-defined-out))

(struct block (body in out mem org) #:mutable)
(struct mult ()) ;; : mult (x y -> z) a! 0 17 for +* next drop drop a ;
(struct funccall (name))
(struct funcdecl (name body) #:mutable)
(struct vardecl (val))
(struct forloop (init body))
(struct ift (t))
(struct iftf (t f))
(struct -ift (t))
(struct -iftf (t f))
(struct aforth (code memsize bit indexmap))

(define ga-bit 18)

(define-syntax gen-block
  (syntax-rules ()
    [(gen-block)
     (block (list) 0 0 #t (list))]
    [(gen-block mem)
     (block (list) 0 0 mem (list))]
    [(gen-block a ... in out)
     (block (list a ...) in out #t (list a ...))]))

(define-syntax gen-block-org
  (syntax-rules ()
    [(gen-block-org (a ...) (b ...) in out)
     (block (list a ...) in out #t (list b ...))]))

(define-syntax prog-append
  (syntax-rules ()
    [(prog-append a b) (program-append a b)]
    [(prog-append a b c ...)
     (prog-append (program-append a b) c ...)]))

(define (program-append a-list b-list [no-limit #f])
  ;; merge b-block into a-block
  (define (merge-into a-block b-block)
    ;; (pretty-display "MERGE:")
    ;; (codegen-print a-block)
    ;; (codegen-print b-block)
    (set-block-body! a-block (append (block-body a-block) (block-body b-block)))
    (set-block-org! a-block (append (block-org a-block) (block-org b-block)))
    (set-block-mem! a-block (and (block-mem a-block) (block-mem b-block)))
    (define a-in  (block-in a-block))
    (define a-out (block-out a-block))
    (define b-in  (block-in  b-block))
    (define b-out (block-out  b-block))
    (if (>= a-out b-in)
	(set-block-out! a-block (- (+ a-out b-out) b-in))
	(begin
	  (set-block-in! a-block (- (+ a-in b-in) a-out))
	  (set-block-out! a-block b-out))))
  
  (cond
   [(empty? a-list) b-list]
   [(empty? b-list) a-list]
   [else
    (define a-last (last a-list))
    (define b-first (car b-list))
    (if (and (block? a-last) (block? b-first)
	     ;; if more than 30, too big, don't merge.
             (or no-limit
                 (<= (+ (length (block-body a-last)) (length (block-body b-first))) 16)))
	(begin
	  (merge-into a-last b-first)
	  (append a-list (cdr b-list)))
	(append a-list b-list))]))

;; casual printting
(define (codegen-print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))

  (cond
   [(list? x)
    (for ([i x])
	 (codegen-print i indent))]
   
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
    (pretty-display (format "~ain:~a out:~a mem:~a)" (inc indent) 
                            (block-in x) (block-out x) (block-mem x)))]
   
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
    (pretty-display (format "~a(forloop "  indent))
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

   [(vardecl? x)
    (pretty-display (format "~a(vardecl '~a)" indent (vardecl-val x)))]

   [(aforth? x)
    (pretty-display (format "~a(aforth " indent))
    (aforth-struct-print (aforth-code x) (inc indent))
    (pretty-display (format "~a~a ~a ~a)" 
			    indent (aforth-memsize x) (aforth-bit x) (aforth-indexmap x)))]

   [(vector? x)
    (pretty-display "#lang racket")
    (pretty-display "(require \"../src/arrayforth.rkt\")")
    (pretty-display "(define programs")
    (pretty-display "  (vector")
    (define size (vector-length x))
    (for ([i (in-range size)])
         (pretty-display (format "    ;; core ~a" i))
	 (aforth-struct-print (vector-ref x i) (inc (inc indent))))
    (pretty-display "  ))")
    (pretty-display "(superoptimize programs \"foo\")")
    ]
   
   [else (raise (format "arrayforth-print: unimplemented for ~a" x))]))


(define (out-space out)
  (if (= out 0)
      (constraint memory s t)
      (constraint-data out memory s t)))

(define index-map #f)
(define mem-size #f)
(define bit #f)

;; optimize per-core program
(define (superoptimize ast name)
  (cond
   [(block? ast)
    (define out (block-out ast))

    (define result
      (block (optimize (if (string? (block-body ast))
                           (block-body ast)
                           (string-join (block-body ast)) )
                       #:f18a #f
                       #:num-bits bit #:name name
                       #:constraint (out-space out)
                       #:mem mem-size #:start mem-size)
             (block-in ast) out (block-mem ast) (block-org ast)))
    (with-output-to-file #:exists 'append 
      (format "~a/~a-work.rkt" outdir name)
      (lambda () 
        (pretty-display ";; original")
        (aforth-struct-print ast)
        (pretty-display ";; optimized")
        (aforth-struct-print result)))
    result
    ]

   [(list? ast)
    (for/list ([x ast])
              (superoptimize x name))]

   [(mult? ast)
    (mult)]

   [(funccall? ast)
    (funccall (funccall-name ast))]

   [(forloop? ast)
    (forloop (superoptimize (forloop-init ast) name)
             (superoptimize (forloop-body ast) name))]

   [(ift? ast)
    (ift (superoptimize (ift-t ast) name))]

   [(iftf? ast)
    (iftf (superoptimize (iftf-t ast) name)
          (superoptimize (iftf-f ast) name))]

   [(-ift? ast)
    (-ift (superoptimize (-ift-t ast) name))]

   [(-iftf? ast)
    (-iftf (superoptimize (-iftf-t ast) name)
           (superoptimize (-iftf-f ast) name))]

   [(funcdecl? ast)
    (funcdecl (funcdecl-name ast)
              (superoptimize (funcdecl-body ast) name))]

   [(vardecl? ast)
    (vardecl (vardecl-val ast))]

   [(aforth? ast)
    (set! bit (if (< (aforth-bit ast) 9) 9 (aforth-bit ast)))
    (set! mem-size (aforth-memsize ast))
    (define result (superoptimize (aforth-code ast) name))
    (pretty-display `(-------------------- result -----------------------))
    (codegen-print result)
    (define ret
      (aforth result (aforth-memsize ast) bit (aforth-indexmap ast)))
    
    (with-output-to-file #:exists 'append 
      (format "~a/~a-work.rkt" outdir name)
      (lambda () 
        (pretty-display ";; original")
        (aforth-struct-print ast)
        (pretty-display ";; optimized")
        (aforth-struct-print ret)
        (pretty-display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
        ))
    ret
    ]

   [(vector? ast)
    (define n (vector-length ast))
    (define output-programs (make-vector n))
  
    (for ([i (in-range (sub1 n))])
         (with-output-to-file #:exists 'append 
           (format "~a/~a-work.rkt" outdir name)
           (lambda ()
             (pretty-display 
              (format ";;;;;;;;;;;;;;;;;;;;;;;; ~a ;;;;;;;;;;;;;;;;;;;;;;;;;;" i))))
         (pretty-display 
          (format ";;;;;;;;;;;;;;;;;;;;;;;; ~a ;;;;;;;;;;;;;;;;;;;;;;;;;;" i))
         (let* ([program (vector-ref ast i)]
                [result (superoptimize program name)])
           (vector-set! output-programs i result)))
    
    (vector-set! output-programs (sub1 n) (vector-ref ast (sub1 n)))
    output-programs]

   [else
    (raise (format "arrayforth: superoptimize: unimplemented for ~a" ast))]))

(define (renameindex ast)
  (cond
   [(block? ast)
    (define body (string-split (block-body ast)))
    (define org (block-org ast))
    (define rename-set (track-index body))
    (pretty-display `(renameindex ,body ,rename-set ,index-map))
    (define new-body
      (for/list ([inst body]
                 [i (in-range (length body))])
                (if (and (set-member? rename-set i) 
                         (dict-has-key? index-map (string->number inst)))
                    (number->string (dict-ref index-map (string->number inst)))
                    inst)))

    (define new-mem-size (dict-ref index-map mem-size))
    (define diff (program-diff? org new-body
                                 new-mem-size (out-space (block-out ast)) bit))

    (if diff
        (begin
          (pretty-display "VALIDATE: different")
          (pretty-display (string-join new-body))
          (pretty-display org))
        (pretty-display "VALIDATE: same"))

    (if diff
        (block org (block-in ast) (block-out ast) new-mem-size org)
        (block (string-join new-body) (block-in ast) (block-out ast) new-mem-size (block-org ast)))]

   [(list? ast)
    (for/list ([x ast])
              (renameindex x))]

   [(mult? ast)
    (mult)]

   [(funccall? ast)
    (funccall (funccall-name ast))]

   [(forloop? ast)
    (forloop (renameindex (forloop-init ast))
             (renameindex (forloop-body ast)))]

   [(ift? ast)
    (ift (renameindex (ift-t ast)))]

   [(iftf? ast)
    (iftf (renameindex (iftf-t ast))
          (renameindex (iftf-f ast)))]

   [(-ift? ast)
    (-ift (renameindex (-ift-t ast)))]

   [(-iftf? ast)
    (-iftf (renameindex (-iftf-t ast))
           (renameindex (-iftf-f ast)))]

   [(funcdecl? ast)
    (funcdecl (funcdecl-name ast)
              (renameindex (funcdecl-body ast)))]

   [(vardecl? ast)
    (vardecl (vardecl-val ast))]

   [(aforth? ast)
    (set! index-map (aforth-indexmap ast))
    (set! mem-size (aforth-memsize ast))
    (set! bit (aforth-bit ast))
    (aforth (renameindex (aforth-code ast))
            (dict-ref index-map mem-size) bit #f)]

   [(vector? ast)
    (define n (vector-length ast))
    (define output-programs (make-vector n))
    
    (for ([i (in-range (sub1 n))])
         (vector-set! output-programs i 
                      (renameindex (vector-ref ast i))))
    
    (vector-set! output-programs (sub1 n) (vector-ref ast (sub1 n)))
    output-programs]

   [else
    (raise (format "arrayforth: renameindex: unimplemented for ~a" ast))]))

;(superoptimize (gen-block "up" "b!" "!b" "325" "b!" "!b" 0 0) "comm" 0 9)

#|
(codegen-print
 (renameindex 
  (aforth 
   (list 
    (funcdecl "main"
              (list 
               (block "right a! @ 4 a! @ 0 . + a! @ . + 2 a! @ 2 +" 0 2 #t)
               ))
    )
   5 9 #hash((4 . 40) (2 . 20)))))|#
  