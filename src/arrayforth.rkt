#lang racket

(require "../../forth-interpreter/ArrayForth/compiler.rkt" 
         "../../forth-interpreter/machine/cegis.rkt" 
         "../../forth-interpreter/machine/state.rkt")

(provide (all-defined-out))

(struct block (body in out mem) #:mutable)
(struct mult ()) ;; : mult (x y -> z) a! 0 17 for +* next drop drop a ;
(struct funccall (name))
(struct funcdecl (name body) #:mutable)
(struct forloop (init body))
(struct ift (t))
(struct iftf (t f))
(struct -ift (t))
(struct -iftf (t f))
(struct aforth (code memsize bit))

(define-syntax gen-block
  (syntax-rules ()
    [(gen-block)
     (block (list) 0 0 #t)]
    [(gen-block mem)
     (block (list) 0 0 mem)]
    [(gen-block a ... in out)
     (block (list a ...) in out #t)]))

(define-syntax prog-append
  (syntax-rules ()
    [(prog-append a b) (program-append a b)]
    [(prog-append a b c ...)
     (prog-append (program-append a b) c ...)]))

    (define (program-append a-list b-list)
      ;; merge b-block into a-block
      (define (merge-into a-block b-block)
	(set-block-body! a-block (append (block-body a-block) (block-body b-block)))
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
	(if (and (block? a-last) (block? b-first))
	    (begin
	      (merge-into a-last b-first)
	      (append a-list (cdr b-list)))
	    (append a-list b-list))]))

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
    (display (format "~a(block: " indent))
    (if (list? (block-body x))
        (for ([i (block-body x)])
          (display i)
          (display " "))
        (display (block-body x)))
    (pretty-display (format ", in:~a out:~a mem:~a)" (block-in x) (block-out x) (block-mem x)))]
   
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
   
   [else (raise (format "visitor-codegen: print: unimplemented for ~a" x))]))

;; compile arrayforth to F18A machine code
(define (aforth->f18a program)
  (if (list? program)
      (compile-to-string (string-join program))
      (compile-to-string program)))

;; optimize per-core program
(define (superoptimize ast name mem-size [bit 9])
  (cond
   [(block? ast)
    (define out (block-out ast))
    (define out-space
      (cond 
       [(= out 0) (constraint memory)]
       [(= out 1) (constraint memory t)]
       [(= out 2) (constraint memory s t)]
       [(> out 2) (constraint memory s t data)]))

    (block (optimize (aforth->f18a (block-body ast))
                     #:num-bits bit #:name name
                     #:constraint out-space
                     #:mem mem-size #:start mem-size)
           (block-in ast) out (block-mem ast))]

   [(list? ast)
    (for/list ([x ast])
              (superoptimize x name mem-size bit))]

   [(mult? ast)
    (mult)]

   [(funccall? ast)
    (funccall (funccall-name ast))]

   [(forloop? ast)
    (forloop (superoptimize (forloop-init ast) name mem-size bit)
             (superoptimize (forloop-body ast) name mem-size bit))]

   [(ift? ast)
    (ift (superoptimize (ift-t ast) name mem-size bit))]

   [(iftf? ast)
    (iftf (superoptimize (iftf-t ast) name mem-size bit)
          (superoptimize (iftf-f ast) name mem-size bit))]

   [(-ift? ast)
    (-ift (superoptimize (-ift-t ast) name mem-size bit))]

   [(-iftf? ast)
    (-iftf (superoptimize (-iftf-t ast) name mem-size bit)
           (superoptimize (-iftf-f ast) name mem-size bit))]

   [(funcdecl? ast)
    (funcdecl (funcdecl-name ast)
              (superoptimize (funcdecl-body ast) name mem-size bit))]

   [else
    (raise (format "arrayforth: superoptimize: unimplemented for ~a" ast))]))

(define (superoptimize-program program name)
  (define bit (if (< (aforth-bit program) 9) 9 (aforth-bit program)))
  (define result (superoptimize (aforth-code program) 
				name (aforth-memsize program) bit))
  (pretty-display `(-------------------- result -----------------------))
  (codegen-print result)
  (aforth result (aforth-memsize program) bit))

;; optimize many-core programs
(define (superoptimize-programs programs name)
  (define n (vector-length programs))
  (define output-programs (make-vector n))
  
  (for ([i (in-range (sub1 n))])
    (pretty-display `(-------------------- ,i -----------------------))
    (let* ([program (vector-ref programs i)]
	   [result (superoptimize-program program name)])
      (vector-set! output-programs i result)))

  (vector-set! output-programs (sub1 n) (vector-ref programs (sub1 n)))

  output-programs)

;(superoptimize (gen-block "up" "b!" "!b" "325" "b!" "!b" 0 0) "comm" 0 9)
