#lang racket

(require "header.rkt" "arrayforth.rkt" "arrayforth-print.rkt"
         "../../forth-interpreter/machine/cegis.rkt" 
         "../../forth-interpreter/machine/state.rkt" 
         "../../forth-interpreter/machine/track-constant.rkt")

(provide superoptimize renameindex)

(define (out-space out cnstr)
  (define result
    (if (= out 0)
	(constraint s t)
	(constraint-data out s t)))
  (struct-copy progstate result [memory (restrict-mem cnstr)] [a (restrict-a cnstr)]))

(define index-map #f)
(define mem-size #f)
(define bit #f)

(define name #f)
(define w #f)
(define h #f)
(define id #f)

(define (superoptimize ast my-name my-w my-h)
  (set! name my-name)
  (set! w my-w)
  (set! h my-h)
  (set! id 0)
  (system (format "rm ~a/~a-work.rkt" outdir name))
  (system (format "rm ~a/~a-work.aforth" outdir name))
  (superoptimize-inner ast))

;; optimize per-core program
(define (superoptimize-inner ast)
  (cond
   [(equal? ast #f) #f]

   [(block? ast)
    (define out (block-out ast))

    (define opt (block (optimize (if (string? (block-body ast))
                                     (block-body ast)
                                     (string-join (block-body ast)))
                                 #:f18a #f
                                 #:num-bits bit #:name name
                                 #:constraint (out-space out (block-cnstr ast))
                                 #:mem mem-size #:start mem-size)
                       (block-in ast) out (block-cnstr ast) (block-org ast)))

    (define renamed (renameindex opt mem-size bit index-map))

    
    (with-output-to-file #:exists 'append 
      (format "~a/~a-work.rkt" outdir name)
      (lambda () 
        (pretty-display ";; original")
        (aforth-struct-print ast)
        (pretty-display ";; optimized")
        (aforth-struct-print opt)
        (pretty-display ";; result")
        (aforth-struct-print renamed)))

    renamed
    ]

   [(list? ast)
    (for/list ([x ast])
              (superoptimize-inner x))]

   [(mult? ast)
    (mult)]

   [(funccall? ast)
    (funccall (funccall-name ast))]

   [(forloop? ast)
    (forloop (superoptimize-inner (forloop-init ast))
             (superoptimize-inner (forloop-body ast))
             (forloop-iter ast)
             (forloop-from ast)
             (forloop-to ast))]

   [(ift? ast)
    (ift (superoptimize-inner (ift-t ast)))]

   [(iftf? ast)
    (iftf (superoptimize-inner (iftf-t ast))
          (superoptimize-inner (iftf-f ast)))]

   [(-ift? ast)
    (-ift (superoptimize-inner (-ift-t ast)))]

   [(-iftf? ast)
    (-iftf (superoptimize-inner (-iftf-t ast))
           (superoptimize-inner (-iftf-f ast)))]

   [(funcdecl? ast)
    (funcdecl (funcdecl-name ast)
              (superoptimize-inner (funcdecl-body ast)))]

   [(vardecl? ast)
    (vardecl (vardecl-val ast))]

   [(aforth? ast)
    (set! bit (if (< (aforth-bit ast) 9) 9 (aforth-bit ast)))
    (set! mem-size (aforth-memsize ast))
    (set! index-map (aforth-indexmap ast))
    (define result (superoptimize-inner (aforth-code ast)))
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

    (with-output-to-file #:exists 'append 
      (format "~a/~a-work.aforth" outdir name)
      (lambda () 
        (aforth-syntax-print ret w h #:id id)
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

         ;; set id for printing
         (set! id i)

         (let* ([program (vector-ref ast i)]
                [result (superoptimize-inner program)])
           (vector-set! output-programs i result)))
    
    (vector-set! output-programs (sub1 n) (vector-ref ast (sub1 n)))
    output-programs]

   [else
    (raise (format "arrayforth: superoptimize: unimplemented for ~a" ast))]))

(define (renameindex ast mem-size bit index-map)
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
      (block org (block-in ast) (block-out ast) (block-cnstr ast) org)
      (block (string-join new-body) (block-in ast) (block-out ast) (block-cnstr ast) (block-org ast))))
