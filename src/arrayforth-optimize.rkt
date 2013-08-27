#lang racket

(require "header.rkt" "arrayforth.rkt" "arrayforth-print.rkt" "arrayforth-def.rkt" 
         "../../forth-interpreter/machine/cegis.rkt" 
         "../../forth-interpreter/machine/state.rkt" 
         "../../forth-interpreter/machine/track-constant.rkt")

(provide superoptimize renameindex)

(define (out-space out cnstr)
  (define result
    (if (= out 0)
	(constraint s t r)
	(constraint-data out s t r)))
  (struct-copy progstate result 
               [memory (restrict-mem cnstr)] 
               [a (restrict-a cnstr)]
               ;[r (restrict-r cnstr)]
               ))

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
  (define (collect-code x big-block limit)
    (define entry (linklist-entry x))
    (cond
     [(block? entry)
      (define body (block-body entry))
      (define seq (if (string? body) (string-split body) body))
      
      (if (<= (+ (length (block-body big-block)) (length seq)) limit)
          (collect-code (linklist-next x) (merge-block big-block entry) limit)
          (values x big-block))]
     
     [else (values x big-block)]))
  
  (cond
   [(equal? ast #f) #f]

   [(block? ast)
    ;(pretty-display "superoptimize: block")
    ;; (raise "superoptimizer: optimize via linklist not block!")
    (define out (block-out ast))

    (define result (optimize (if (string? (block-body ast))
                                 (block-body ast)
                                 (string-join (block-body ast)))
                             #:f18a #f
                             #:num-bits bit #:name name
                             #:constraint (out-space out (block-cnstr ast))
                             #:mem mem-size #:start mem-size))

    (define opt (if (equal? result 'timeout)
                    ast
                    (block result (block-in ast) out (block-cnstr ast) (block-org ast))))

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

   [(linklist? ast)
    ;(pretty-display (format "superoptimize: linklist ~a" (linklist-entry ast)))
    (cond
     [(and (linklist-entry ast) (block? (linklist-entry ast)))
      ;; optimize code in group of blocks whose length <= len
      ;; if timeout => reduce len
      ;; if getting result => return the optimized block
      (define (optimize-loop len)
        (define-values (next block-noopt) 
          (collect-code ast (block (list) 0 0 (restrict #f #f #f #f) (list)) len))
        
        (define out (block-out block-noopt))
        (define body (block-body block-noopt))
        (pretty-display `(optimize-loop ,len ,body))
        (define cnstr (block-cnstr block-noopt))
        (define result (optimize (string-join body)
				 #:f18a #f
				 #:num-bits bit #:name name
				 #:constraint (out-space out cnstr)
				 #:mem mem-size #:start mem-size))

        (if (equal? result 'timeout)
            (let* ([last-block (linklist-entry (linklist-prev next))]
                   [last-body (block-body last-block)]
                   [last-list (if (string? last-body) (string-split last-body) last-body)])
              (optimize-loop (- (length body) (max 1 (length last-list)))))

            (begin
              (set-block-body! block-noopt result)
              (values next block-noopt))))

      (define-values (next opt) (optimize-loop block-limit))
      (pretty-display "OPTMIZE: DONE")
      (define renamed (renameindex opt mem-size bit index-map))
      (pretty-display "RENAME: DONE")
    
      (with-output-to-file #:exists 'append 
                           (format "~a/~a-work.rkt" outdir name)
                           (lambda () 
                             (display ";; original: ")
                             (pretty-display (block-org renamed))
                             (display ";; optimized: ")
                             (pretty-display (block-body renamed))
                             (pretty-display ";; result")
                             (aforth-struct-print renamed)))

      (define res (if (string? (block-body renamed))
                      (block-body renamed)
                      (string-trim (string-join (block-body renamed)))))
      (define org (if (string? (block-org renamed)) 
                      (block-org renamed)
                      (string-trim (string-join (block-org renamed)))))

      (if (and (equal? res org) (block? (linklist-entry next)))
          (superoptimize-inner (linklist-next ast))
          (begin
            (set-linklist-entry! ast renamed)
            (set-linklist-next! ast next)
            (set-linklist-prev! next ast)
            (superoptimize-inner next)))]

     [(linklist-entry ast)
      (superoptimize-inner (linklist-entry ast))
      (superoptimize-inner (linklist-next ast))]

     [(linklist-next ast)
      (superoptimize-inner (linklist-next ast))]
     )

    ast]
            

   [(list? ast)
    ;(pretty-display "superoptimize: list")
    (for/list ([x ast])
              (superoptimize-inner x))]

   [(mult? ast)
    (mult)]

   [(funccall? ast)
    (funccall (funccall-name ast))]

   [(forloop? ast)
    ;(pretty-display "superoptimize: forloop")
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
    ;(pretty-display "superoptimize: funcdecl")
    (funcdecl (funcdecl-name ast)
              (superoptimize-inner (funcdecl-body ast)))]

   [(vardecl? ast)
    ;(pretty-display "superoptimize: vardel")
    (vardecl (vardecl-val ast))]

   [(aforth? ast)
    ;(pretty-display "superoptimize: aforth")
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
  
    (vector-set! output-programs (sub1 n) (vector-ref ast (sub1 n)))

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

         (let* ([program (aforth-linklist (vector-ref ast i) list->linklist)]
                [result (superoptimize-inner program)])
           (vector-set! output-programs i result)))
    
    output-programs]

   [else
    (raise (format "arrayforth: superoptimize: unimplemented for ~a" ast))]))

(define (renameindex ast mem-size bit index-map)
  (define (renameindex-inner)
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
                                new-mem-size (out-space (block-out ast) (block-cnstr ast)) bit))
    
    (if diff
        (begin
          (pretty-display "VALIDATE: different")
          (pretty-display (string-join new-body))
          (pretty-display org))
        (pretty-display "VALIDATE: same"))
    
    (if diff
        (block org (block-in ast) (block-out ast) (block-cnstr ast) org)
        (block (string-join new-body) (block-in ast) (block-out ast) (block-cnstr ast) (block-org ast))))
  
  (if (> (dict-ref index-map mem-size) reduce-limit)
      (renameindex-inner)
      (block (block-body ast) (block-in ast) (block-out ast) (block-cnstr ast) (block-org ast))))
