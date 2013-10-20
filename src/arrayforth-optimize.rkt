#lang racket

(require "header.rkt" "arrayforth.rkt" "arrayforth-print.rkt" "arrayforth-def.rkt" 
         "../../forth-interpreter/machine/cegis.rkt" 
         "../../forth-interpreter/machine/state.rkt" 
         "../../forth-interpreter/machine/programs.rkt" 
         "../../forth-interpreter/machine/track-constant.rkt")

(provide superoptimize renameindex)

(define (out-space out cnstr)
  (define result
    (if (= out 0)
	(constraint s t r)
	(constraint (data out) s t r)))
  (struct-copy progstate result 
               [memory (restrict-mem cnstr)] 
               [a (restrict-a cnstr)]
               [b (restrict-b cnstr)]
               [r #t]
               [return (if (restrict-r cnstr) 2 2)]
               ))

(define (in-constraint data inst)
  (cond
   [(equal? data #f) 
    (default-state)]
   [(or (equal? inst "b!") (equal? inst "a!")) 
    (default-state (t (port-number data)))]
   [(or (equal? inst "!b") (equal? inst "@b")) 
    (default-state (b (port-number data)))]
   [(or (equal? inst "!") (equal? inst "@")) 
    (default-state (a (port-number data)))]
   [else
    (default-state)]))
  

(define index-map #f)
(define mem-size #f)
(define bit #f)

(define name #f)
(define w #f)
(define h #f)
(define id #f)
(define before 0)
(define after 0)

(define (superoptimize ast my-name my-w my-h #:id [id 0])
  (set! name my-name)
  (set! w my-w)
  (set! h my-h)
  (set! id id)
  (set! before 0)
  (set! after 0)
  (system (format "rm ~a/~a.stat" outdir name))
  (system (format "rm ~a/~a-work.rkt" outdir name))
  (system (format "rm ~a/~a-work.aforth" outdir name))
  (let ([result (superoptimize-inner ast)])
    ;; (with-output-to-file #:exists 'append 
    ;;                      (format "~a/~a.stat" outdir name)
    ;;                      (lambda () 
    ;;                        (pretty-display (format "~a ~a (TOTAL)" before after))))
    result))

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
    (pretty-display "superoptimize: block")
    ;; (raise "superoptimizer: optimize via linklist not block!")
    (define out (block-out ast))
    (define body-list (if (string? (block-body ast)) 
                          (string-split (block-body ast))
                          (block-body ast)))
                     
    (define result (optimize (string-join body-list)
                             #:f18a #f
                             #:num-bits max-bit #:name name
                             #:constraint (out-space out (block-cnstr ast))
                             #:start-state (if (empty? body-list) 
                                               (default-state)
                                               (in-constraint (block-incnstr ast) 
                                                              (car body-list)))
                             #:mem mem-size #:start mem-size))

    (define opt (if (equal? result 'timeout)
                    ast
                    (new-block result (block-in ast) out (block-cnstr ast) (block-org ast))))

    (define renamed (renameindex opt mem-size index-map))
    (pretty-display "CASE 3")
    (aforth-syntax-print renamed 1 1)
    
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
    (pretty-display (format "superoptimize: linklist ~a" (linklist-entry ast)))
    (cond
     [(and (linklist-entry ast) (block? (linklist-entry ast)))
      ;; optimize code in group of blocks whose length <= len
      ;; if timeout => reduce len
      ;; if getting result => return the optimized block
      (define (optimize-loop len)
        (define-values (next block-noopt) 
          (collect-code ast (new-block (list) 0 0 (restrict #t #f #f #f) (list)) len))
        
        (define out (block-out block-noopt))
        (define body (block-body block-noopt))
        (pretty-display `(optimize-loop ,len ,body))
        (define cnstr (block-cnstr block-noopt))
        (define result (optimize (string-join body)
				 #:f18a #f
				 #:num-bits max-bit #:name name
				 #:constraint (out-space out cnstr)
                                 #:start-state (if (empty? body)
                                                   (default-state)
                                                   (in-constraint 
                                                    (block-incnstr block-noopt)
                                                    (car body)))
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
      (define renamed (renameindex opt mem-size index-map))
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

      (define res-len (length-with-literal res))
      (define org-len (length-with-literal org))
      (set! before (+ before org-len))
      (set! after (+ after res-len))
    
      (with-output-to-file #:exists 'append 
                           (format "~a/~a.stat" outdir name)
                           (lambda () 
			     (pretty-display (format "~a ~a \"~a\" \"~a\"" org-len res-len org res))))

      (if (and (equal? res org) (block? (linklist-entry next)))
          ;; sliding window: skip one superoptimizable unit
	  (let ([entry (linklist-entry ast)])
	    ;; change to original in case the we do memory compression
            (pretty-display "CASE 1")
	    (set-block-body! entry (block-org entry))
            (pretty-display `(set-block-body! ,(block-org entry)))
	    (superoptimize-inner (linklist-next ast)))
          (begin
            (pretty-display "CASE 2")
            (aforth-syntax-print renamed 1 1)
            (set-linklist-entry! ast renamed)
            (set-linklist-next! ast next)
            (set-linklist-prev! next ast)
            (superoptimize-inner next)))]

     [(linklist-entry ast)
      (set-linklist-entry! ast (superoptimize-inner (linklist-entry ast)))
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
    (set! ast (aforth-linklist ast))
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

    ;; (with-output-to-file #:exists 'append 
    ;;   (format "~a/~a-work.aforth" outdir name)
    ;;   (lambda () 
    ;;     (aforth-syntax-print ret w h #:id id)
    ;;     ))

    ret
    ]

   [(vector? ast)
    (define n (vector-length ast))
    (define output-programs (make-vector n))
  
    ;(vector-set! output-programs (sub1 n) (vector-ref ast (sub1 n)))

    (for ([i (in-range n)])

         (with-output-to-file #:exists 'append 
           (format "~a/~a-work.rkt" outdir name)
           (lambda ()
             (pretty-display 
              (format ";;;;;;;;;;;;;;;;;;;;;;;; ~a ;;;;;;;;;;;;;;;;;;;;;;;;;;" i))))

         (pretty-display 
          (format ";;;;;;;;;;;;;;;;;;;;;;;; ~a ;;;;;;;;;;;;;;;;;;;;;;;;;;" i))

         ;; set id for printing
         (set! id i)

         (let* ([program (aforth-linklist (vector-ref ast i))]
                [result (superoptimize-inner program)])
           (vector-set! output-programs i result)))
    
    
    output-programs]

   [else
    (raise (format "arrayforth: superoptimize: unimplemented for ~a" ast))]))

(define (renameindex ast mem-size index-map)
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
    (pretty-display `(compare ,new-body ,org mem-size ,new-mem-size bit))
    (define diff (program-diff? org new-body
                                new-mem-size (out-space (block-out ast) (block-cnstr ast)) max-bit))
    
    (if diff
        (begin
          (pretty-display "VALIDATE: different")
          (pretty-display (string-join new-body))
          (pretty-display org))
        (pretty-display "VALIDATE: same"))
    
    (if diff
        (new-block org (block-in ast) (block-out ast) (block-cnstr ast) org)
        (new-block (string-join new-body) (block-in ast) (block-out ast) (block-cnstr ast) (block-org ast))))
  
  (if index-map
      (renameindex-inner)
      (new-block (block-body ast) (block-in ast) (block-out ast) (block-cnstr ast) (block-org ast))))
