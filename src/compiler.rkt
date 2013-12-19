
#lang racket

(require "header.rkt"
	 "arrayforth-def.rkt"
         "arrayforth-optimize.rkt"
         "arrayforth-print.rkt"
         "arrayforth.rkt"
         "ast-util.rkt"
         "layout-sa.rkt" 
         "optimize-distributor.rkt"
	 "parser.rkt"
         "partitioner.rkt" 
         "separator.rkt"
         "visitor-arrayaccess.rkt"
         "visitor-codegen.rkt"
         "visitor-desugar.rkt"
         "visitor-linker.rkt" 
         "visitor-loopopt.rkt"
	 "visitor-mapreduce.rkt"
         "visitor-memory.rkt"
         "visitor-printer.rkt"
         "visitor-regalloc.rkt"
         "visitor-tempinsert2.rkt" 
         "visitor-tempinsert.rkt" 
         )

(provide compile test-simulate parse compile-to-IR compile-and-optimize)

;; Parse HLP from file to AST
(define (parse file)
  ;(define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (pretty-display "=============== before map-reduce ===============")
  (send my-ast pretty-print)
  (send my-ast accept (new mapper-reducer%))
  (pretty-display "=============== after map-reduce ===============")
  (send my-ast pretty-print)
  (send my-ast accept (new linker%))
  (pretty-display "=============== after link ===============")
  (send my-ast pretty-print)
  (send my-ast accept (new temp-inserter%))
  (pretty-display "=============== after temp-insert ===============")
  (send my-ast pretty-print)
  (send my-ast accept (new desugar%))
  (pretty-display "=============== after desugar ===============")
  (send my-ast pretty-print)
  my-ast)

;; Compile IR to machine code.
(define (generate-code program i w h virtual)
  (pretty-display `(-------------------- ,i -----------------------))
  (send program pretty-print)

  ;; mark forloop and array for optimization
  (pretty-display ">>> arrayaccess >>>")
  (send program accept (new arrayaccess%))

  ;; registor allocatoin (optional)
  (pretty-display ">>> registor allocation >>>")
  (send program accept (new registor-allocator%))
  (send program pretty-print)
  
  (pretty-display ">>> memory-mapper >>>")
  (define data-iter (send program accept (new memory-mapper%)))
  (send program pretty-print)
  (let* (;; only generated reduced version if mem > 5
         [reduce (and virtual (> (+ (meminfo-addr (car data-iter)) (cdr data-iter)) 
                                 reduce-limit))]
         [code-gen (new code-generator% [data-size (car data-iter)]
                        [iter-size (cdr data-iter)]
                        [core i] [w w] [h h] [virtual reduce])])
    (pretty-display ">>> code gen")
    (define res (send program accept code-gen))
    (pretty-display ">>> result")
    (codegen-print res)
    res
    ;; (pretty-display ">>> repeating def")
    ;; (pretty-display virtual)
    ;; (define concise (define-repeating-code res))
    ;; concise
    ))

;; Compile per-core IRs to per-core machine codes.
(define (generate-codes programs w h virtual)
  (define machine-codes (make-vector (add1 (* w h))))
  (for ([i (in-range (add1 (* w h)))])
    (let ([program (vector-ref programs i)])
      (vector-set! machine-codes i (generate-code program i w h virtual))))
  machine-codes)


;; Compile HLP read from file to per-core machine codes.
(define (compile-to-IR file name capacity input [w 5] [h 4] 
                       #:verbose [verbose #t]
                       #:run [run #f]
		       #:weight [weight #t]
		       #:partition [syn #t])
  
  ;; Create output directory at the same level as input file.
  (set-outdir (string-append
	       (substring file 0 (cdr (last (regexp-match-positions* #rx"/" file))))
	       name))
  (system (format "mkdir ~a" outdir))

  (define n (* w h))
  (define my-ast (parse file))
  (define concise-printer (new printer% [out #t]))
  
  ;; generate sequantial simulation code
  (when run (simulate-onecore my-ast name input))
  
  (when verbose
    (pretty-display "--- before partition ---")
    (send my-ast pretty-print))
  
  ;; partition
  (define partition (optimize-comm my-ast
                                   #:name name
                                   #:cores (* w h) 
                                   #:capacity capacity 
                                   #:verbose #t
				   #:synthesis syn))
  
  (when verbose
    (pretty-display "--- after partition ---")
    (send my-ast pretty-print))
  
  (send my-ast accept (new temp-inserter2% [replace-all #f]))
  
  (when verbose
    (pretty-display "--- after temp insert2 ---")
    (send my-ast pretty-print))
  
  ;; layout
  (define layout-res (layout my-ast n w h name weight))
  
  (when verbose
    (pretty-display "--- after layout ---"))

  ;; generate multicore ASTs and output equivalent cpp simuation code to file
  (define programs (sep-and-insertcomm name my-ast w h 
                                       (layoutinfo-routes layout-res)
                                       (layoutinfo-part2core layout-res)
                                       #:verbose #t))

  programs)



;; Compile per-core HLP read from file to machine code.
(define (compile-percore file core w h)
  (define my-ast (parse file))
  ;(send my-ast pretty-print)
  (aforth-syntax-print (generate-code my-ast core w h #f) w h))

;; compile HLP to optimized one-core machine code.
;; for testing only.
(define (compile-and-optimize-percore file name core w h)

  (pretty-display "------------------ AST ----------------------")
  (define program (parse file))
  (send program pretty-print)

  (pretty-display "------------------ RAW CODE ----------------------")
  (define real-code (generate-code program 0 w h #f))
  ;(codegen-print real-code)

  (pretty-display "------------------ REDUCED CODE ----------------------")
  (define virtual-code (generate-code program 0 w h #t))
  (codegen-print virtual-code)

  (pretty-display "------------------ OPT CODE ----------------------")
  (define opt (superoptimize virtual-code "name" w h #t outdir))
  (codegen-print opt)
  
  (aforth-syntax-print opt w h)
  )
  
;; compile HLP to optimized many-core machine code
(define (compile-and-optimize file name capacity input 
                              #:w [w 5] #:h [h 4] 
                              #:verbose [verbose #t]
                              #:opt [opt #t] 
			      #:layout [layout #t] 
			      #:partition [xxx #t]
                              #:sliding [sliding #t]
			      #:run [run #f])
    
  (define programs (compile-to-IR file name capacity input w h #:verbose verbose 
				  #:weight layout #:partition xxx))
  (when run
	(pretty-display (format "running ~a ..." name))
	(simulate-multicore name input))

  (define real-codes (generate-codes programs w h #f))
  
  (with-output-to-file #:exists 'truncate (format "~a/~a-gen1.rkt" outdir name)
    (lambda () (aforth-struct-print real-codes)))
  
  (define shorter-codes (define-repeating-codes real-codes w h))
  (define real-opts shorter-codes)
  
  (with-output-to-file #:exists 'truncate (format "~a/~a-gen2.rkt" outdir name)
    (lambda () (aforth-struct-print shorter-codes)))
  
  ;; arrayforth without superoptimization
  (with-output-to-file #:exists 'truncate (format "~a/~a-noopt1.aforth" outdir name)
    (lambda ()
      (aforth-syntax-print real-codes w h)))
  
  ;; arrayforth without superoptimization
  (with-output-to-file #:exists 'truncate (format "~a/~a-noopt2.aforth" outdir name)
    (lambda ()
      (aforth-syntax-print shorter-codes w h)))
  
  (when opt
    ;; genreate reduced code
    (define virtual-codes (define-repeating-codes (generate-codes programs w h #t) w h))
    
    (define start (current-seconds))
    (if distributed
        (distribute-and-optimize virtual-codes name w h sliding)
        (begin
          (with-output-to-file #:exists 'truncate (format "~a/~a-gen-red.rkt" outdir name)
                               (lambda () 
                                 (print-header)
                                 (aforth-struct-print virtual-codes)
                                 (print-optimize name w h sliding)))
          ;; superoptimize
          (set! real-opts (superoptimize virtual-codes name w h sliding outdir))
          (with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" outdir name)
                               (lambda () (aforth-struct-print real-opts)))
          ;; superoptimized arrayforth
          (with-output-to-file #:exists 'truncate (format "~a/~a.aforth" outdir name)
                               (lambda ()
                                 (aforth-syntax-print real-opts w h)))))
    (pretty-display (format "optimizing time: ~a s" (- (current-seconds) start)))
    (with-output-to-file #:exists 'append (format "~a/~a.time" outdir name)
      (lambda ()
        (pretty-display (format "optimizing time: ~a s" (- (current-seconds) start)))))
    )
  
  )

(define testdir "../tests/run")

(define (test-simulate file name input capacity w h partition)
  (compile-to-IR file name capacity input w h
                 #:run #t #:partition partition)
  (pretty-display (format "running ~a ..." name))
  (define diff (simulate-multicore name input))
  
  (cond
    [(= diff 0) "PASSED"]
    [(= diff 1) "FAILED"]
    [(= diff 2) "NOT-FOUND"]))
