
#lang racket

(require "header.rkt"
         "parser.rkt"
         "ast-util.rkt"
         "partitioner.rkt" 
         "layout-sa.rkt" 
         "separator.rkt"
         "arrayforth.rkt"
         "arrayforth-print.rkt"
         "visitor-desugar.rkt"
         "visitor-printer.rkt"
         "visitor-linker.rkt" 
         "visitor-tempinsert.rkt" 
         "visitor-desugar.rkt"
         "visitor-memory.rkt"
         "visitor-codegen.rkt")

(provide compile test-simulate parse)

;; Parse HLP from file to AST
(define (parse file)
  ;(define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (define need-temp (send my-ast accept (new linker%)))
  ;(when need-temp
    (send my-ast accept (new temp-inserter%))
    (send my-ast accept (new desugar%))
    ;)
  my-ast)

;; Compile IR to machine code.
(define (generate-code program i w h virtual)
  (pretty-display `(-------------------- ,i -----------------------))
  (pretty-display program)
  (let* ([data-iter (send program accept (new memory-mapper%))]
         [code-gen (new code-generator% [data-size (car data-iter)]
                        [iter-size (cdr data-iter)]
                        [core i] [w w] [h h] [virtual virtual])])
    (pretty-display ">>> code gen")
    (define res (send program accept code-gen))
    (pretty-display ">>> result")
    (codegen-print res)
    res))

;; Compile per-core IRs to per-core machine codes.
(define (generate-codes programs w h virtual)
  (define machine-codes (make-vector (add1 (* w h))))
  (for ([i (in-range (add1 (* w h)))])
    (let ([program (vector-ref programs i)])
      (vector-set! machine-codes i (generate-code program i w h virtual))))
  machine-codes)


;; Compile HLP read from file to per-core machine codes.
(define (compile-to-IR file name capacity input [w 5] [h 4] 
                       #:verbose [verbose #t])
  
  (define n (* w h))
  (define my-ast (parse file))
  (define concise-printer (new printer% [out #t]))
  
  ;; generate sequantial simulation code
  (when input (simulate-onecore my-ast name input))
  
  (when verbose
    (pretty-display "--- before partition ---")
    (send my-ast pretty-print))
  
  ;; partition
  (define partition (optimize-comm my-ast
                                   #:name name
                                   #:cores (* w h) 
                                   #:capacity capacity 
                                   #:verbose #t))
  (when verbose
    (pretty-display "--- after partition ---")
    (send my-ast pretty-print))
  
  ;; layout
  (define layout-res (layout my-ast
                             n w h name))
  
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
  (aforth-syntax-info w h 0)
  (aforth-syntax-print (generate-code my-ast core w h #f)))

;; compile HLP to optimized one-core machine code.
;; for testing only.
(define (compile-and-optimize-percore file core w h)
  (pretty-display "------------------ AST ----------------------")
  (define program (parse file))
  (send program pretty-print)

  (pretty-display "------------------ RAW CODE ----------------------")
  (define real-code (generate-code program 0 w h #f))
  ;(codegen-print real-code)

  (pretty-display "------------------ REDUCED CODE ----------------------")
  (define virtual-code (generate-code program 0 w h #t))
  (codegen-print virtual-code)

  (pretty-display "------------------ OPT REDUCED CODE ----------------------")
  (define virtual-opt (superoptimize virtual-code "name"))
  (codegen-print virtual-opt)

  (pretty-display "------------------ OPT CODE ----------------------")
  (define real-opt (renameindex virtual-opt))
  (codegen-print real-opt)
  
  (aforth-syntax-info w h)
  (aforth-syntax-print real-opt)
  )
  
;; compile HLP to optimized many-core machine code
(define (compile-and-optimize file name capacity input 
                              #:w [w 5] #:h [h 4] 
                              #:verbose [verbose #f]
                              #:opt [opt #t])
  (define programs (compile-to-IR file name capacity input w h #:verbose verbose))

  (define real-codes (generate-codes programs w h #f))
  (define real-opts real-codes)
  
  
  (with-output-to-file #:exists 'truncate (format "~a/~a-gen.rkt" outdir name)
    (lambda () (aforth-struct-print real-codes)))
  
  ;; arrayforth without superoptimization
  (with-output-to-file #:exists 'truncate (format "~a/~a-noopt.aforth" outdir name)
    (lambda ()
      (aforth-syntax-info w h)
      (aforth-syntax-print real-codes)))
  
  (when opt
    (define virtual-codes (generate-codes programs w h #t))
    
    (with-output-to-file #:exists 'truncate (format "~a/~a-gen-red.rkt" outdir name)
      (lambda () (aforth-struct-print virtual-codes)))
    
    (system (format "rm ~a/~a-work.rkt" outdir name))
    
    (define virtual-opts (superoptimize virtual-codes name))
    (set! real-opts (renameindex virtual-opts))
    
    (with-output-to-file #:exists 'truncate (format "~a/~a-opt-red.rkt" outdir name)
      (lambda () (aforth-struct-print virtual-opts)))
    (with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" outdir name)
      (lambda () (aforth-struct-print real-opts))))
  
  ;; superoptimized arrayforth
  (with-output-to-file #:exists 'truncate (format "~a/~a.aforth" outdir name)
    (lambda ()
      (aforth-syntax-info w h)
      (aforth-syntax-print real-opts)))
  )

;(compile-to-IR "../examples/array.cll" "array" 256 "null" 4 5 #:verbose #t)
;(compile-to-IR "../tests/run/md5-noio.cll" "md5noio" 
;               480 "null" 7 6 #:verbose #t)
;(compile-and-optimize "../tests/run/while-noio.cll" "whilenoio" 
;                      256 "null" #:opt #f)
;(compile-and-optimize "../tests/run/offset-noio.cll" "offsetnoio" 
;                      256 "null" #:opt #t)
(compile-and-optimize "../tests/run/function-noio.cll" "functionnoio" 
                      256 "null" #:opt #t)
;(compile-and-optimize "../tests/run/md5-noio.cll" "md5noio" 
;                      600 "null" #:w 10 #:h 5 #:opt #t)

;(compile-percore "../examples/add.cll" 0 2 2)
;(compile-and-optimize-percore "../examples/add.cll" 0 2 2)

(define testdir "../tests/run")

(define (test-simulate name input capacity)
  (compile-to-IR (format "~a/~a.cll" testdir name) name capacity input)
  (pretty-display (format "running ~a ..." name))
  (define diff (simulate-multicore name input))
  
  (cond
    [(= diff 0) "PASSED"]
    [(= diff 1) "FAILED"]
    [(= diff 2) "NOT-FOUND"]))
