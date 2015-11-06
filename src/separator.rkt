#lang racket

(require "header.rkt" 
         "visitor-comminsert.rkt" 
         "visitor-unroll.rkt" 
         "visitor-divider.rkt" 
         "visitor-loopopt.rkt"
         "visitor-printer.rkt"
	 "visitor-tempremove.rkt"
         "visitor-cprinter.rkt")

(provide (all-defined-out))

(define (sep-and-insertcomm name ast w h routing-table part2core #:verbose [verbose #f])

  (define concise-printer (new printer% [out #t]))
  (define cprinter (new cprinter% [thread #t] [w w] [h h]))
  (define n (add1 (* w h)))

  (define (print-main)
    (pretty-display "int main() {")
    (display "  pthread_t")

    ;; declare pthread
    (define first #t)
    (for ([i (in-range n)])
         (unless (empty? (get-field stmts (vector-ref programs i)))
           (if first 
               (set! first #f)
               (display ","))
           (display (format " t_~a" i))))
    (pretty-display ";")
    
    ;; setup
    (pretty-display "  setup();")
  
    ;; pthread_crate
    (for ([i (in-range n)])
         (unless (empty? (get-field stmts (vector-ref programs i)))
           (pretty-display (format "  pthread_create(&t_~a, NULL, main_~a, NULL);" i i))))

    ;; pthread_join
    (for ([i (in-range n)])
         (unless (empty? (get-field stmts (vector-ref programs i)))
           (pretty-display (format "  pthread_join(t_~a, NULL);" i))))

    ;; return
    (pretty-display "  return 0;")
    (pretty-display "}"))

  (define (print-to-file programs [ext ""])
    (with-output-to-file #:exists 'truncate (format "~a/~a~a.cpp" outdir name ext)
      (lambda ()
	(pretty-display (format "#include \"~a/communication.cpp\"\n" srcdir))
	(for ([i (in-range n)])
	     (pretty-display (format "//-------------------- CORE ~a(~a,~a) ---------------------"
				     i (floor (/ i w)) (modulo i w)))
	     (send cprinter set-core i)
	     (send (vector-ref programs i) accept cprinter)
	     (newline))
	(print-main)
	)))
  
  ;; Unroll for loop according to array distributions of variables inside its body.
  ;; Note: given AST is mutated.
  ;; (define for-unroller (new loop-unroller%))
  ;; (send ast accept for-unroller)  
  ;; (when verbose
  ;;       (pretty-display "--- after unroll ---")
  ;;       ;(send ast accept concise-printer)
  ;;       (send ast pretty-print)
  ;;       )

  ;; 1) Insert communication route to send-path field.
  ;; 2) Convert partition ID to actual core ID.
  ;; Note: given AST is mutate.
  (define commcode-inserter (new commcode-inserter% 
                                 [routing-table routing-table]
                                 [part2core part2core]
                                 [w w] [h h]
                                 [obstacles (get-field noroute ast)]
                                 [actors (get-field actors ast)]))
  (send ast accept commcode-inserter)
  (when verbose
        (pretty-display "--- after insert communication ---")
        (send ast pretty-print)
	(pretty-display "--- before dividing ---")
	)

  (define divider (new ast-divider%
                       [routing-table routing-table]
                       [actors (get-field actors ast)]
                       [w w] [h h]))
  (define programs (send ast accept divider))
  (when verbose (pretty-display "--- after dividing ---"))

  (print-to-file programs "_temp")
  
  (define temp-remover (new temp-remover%))
  (define loop-optimizer (new loop-optimizer%))
  (for ([i (in-range n)])
       (send (vector-ref programs i) accept temp-remover)
       (send (vector-ref programs i) accept loop-optimizer))

  (when verbose (pretty-display "--- after removing temp & optimizing loop ---"))
  
  (print-to-file programs)
  
  programs
  )

(define (generate-onecore-simulation ast file)
  (with-output-to-file #:exists 'truncate file
    (lambda ()
      (define cprinter (new cprinter% [thread #f]))
      (pretty-display (format "#include \"~a/communication.cpp\"\n" srcdir))
      (send ast accept cprinter)))
  )

(define (simulate-onecore ast name input)
  (define cpp    (format "~a/~a_seq.cpp" outdir name))
  (define binary (format "~a/~a_seq" outdir name))
  (define expect (format "~a/out/~a_~a.out" datadir name input))
  
  (generate-onecore-simulation ast cpp)
  
  (system (format "rm -f ~a ~a" binary expect))
  (unless (= 0 (system/exit-code (format "g++ -pthread -std=c++0x ~a -o ~a" 
					 cpp
					 binary)))
	;; error
	(raise "compilation error at sequantial simulation file."))
  (pretty-display (format "GENERATE ~a" expect))
  (system (format "./~a < ~a/~a > ~a"
                  binary
                  datadir input  ;; input
                  expect)) ;; output
  )
       
(define (simulate-multicore name input)
  (define binary (format "~a/~a" outdir name))
  (define output (format "~a/out/~a_~a.tmp" datadir name input))
  (define expect (format "~a/out/~a_~a.out" datadir name input))
  
  (system (format "rm -f ~a ~a" binary output))
  (unless (= 0 (system/exit-code (format "g++ -pthread -std=c++0x ~a/~a.cpp -o ~a" 
					 outdir name 
					 binary)))
	  ;; error
	(raise "compilation error at multicore simulation file."))
  (system (format "./~a < ~a/~a > ~a"
                  binary
                  datadir input  ;; input
                  output)) ;; output

  (system/exit-code (format "diff ~a ~a" output expect)))
      
  
