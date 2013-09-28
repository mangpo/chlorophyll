#lang racket

(require "arrayforth.rkt" "header.rkt")

(struct task (sp o i e))

(define (print-file program name core w h)
  (with-output-to-file #:exists 'truncate (format "~a/~a-~a-gen-red.rkt" outdir name core)
    (lambda () 
    (pretty-display "#lang racket")
    (pretty-display "(require \"../src/header.rkt\" \"../src/arrayforth.rkt\" \"../src/arrayforth-optimize.rkt\" \"../src/arrayforth-print.rkt\")")

      (pretty-display "(define program")
      (aforth-struct-print program)
      (pretty-display ")")
      (pretty-display (format "(define name \"~a-~a\")" name core))
      (pretty-display (format "(define real-opts (superoptimize program name ~a ~a #:id ~a))" w h core))
      (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a/~a-opt.rkt\" outdir name) (lambda () (aforth-struct-print real-opts)))")
      (pretty-display "(with-output-to-file #:exists 'truncate (format \"~a/~a-opt.aforth\" outdir name)")
      (pretty-display (format "(lambda () (aforth-syntax-print real-opts ~a ~a #:id ~a)))" w h core))
      )))

(define (exec-racket name)
  (define out-port (open-output-file (format "~a/~a.log" outdir name) #:exists 'truncate))
  (let-values ([(sp o i e) 
                (subprocess out-port 
                            #f #f 
                            (find-executable-path "racket") 
                            (format "~a/~a-gen-red.rkt" outdir name))])
    (task sp out-port i e)))

(define (finish queue runnings)
  (unless (empty? runnings)
    (sync/timeout check-interval (task-sp (last runnings)))
    (let ([still (filter (lambda (t) 
                           (if (equal? (subprocess-status (task-sp t)) 'running)
                               #t
                               (begin
                                 (close-output-port (task-i t))
                                 (close-input-port (task-e t))
                                 (close-output-port (task-o t))
                                 #f)))
                         runnings)])
      (run queue still))))

(define (run queue runnings)
  (if (and (not (empty? queue)) (< (length runnings) procs))
      (run (cdr queue) (cons (exec-racket (car queue)) runnings))
      (finish queue runnings)))
	       
  
(define (distribute-and-optimize programs name w h)
  (define files
    (for/list ([i (in-range (* w h))])
	      (print-file (vector-ref programs i) name i w h)
	      (format "~a-~a" name i)))
  
  (run files (list))
  )

(define programs
  (vector
    ;; core 2
    (aforth 
      ;; linklist
      (list 
        (vardecl '())
        (funcdecl "main"
          ;; linklist
          (list 
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f) "right"
              "right b! @b ")
            (block
              "1 "
              0 1 (restrict #t #f #f #f) #f
              "1 ")
            (block
              "+ "
              2 1 (restrict #t #f #f #f) #f
              "+ ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f) "right"
              "right b! !b ")
          )
        )
      )
    0 2 #f)
    ;; core 3
    (aforth 
      ;; linklist
      (list 
        (vardecl '(0))
        (funcdecl "main"
          ;; linklist
          (list 
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f) #f
              "0 b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f) "right"
              "right b! !b ")
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f) "right"
              "right b! @b ")
            (block
              "0 b! !b "
              1 0 (restrict #t #f #f #f) #f
              "0 b! !b ")
          )
        )
      )
    1 2 #f)))

(distribute-and-optimize programs "test" 2 1)
