#lang racket

(require "arrayforth.rkt" "header.rkt")
(require "arrayforth-superopt-api.rkt")
(provide distribute-and-optimize)

(struct task (sp o i))

;; Print optimizing file for each core
(define (print-file program name core w h sliding)
  (with-output-to-file #:exists 'truncate (format "~a/~a-~a-gen-red.rkt" outdir name core)
    (lambda () 
      (print-generic-header) ;;(print-header)
      (pretty-display "(define code") ;;(pretty-display "(define program")
      (print-generic program) ;;(aforth-struct-print program)
      (pretty-display ")")
      (print-generic-optimize name w h sliding core) ;;(print-optimize name w h sliding core)
      ))
  )

(define (close-ports i o)
  (close-output-port i)
  (close-output-port o))

;; Execute racket on the optimize file
(define (exec-racket name)
  (define out-port (open-output-file (format "~a/~a.log" outdir name) #:exists 'truncate))
  
  (let-values ([(sp o i e) 
                (subprocess out-port #f out-port
                            (find-executable-path "racket") 
                            (format "~a/~a-gen-red.rkt" outdir name))])

    (with-handlers* ([exn:break? (lambda (err)
                                   (pretty-display `(kill ,name))
                                   (subprocess-kill sp #t)
                                   (close-ports i out-port)
                                   (raise err))])
                    (task sp out-port i))))

;; Remove terminating subprocess from the running list
(define (finish queue runnings)
  (unless (empty? runnings)
    (sync/timeout/enable-break check-interval (task-sp (last runnings)))
    (let ([still (filter (lambda (t) 
                           (if (equal? (subprocess-status (task-sp t)) 'running)
                               #t
                               (begin
                                 (close-ports (task-i t) (task-o t))
                                 #f)))
                         runnings)])
      (run queue still))))

;; Execute more file. Remove from queue to runnings
(define (run queue runnings)
  (define (cleanup e)
    (for ([t runnings])
         (when (equal? (subprocess-status (task-sp t)) 'running)
               (pretty-display `(kill ,t))
               (subprocess-kill (task-sp t) #t))
         (close-ports (task-i t) (task-o t)))
    (raise "user break"))

  (with-handlers* ([exn:break? cleanup])
    (if (and (not (empty? queue)) (< (length runnings) procs))
        (run (cdr queue) (cons (exec-racket (car queue)) runnings))
        (finish queue runnings)))
  )
	       
;; Read file
(define (read-port in)
  (let ([next (read-line in)])
    (unless (eof-object? next)
      (pretty-display next)
      (read-port in))))
  
;; Create file for each core and optimize each core on a subprocess.
;; Combine result to one file.
(define (distribute-and-optimize programs name w h sliding)
  (set! programs (generic-form programs))

  ;; Create file for each core.
  (define files
    (for/list ([i (in-range (* w h))])
	      (print-file (vector-ref programs i) name i w h sliding)
	      (format "~a-~a" name i)))
  (raise "done")
  ;; Run each core file.
  (run files (list))
  
  ;; Aggregate results.
  (with-output-to-file #:exists 'truncate (format "~a/~a.aforth" outdir name)
    (lambda () 
      ;; Collect which blocks are empty
      (define empty-content (make-vector (* w h)))
      (for ([id (in-range (* w h))])
        (let ([port (open-input-file (format "~a/~a-~a-opt.aforth" outdir name id))])
          (vector-set! empty-content id (eof-object? (read-line port)))
          (close-input-port port)
          ))

      ;; Linker
      (pretty-display "{block 790}")
      (pretty-display "host target | cr")
      (for ([id (* w h)])
           (unless (vector-ref empty-content id)
                   (pretty-display (format "~a node ~a load"
                                           (core-id id w) (+ block-offset (* 2 id))))))

      ;; Loader
      (newline)
      (pretty-display "{block 792}")
      (pretty-display ": /node dup +node /ram ; | cr")
      (for ([id (* w h)])
           (unless (vector-ref empty-content id)
                   (pretty-display (format "~a /node $0 /p" (core-id id w)))))
      (newline)

      (for ([i (in-range (* w h))])
        (let ([port (open-input-file (format "~a/~a-~a-opt.aforth" outdir name i))])
          (read-port port)
          (close-input-port port)
          ))
      ))
  
  ;; Aggregate stat.
  (with-output-to-file #:exists 'truncate (format "~a/~a.stat" outdir name)
    (lambda () 
      (for ([i (in-range (* w h))])
           (let ([file (format "~a/~a-~a.stat" outdir name i)])
             (when (file-exists? file)
                   (let ([port (open-input-file file)])
                     (read-port port)))))
      ))
      
  )
