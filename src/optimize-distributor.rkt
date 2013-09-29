#lang racket

(require "arrayforth.rkt" "header.rkt")
(provide distribute-and-optimize)

(struct task (sp o i e))

;; Print optimizing file for each core
(define (print-file program name core w h)
  (with-output-to-file #:exists 'truncate (format "~a/~a-~a-gen-red.rkt" outdir name core)
    (lambda () 
      (print-header)
      (pretty-display "(define program")
      (aforth-struct-print program)
      (pretty-display ")")
      (print-optimize name w h core))))

(define (close-ports i e o)
  (close-output-port i)
  (close-input-port e)
  (close-output-port o))

;; Execute racket on the optimize file
(define (exec-racket name)
  (define out-port (open-output-file (format "~a/~a.log" outdir name) #:exists 'truncate))
  
  (let-values ([(sp o i e) 
                (subprocess out-port 
                            #f #f 
                            (find-executable-path "racket") 
                            (format "~a/~a-gen-red.rkt" outdir name))])

    (with-handlers* ([exn:break? (lambda (e)
                                   (close-ports i e out-port)
                                   (raise e))])
                    (task sp out-port i e))))

;; Remove terminating subprocess from the running list
(define (finish queue runnings)
  (unless (empty? runnings)
    (sync/timeout check-interval (task-sp (last runnings)))
    (let ([still (filter (lambda (t) 
                           (if (equal? (subprocess-status (task-sp t)) 'running)
                               #t
                               (begin
                                 (close-ports (task-i t) (task-e t) (task-o t))
                                 #f)))
                         runnings)])
      (run queue still))))

;; Execute more file. Remove from queue to runnings
(define (run queue runnings)
  (define (cleanup e)
    (for ([t runnings])
         (when (equal? (subprocess-status (task-sp t)) 'running)
               (subprocess-kill (task-sp t) #t))
         (close-ports (task-i t) (task-e t) (task-o t)))
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
(define (distribute-and-optimize programs name w h)
  ;; Create file for each core.
  (define files
    (for/list ([i (in-range (* w h))])
	      (print-file (vector-ref programs i) name i w h)
	      (format "~a-~a" name i)))
  
  ;; Run each core file.
  (run files (list))
  
  ;; Aggregate results.
  (with-output-to-file #:exists 'truncate (format "~a/~a.aforth" outdir name)
    (lambda () 
      (for ([i (in-range (* w h))])
        (let ([port (open-input-file (format "~a/~a-~a-opt.aforth" outdir name i))])
          (read-port port)))
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
          )
        )
      )
    0 2 #f)
    ;; core 3
    (aforth 
      ;; linklist
      (list 
        (vardecl '(0 0 0 0 0 0 0 0 0 0 0))
        (funcdecl "main"
          ;; linklist
          (list 
            (block
              "2 b! @b "
              0 1 (restrict #t #f #f #f) #f
              "10 b! @b ")
          )
        )
      )
    3 2 #hash((2 . 10) (3 . 11)))))

;(distribute-and-optimize programs "test" 2 1)
