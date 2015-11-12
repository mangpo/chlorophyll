#lang racket

(require "header.rkt"
         "arrayforth.rkt"
         "arrayforth-print.rkt")

(provide arrayforth-basic-optimize remove-b)

(define id 0)
(define w #f)
(define h #f)

(define inlined-functions #f) ;; name -> body
(define function-defs #f) ;; body string -> name
(define function-renames #f) ;; old name -> new name
(define new-main #f) ;;TODO: use 'function-renames' instead of 'new-main'
(define node #f)
(define used-ports #f)

(define function-register-usage #f) ;; maps function names to register structs

(define (arrayforth-basic-optimize code w_ h_) ;;TODO: rename arrayforth-basic-optimize
  (define optimized #f)
  (define (reset)
    (set! inlined-functions (make-hash))
    (set! new-main #f)
    (set! function-defs (make-custom-hash aforth-eq?))
    (set! function-renames (make-hash))
    (set! function-register-usage (make-hash))
    (set! used-ports (make-hash)))
  (if code
      (begin
        (set! w w_)
        (set! h h_)
        (list->vector
         (for/list ((a code)
                    (id (range (* w h))))
           (set! node (core-id id w))
           (reset)
           (scan a)
           (aforth-syntax-print a w h)
           (set! optimized (opt a))
           (aforth-syntax-print optimized w h)
           (pretty-display optimized)
           optimized)))
      code))


(define (remove-b code w_ h_)
  (if code
      (begin
        (set! w w_)
        (set! h h_)
        (list->vector
         (for/list ((a code)
                    (id (range (* w h))))
           (set! node (core-id id w))
           (opt a #t))))
      code))

(define (linklist-count rest [n 0])
  ;; count non-false items
  (if rest
      (linklist-count (linklist-next rest) (+ n (if (linklist-entry rest) 1 0)))
      n))

(define (linklist-print ll)
  (define (print x)
    (when x
      (print (linklist-next x))))
  (print ll))

(define (linklist-first-nonfalse ll)
  ;; return the first item in LL that is not false
  (when linklist-first-nonfalse
    (if (linklist-entry ll)
        (linklist-entry ll)
        (linklist-first-nonfalse (linklist-next ll)))))

(define (list->linklist lst)
  (define (copy x)
    (if (block? x)
        (block (block-body x) (block-in x) (block-out x)
               (block-cnstr x) (block-incnstr x) (block-org x))
        x))

  (define (inner lst)
    (if (empty? lst)
        (linklist #f #f #f)
        (let* ([rest (inner (cdr lst))]
               [me (linklist #f (copy (car lst)) rest)])
          (when rest
            (set-linklist-prev! rest me))
          me)))

  (define non-empty (filter (lambda (x) (not (empty-block? x))) lst))
  (define start (inner non-empty))
  (define head (linklist #f #f start))
  (set-linklist-prev! start head)
  head)

(define (linklist->list lst)
  (define (inner lst)
    (if (linklist-entry lst)
	(cons (linklist-entry lst) (inner (linklist-next lst)))
	(if (linklist-next lst)
	    (raise "Invalid linklist: inproper tail.")
	    (list))))

  (if (linklist-entry lst)
      (raise "Invalid linklist: no head.")
      (inner (linklist-next lst))))

(define (add-port p)
  (if (hash-has-key? used-ports p)
      (hash-set! used-ports p (add1 (hash-ref used-ports p)))
      (hash-set! used-ports p 1)))

(define (scan ast)
  (cond
   [(linklist? ast)
    (scan (linklist-entry ast))
    (scan (linklist-next ast))
    ]

   [(forloop? ast)
    (scan (forloop-init ast))
    (scan (forloop-body ast))
    ]

   [(ift? ast)
    (scan (ift-t ast))
    ]

   [(iftf? ast)
    (scan (iftf-t ast))
    (scan (iftf-f ast))
    ]

   [(-ift? ast)
    (scan (-ift-t ast))
    ]

   [(-iftf? ast)
    (scan (-iftf-t ast))
    (scan (-iftf-f ast))
    ]

   [(funcdecl? ast)
    (define body (funcdecl-body ast))
    (define name (funcdecl-name ast))
    (define body-str (with-output-to-string (lambda ()
                                              (aforth-syntax-print body w h))))
    (if (dict-has-key? function-defs body-str)
        (begin
          (printf "renaming: ~a ==> ~a\n" name (dict-ref function-defs body-str))
          (hash-set! function-renames name (dict-ref function-defs body-str)))
        (dict-set! function-defs body-str name))
    (define x (linklist-first-nonfalse body))
    ;;`find-new-main' is used to find the name of the function
    ;; that will be the new main. for example:
    ;;
    ;;   : 1while down b! @b dup right b! !b 1if 1while ;
    ;;   : edge 1while ;
    ;;   : main edge ;
    ;;   ==>
    ;;   : main down b! @b dup right b! !b 1if 1while ;
    (define (find-new-main name)
      (if (hash-has-key? inlined-functions name)
          (let* ((x (hash-ref inlined-functions name))
                 (first (and x (linklist-first-nonfalse x))))
            (if (and (= (linklist-count x) 1)
                     (funccall? first))
                (find-new-main (funccall-name first))
                name))
          name))
    (when (and (= (linklist-count body) 1)
               (funccall? x))
      (if (equal? name "main")
          (begin
            ;; save the name of the new main function.
            (set! new-main (find-new-main (funccall-name x)))
            ;; set main to be inlined so that the definition will be removed
            (hash-set! inlined-functions "main" #t))
          (hash-set! inlined-functions (funcdecl-name ast) (funcdecl-body ast))))
    (scan (funcdecl-body ast))
    ]

   [(vardecl? ast)
    ]

   [(aforth? ast)
    (scan (aforth-code ast))
    ]

   [(vector? ast)
    (for ([i (* w h)])
      (set! id i)
      (scan (vector-ref ast i)))
    ]

   [(list? ast)
    (for ((x ast))
      (scan x))
    ]
   ))

(define (get-inline-body name)
  ;; recursively look for inlined function body
  (define body (and (hash-has-key? inlined-functions name)
                    (hash-ref inlined-functions name)))
  (define first (and (linklist? body) (linklist-first-nonfalse body)))
  (or (and body first
           (= (linklist-count body) 1)
           (funccall? first)
           (get-inline-body (funccall-name first)))
      body))

(define (replace-zeros code)
  ;; 0 -> dup dup or
  (define ret '())
  (define was-string? #f)
  (when (string? code);;TODO: why are some of them strings???
    (set! code (string-split code))
    (set! was-string? #t))
  (for ((x code))
    (set! ret (if (equal? x "0")
                  (cons "or" (cons "dup" (cons "dup" ret)))
                  (cons x ret))))
  (if was-string?
      (string-join (reverse ret))
      (reverse ret)))

(define (remove-b-reg body org)
  (define str-b? (string? body))
  (define str-o? (string? body))
  (define body_l (if str-b? (string-split body) body))
  (define org_l (if str-o? (string-split org) org))
  (define (find a b)
    (if (null? a)
        #f
        (if (equal? (car a) "b!")
            (car b)
            (find (cdr a) (cdr b)))))

  ;;TODO: combine find and rm-b into a single pass
  (define (rm-b code)
    (if (null? code)
        code
        (if (and (not (null? (cdr code))) (equal? (cadr code) "b!"))
            (rm-b (cddr code))
            (cons (car code) (rm-b (cdr code))))))

  (define body-val (find body_l (cons #f body_l)))
  (define org-val (find org_l (cons #f org_l)))
  (if (and body-val (equal? body-val org-val))
        (cons (if str-b? (string-join (rm-b body_l)) (rm-b body_l))
              (if str-o? (string-join (rm-b org_l)) (rm-b org_l)))
      (cons body org)))

(define (opt ast [rm #f])
  (cond
   [(linklist? ast)
    (linklist (linklist-prev ast)
              (opt (linklist-entry ast) rm)
              (opt (linklist-next ast) rm))
    ]

   [(block? ast)
    ;; 0 ==> dup dup or
    (define replacements (if (and rm (not (block-rm ast)))
                             (remove-b-reg (block-body ast) (block-org ast))
                             (cons (block-body ast) (block-org ast))))
    (block (if rm
               (car replacements)
               (replace-zeros (block-body ast)))
           (block-in ast)
           (block-out ast)
           (block-cnstr ast)
           (block-incnstr ast)
           (if rm
               (cdr replacements)
               (replace-zeros (block-org ast))))
    ]

   [(forloop? ast)
    (forloop (opt (forloop-init ast) rm)
             (opt (forloop-body ast) rm)
             (opt (forloop-iter ast) rm)
             (opt (forloop-from ast) rm)
             (opt (forloop-to ast) rm))
    ]

   [(ift? ast)
    (ift (opt (ift-t ast) rm))
    ]

   [(iftf? ast)
    (iftf (opt (iftf-t ast) rm)
          (opt (iftf-f ast) rm))
    ]

   [(-ift? ast)
    (-ift (opt (-ift-t ast) rm))
    ]

   [(-iftf? ast)
    (-iftf (opt (-iftf-t ast) rm)
           (opt (-iftf-f ast) rm))
    ]

   [(mult? ast)
    ast
    ]

   [(funccall? ast)
    (if rm
        (funccall (funccall-name ast))
        (let* ((name (funccall-name ast))
               (body (get-inline-body name)))
          (cond ((equal? name new-main)
                 (set! name "main"))
                ((hash-has-key? function-renames name)
                 (set! name (hash-ref function-renames name))))
          (or body (funccall name))))
    ]

   [(funcdecl? ast)
    (define name (funcdecl-name ast))
    (funcdecl (if (and (not rm)
                       (equal? name new-main)) "main" name)
              (opt (funcdecl-body ast) rm)
              (funcdecl-simple ast))
    ]

   [(vardecl? ast)
    (vardecl (vardecl-val ast))
    ]

   [(aforth? ast)
    (if rm
        (aforth (opt (aforth-code ast) rm) (aforth-memsize ast) (aforth-bit ast)
                (aforth-indexmap ast) (aforth-a ast) (aforth-position ast))
        (let ((code '()))
          (for ((a (linklist->list (aforth-code ast))))
            ;; discard function definitions that are inlined
            (if (and (funcdecl? a)
                     (or (get-inline-body (funcdecl-name a))
                         (hash-has-key? function-renames (funcdecl-name a))))
                (printf "discarding inlined funcdecl for: ~a\n" (funcdecl-name a))
                (set! code (cons (opt a rm) code))))
          (aforth (list->linklist (reverse code)) (aforth-memsize ast)
                  (aforth-bit ast) (aforth-indexmap ast) (aforth-a ast)
                  (aforth-position ast))))
    ]

   [(vector? ast)
    (list->vector (for/list ([i (* w h)])
                    (set! id i)
                    (opt (vector-ref ast i) rm)))
    ]

   [(list? ast)
    (for/list ((x ast))
      (opt x rm))
    ]

   [else ast]))
