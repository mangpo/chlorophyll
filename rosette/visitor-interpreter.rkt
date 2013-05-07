#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "parser.rkt" "visitor-interface.rkt" "space-estimator.rkt" 
         "symbolic-dict.rkt")

(provide (all-defined-out))

(define debug #t)

(struct comminfo (msgs placeset firstast))

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field places
                [env (make-hash)] 
		[known-stack (list #t)])
    
    ;; find actual place for @place(exp)
    (define (find-place ast [modify #t])
      (when (not (is-a? ast Livable%))
        (raise "find-place: ast is not Livable%"))
      
      (define place-exp (get-field place ast))

      (define (derive-place p)
        (cond
         [(is-a? p Var%) 
          (car (lookup env p))]
         
         [(is-a? p Array%)
          (car (lookup env p))]
         
         [else
          (raise-syntax-error 'unsupported (format "place(~a) at src: l: ~a c: ~a"
                                                   (send p to-string)
                                                   (send p get-line)
                                                   (send p get-col)))]))

      (if (is-a? place-exp Place%)
          (let ([place (derive-place (get-field at place-exp))])
            ;(pretty-display (format "derive = ~a" place))
            (when modify (set-field! place ast place))
            place)
          place-exp))
    
    ;; find index of @place(x[i]) if x is distributed.
    (define (find-index place)
      (if (is-a? place Place%)
          (let* ([at (get-field at place)]
                 [index (if (is-a? at Array%)
                            ; return i when place = @place(x[i])
                            (get-field index at)
                            ; return i when place = @place(i)
                            at)])
            (if (is-a? index Var%)
                ; need to set known-type to match with the environment
                (let ([known-type (cdr (lookup env index))])
                  (set-field! known-type index known-type)
                  ; return index
                  index)
                (raise "Place expression cannot be more complicated than @place(x[i])")))         
          ; return false otherwise
          #f))

    ;; find place-type for @place(x)
    (define (find-place-type ast native)
      (when (not (is-a? native Livable%))
        (raise "find-place-type: native is not Livable%"))
      
      ;(pretty-display (format "find-place-type ~a" (send ast to-string)))
      
      (define place-type (get-field place-type ast))
      (if (or (number? place-type) (pair? place-type))
          ; place-type is infered during abstract interpretation
          ; and the format is right, we can return right away
          place-type
          (begin
            ; get index for @place(i) before place because (find-place) will remove that info
            (define index (find-index (get-field place native)))
            ;(when index
            ;  (pretty-display (format "index = ~a\nknown = ~a"
            ;                    (send index to-string) (get-field known-type index))))
            (define place (find-place native))
            (if (number? place)
                place
                (cons place index)))))

    ;; env dictionary map variable to (place . known-type)
    ;; place has 2 categories
    ;; 1) single place => integer
    ;; 2) place list   => (list_of_RangePlace%)

    ;; place-type has 2 categories
    ;; 1) single place => integer
    ;; 2) place list & index => (list_of_RangePlace% . index)

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place add-space)
      ;(assert (or (number? place) (list? place)))
      (if (number? place)
          (cores-inc-space places place add-space)
          (for ([p place])
               (cores-inc-space places (get-field place p) add-space))))
    
    ;;; Increase the used space of "place" with op.
    (define (inc-space-with-op place op)
      (assert (or (number? place) (list? place)))
      ;TODO: change to (cores-add-op places place op)
      (if (number? place)
          (cores-inc-space places place (est-space op))
          (for ([p place])
               (cores-inc-space places (get-field place p) (est-space op)))))
    
    ;; Increase the used space of places in the given set by "add-space".
    ;; Used in body inside for and if.
    (define (inc-space-placeset placeset add-space)
      (define (loop placelist)
        (when (not (empty? placelist))
          (let* ([others (cdr placelist)]
                 [me (car placelist)])
            (loop others)
            (when (andmap (lambda (x) (not (equal? x me))) others) (inc-space me add-space)))))
        
      (loop (set->list placeset)))

    ;;; Count number of message passes. If there is a message pass, it also take up more space.
    (define (count-msg x-ast y-ast)
      ;(assert (and (is-a? x-ast Base%) (is-a? y-ast Base%)))

      (define (place-type? p)
	(or (number? p) (place-type-dist? p)))

      (define (place-type-dist? p)
        (and (pair? p) (and (and (list? (car p)) (is-a? (cdr p) Base%)))))
      
      ;; Return the place that a resides if a only lives in one place. 
      ;; Otherwise, return string representation of a.
      (define (get-str-rep a)
        ;(assert (place-type? a))
	(if (number? a)
	    a
            (let ([place-list (car a)]
                  [index (cdr a)])
              (if (= (length place-list) 1)
                  (get-field place (car place-list))
                  (format "~a . ~a" 
                          (place-list-to-string place-list)
                          (send index to-string))))))

      ;; Add comm space to cores
      (define (add-comm x)
	;(assert (place-type? x))
        (if (number? x)
             (inc-space x est-comm)
             (for ([p (car x)])
                  (inc-space (get-field place p) est-comm))))

      (define (same-place? a b)
	;(assert (and (place-type? a) (place-type? b)))
       
        (if (and (number? a) (number? b))
            (equal? a b)
            (if (and (place-type-dist? a) (place-type-dist? b))
                (let ([a-list (car a)]
                      [b-list (car b)]
                      [a-index (cdr a)]
                      [b-index (cdr b)])
                  (and (and (equal? (length a-list) (length b-list))
                            (equal? (send a-index to-string) (send b-index to-string)))
                       (andmap (lambda (a-p b-p) (send a-p equal-rangeplace? b-p))
                               a-list b-list)))
                ; if one of them is @any
                (or (and (is-a? a Place%) (equal? (get-field at a) "any"))
                    (and (is-a? b Place%) (equal? (get-field at b) "any")))))
        )

      ;; Return 1 if it is absolutly in one place
      ;;             or the index is known.
      ;; Return number of cores p resides in otherwise.
      (define (count-comm p)
	;(assert (place-type? p))
        (cond
          [(number? p) 1]
          [(is-a? p Place%)
           (let ([at (get-field at p)])
             (if (equal? at "any")
                 1
                 (raise "count-comm doesn't support Place% that is not @any")))]
          [(pair? p)
           (if (and (get-field known-type (cdr p)) (car known-stack))
               1
               (length (car p)))]
          [else (raise "implemented for this type")]))
      
      ;; x and y in form (cons place-list known-type)
      ;(pretty-display (send x-ast to-string))
      (define x (get-field place-type x-ast))
      (define x-comm (count-comm x))
      
      ;(pretty-display (send y-ast to-string))
      (define y (get-field place-type y-ast))
      (define y-comm (count-comm y))

      (cond 
        ;[(equal? x 0) 0] ;any
        ;[(equal? y 0) 0] ;any

        ;; if x and y are the same place.
        [(same-place? x y) 0]

	;; if x and y are in a single core.
	[(and (= x-comm 1) (= y-comm 1))
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display "COMM + 1"))
	 1]

        ;; if x or y is in multiple cores.
        [else 
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display (format "COMM + ~a + ~a" x-comm y-comm)))
         (+ x-comm y-comm)])) ;; TODO: not 1 if it's not an array of statically known

    (define (lookup env ast)
      (dict-ref env (get-field name ast)
                (lambda () (lookup (dict-ref env "__up__" 
                                             (lambda () (send ast not-found-error)))
                                   ast))))

    (define (update env ast val)
      (let ([name (get-field name ast)])
        (if (dict-has-key? env name)
            (dict-set! env name val)
            (update (dict-ref env "__up__"
                              (lambda () (send ast not-found-error))) ast val))))

    (define (declare env name val)
      (dict-set! env name val))

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env)))

    (define (pop-scope)
      (set! env (dict-ref env "__up__")))
    
    (define (place-at places index ast)
      (when (empty? places)
            (send ast index-out-of-bound index))
      (let* ([current (car places)]
             [from    (get-field from current)]
             [to      (get-field to current)])
        (if (and (>= index from) (< index to))
            (get-field place current)
            (place-at (cdr places) index ast))))
        
    
    ;; (define/public (assert-capacity)
    ;;   (cores-assert places))

    ;; (define/public (display-used-space)
    ;;   (display-cores places))
    
    ;; (define/public (get-concrete-cores)
    ;;   (cores-evaluate places))

    ;; (define/public (num-cores)
    ;;   (cores-count places))
      
    (define/public (visit ast)
      (cond
       [(is-a? ast Const%)
          (raise "error")
          (define place (find-place ast))
	  (inc-space place est-num) ; increase space
          (comminfo 0 (set place) ast)]

       [(is-a? ast Num%)
	  ;(define const (get-field n ast))
          (when debug (pretty-display (format ">> Num ~a" (send ast to-string))))

	  ;(send const accept this)
          (define place-type (find-place-type ast (get-field n ast)))
	  (set-field! place-type ast place-type)

          (comminfo 0 (to-place-set place-type) ast)]
       
       [(is-a? ast Array%)
          ;; lookup place from env
          (define place-known (lookup env ast))
          (define places (car place-known))
          (define known-type (cdr place-known))
          (set-field! known-type ast known-type)

	  (define index (get-field index ast))
	  (define index-ret (send index accept this))

          (when debug (pretty-display (format ">> Array ~a" (send ast to-string))))

          (if (= (length places) 1)
              ;; Array lives in only one place
              (set-field! place-type ast (get-field place (car places)))
	      (if (is-a? index Num%)
		  ;; Know exact index
		  (set-field! place-type ast (place-at places (send index get-value) ast))
		  ;; Pair of list of possible places and index
		  (set-field! place-type ast (cons places index))))

          (inc-space places est-acc-arr) ; not accurate

          (comminfo 
           (+ (comminfo-msgs index-ret) (count-msg index ast))
           (set-union (comminfo-placeset index-ret) (to-place-set places))
           (comminfo-firstast index-ret))
          ]

       [(is-a? ast Var%)
          ;; lookup place from env
          (define place-known (lookup env ast))
          (define place (car place-known))

          ;; place can be list if var is iterator
          ;; need to call to-place-type to turn place-list into (place-list . index)
          (set-field! place-type ast (to-place-type ast place))

          (set-field! known-type ast (cdr place-known))

          (when debug (pretty-display (format ">> Var ~a" (send ast to-string))))

          ;; no space taken for now
          ;; x[i] in loop => take no space, i can be on stack
          ;(inc-space (get-field place ast) est-var) ; doesn't work with place-list

          (comminfo 0 (to-place-set place) ast)
          ]

       [(is-a? ast Op%)
          (raise "error")
          (define place (find-place ast))
	  (inc-space-with-op place (get-field op ast)) ; increase space
	  (comminfo 0 (to-place-set place) ast)
          ]


       [(is-a? ast UnaExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define op (get-field op ast))
          (define e1-ret (send e1 accept this))
	  ;(define op-ret (send op accept this))
          
          ;; set place-type known-type
          (define place-type (find-place-type ast op))
          (set-field! place-type ast place-type)
          (set-field! known-type ast (get-field known-type e1))

	  ;; already set place type

          (when debug
                (pretty-display (format ">> UnaOp ~a" (send ast to-string))))
          
          (comminfo
           (+ (comminfo-msgs e1-ret) (count-msg ast e1))
           (set-union (comminfo-placeset e1-ret) (to-place-set place-type))
           (comminfo-firstast e1-ret))
          ]

       [(is-a? ast BinExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define op (get-field op ast))
          (define e1-ret (send e1 accept this))
          (define e2-ret (send e2 accept this))
	  ;(define op-ret (send op accept this))

          
          ;; set place-type known-type
          (define place-type (find-place-type ast op))
          (set-field! place-type ast place-type)
          (set-field! known-type ast (and (get-field known-type e1) (get-field known-type e2)))
          ;; ^ problematic here (maybe not anymore)
          
          (when debug
                (pretty-display (format ">> BinOp ~a" (send ast to-string))))

          (comminfo
           (+ (+ (+ (comminfo-msgs e1-ret) (comminfo-msgs e2-ret))
                 (count-msg ast e1))
              (count-msg ast e2))
           (set-union (set-union (comminfo-placeset e1-ret) (comminfo-placeset e2-ret)) (to-place-set place-type))
           (comminfo-firstast e1-ret))
          ]

       [(is-a? ast FuncCall%)
          (when debug
                (pretty-display (format ">> FuncCall ~a" (send ast to-string))))

	  (define func (lookup env ast))
	  (define func-ast (car func))
	  (define func-ret (cdr func))

          (define msgs (comminfo-msgs func-ret))
	  (define placeset (comminfo-placeset func-ret))
	  (define firstast (comminfo-firstast func-ret))

	  (for ([param (get-field stmts (get-field args func-ast))] ; signature
		[arg   (get-field args ast)]) ; actual
	       (let ([arg-ret (send arg accept this)])
		 (set! msgs (+ msgs (+ (count-msg param arg) (comminfo-msgs arg-ret))))
		 (set! placeset (set-union placeset (comminfo-placeset arg-ret)))))


	  ;; set place-type known-type
	  (let ([return (get-field return func-ast)])
	    (set-field! place-type ast (get-field place return))
	    (set-field! known-type ast (get-field known return)))
		 
	  (comminfo msgs placeset firstast)]
                
       [(is-a? ast VarDecl%) 
          (define place (find-place ast))
	  (define known (get-field known ast))
          (define var-list (get-field var-list ast))

	  ;; Param% only
	  (when (is-a? ast Param%)
		(set-field! place-type ast place)
		(set-field! known-type ast known))
          
          ;; put vars into env
          (for ([var var-list])
               (declare env var (cons place known)))

          (when debug
                (pretty-display (format ">> VarDecl ~a" var-list)))

	  ;; increase space for variable
          (inc-space place (* (length var-list) est-data)) ; increase space
          (comminfo 0 (set place) ast)
          ]

       [(is-a? ast ArrayDecl%)
          (define place-list (get-field place-list ast)) ; don't support place(x)
          (define known (get-field known ast))

          (when debug
                (pretty-display (format ">> ArrayDecl ~a" (get-field var ast))))

          ;; check boundaries
          (define last 0)

          (when (list? place-list)
                (for ([p place-list])
                     (let* ([from (get-field from p)]
                            [to   (get-field to p)])
                       (when (not (= from last))
                             (send ast bound-error))
                       (set! last to)
                       (inc-space (find-place p) (* (- to from) est-data)) ; increase space
                       ))

                (when (not (= (get-field bound ast) last))
                      (send ast bound-error)))
          
          ;; We might not need this. This is a dead code right now.
          (when (and (is-a? place-list Place%) 
                     (equal? (get-field at place-list) "any"))
            (raise-type-error (get-field var ast) "anything about @any" 
                              0 "Cannot assign @any for array."))
            

          ;; put array into env
          (declare env (get-field var ast) (cons place-list known))

          (comminfo 0 (to-place-set place-list) ast)
          ]

       [(is-a? ast For%)
          (define place-list (get-field place-list ast)) ; don't allow place(x)

          (when debug
                (pretty-display (format ">> For ~a" (send (get-field iter ast) to-string))))

          ;; check boundaries
          (define last 0)
        
          (when (list? place-list)
              (for ([p place-list])
                   (let* ([from (get-field from p)]
                          [to   (get-field to p)])
                     (when (not (= from last))
                           (send ast bound-error))
                     (set! last to))))
		 ;(inc-space (get-field place p) est-for))) ; increase space

          ;; Add new scope for body.
          (push-scope)

          ;; This "for" iterating pattern is satically known.
          (declare env 
                     (get-field name (get-field iter ast))
                     (cons place-list #t))

          (define body-ret (send (get-field body ast) accept this))
          (define body-place-set (comminfo-placeset body-ret))
          
          ;(for ([p body-place-set])
          ;     (inc-space p est-for)) ; increase space (already accounts for i here)
          (inc-space-placeset body-place-set est-for)

          ;; Remove scope.
          (pop-scope)

          (comminfo
           (* (comminfo-msgs body-ret) (- (get-field to ast) (get-field from ast)))
           body-place-set
           (comminfo-firstast body-ret))
          ]

       [(is-a? ast If%)
        (define condition (get-field condition ast))
        (define condition-ret (send condition accept this))
        
	; push to stack
	(set! known-stack (cons (get-field known-type condition) known-stack))

        (push-scope)
        (define true-ret (send (get-field true-block ast) accept this))
        (pop-scope)
        
        ; between condition and true-block
        (define ret
          (comminfo
           (+ (+ (count-msg condition (comminfo-firstast true-ret))
                 (comminfo-msgs condition-ret))
              ; *2 for 1) sending condition result to body
              ;        2) communication within body themselves
              (* 2 (comminfo-msgs true-ret)))
           (set-union (comminfo-placeset condition-ret) (comminfo-placeset true-ret))
           (comminfo-firstast condition-ret)))
        
        (define false-block (get-field false-block ast))
	; pop from stack
	(set! known-stack (cdr known-stack))
        
        (push-scope)
        (set! ret 
              (let ([false-block (get-field false-block ast)])
                (if false-block
                    ; between condition and false-block
                    (let ([false-ret (send false-block accept this)])
                      (comminfo
                       (+ (comminfo-msgs ret)
                          (+ (count-msg condition (comminfo-firstast false-ret))
                             (* 2 (comminfo-msgs false-ret))))
                       (set-union (comminfo-placeset ret) (comminfo-placeset false-ret))
                       (comminfo-firstast ret)))
                    ret)))
        (pop-scope)
        
        ; increase space
        (inc-space-placeset (comminfo-placeset ret) est-if)
        ret]

       [(is-a? ast While%)
          (define condition (get-field condition ast))
          (define condition-ret (send condition accept this))
          
          ; push to stack
          (set! known-stack (cons (get-field known-type condition) known-stack))
          (define body-ret (send (get-field body ast) accept this))
	  ; pop from stack
          (set! known-stack (cdr known-stack))
          
          ; increase space
          (inc-space-placeset (comminfo-placeset body-ret) est-while)

          (comminfo
           (+ (comminfo-msgs condition-ret) 
              (* (comminfo-msgs body-ret) (get-field bound ast)))
           (set-union (comminfo-placeset condition-ret) (comminfo-placeset body-ret))
           (comminfo-firstast condition-ret))]

       [(is-a? ast Assign%) 
          (when debug (newline))
          (define lhs (get-field lhs ast))
          (define rhs (get-field rhs ast))

          (when debug
                (pretty-display ">> Assign"))
          ;; Visit lhs
          (define lhs-ret (send lhs accept this))

          (define lhs-place-type (get-field place-type lhs))
          (define lhs-known-type (get-field known-type lhs))
	  (define lhs-name (get-field name lhs))

          ;; If rhs is a number, set place to be equal to lhs
          (when (is-a? rhs Num%) (send rhs infer-place lhs-place-type))

          ;; Visit rhs
          (define rhs-ret (send rhs accept this))

          ;; Update dynamic known type
          (define rhs-known-type (get-field known-type rhs))
          (when (and (not rhs-known-type) lhs-known-type)
                (set-field! known-type lhs #f)
		(let ([old (lookup env lhs)])
		  (update env lhs (cons (car old) #f))))

	  ;; Don't increase space
       
          (comminfo
           (+ (+ (comminfo-msgs rhs-ret) (comminfo-msgs lhs-ret)) (count-msg lhs rhs))
           (set-union (comminfo-placeset rhs-ret) (comminfo-placeset lhs-ret))
           (comminfo-firstast rhs-ret))
         ]

       [(is-a? ast Block%) 
          (foldl (lambda (stmt all) 
                   (let ([ret (send stmt accept this)])
                     (comminfo (+ (comminfo-msgs all) (comminfo-msgs ret))
                           (set-union (comminfo-placeset all) (comminfo-placeset ret))
                           (or (comminfo-firstast all) (comminfo-firstast ret)))))
                   (comminfo 0 (set) #f) (get-field stmts ast))]

       [(is-a? ast FuncDecl%)
          (push-scope)
	  (define return (get-field return ast))
          (define return-ret (send return accept this))
          (define args-ret (send (get-field args ast) accept this))
          (define body-ret (send (get-field body ast) accept this))
          (pop-scope)
          
          (when debug
                (pretty-display (format ">> FuncDecl ~a" (get-field name ast))))

	  (let ([ret (comminfo (+ (+ (comminfo-msgs args-ret) (comminfo-msgs body-ret)) (comminfo-msgs return-ret))
			       (set-union (set-union (comminfo-placeset args-ret) (comminfo-placeset body-ret)) (comminfo-placeset return-ret))
			       (comminfo-firstast return-ret))])
	    ;; declare function
	    (declare env (get-field name ast) (cons ast ret))
	    ret)]
		
       [else (raise "Error: count-msg-interpreter unimplemented!")]))
))
