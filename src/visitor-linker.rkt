#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

;; Attach data type to every variable, and function signature AST to function call.
;; Interpret known/unknown type.
;; Mark array is cluster or not cluster.
(define linker%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] [array-map (make-hash)] [non-native #f] [entry #f])
    ;; env maps
    ;; 1) var-name  -> (cons type known)
    ;; 2) func-name -> func-ast

    ;; type
    ;; 1) data-type
    ;; 2) (cons data-type entry)

    ;; Declare IO function: in(), out(data)
    (declare env "in" stdin)
    (declare env "out" stdout)

    (struct val (type expand known))

    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" array-map)
        (set! array-map new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__"))
      (set! array-map (dict-ref array-map "__up__")))


    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         (define type (get-field type ast)) 
	 (when (pair? type)
	       (set! non-native #f))

         (define known (get-field known ast))  
         (for ([name (get-field var-list ast)])
           ;; declare type
           (declare env name 
		    (if (string? type)
			(val type 1 known)
			(val (car type) (cdr type) known))))
         ]
        
        [(is-a? ast ArrayDecl%)
         (define type (get-field type ast)) 
	 (when (pair? type)
	       (set! non-native #f))
         
         (define known (get-field known ast))    
         ;; declare type
         (declare env (get-field var ast) 
		  (if (string? type)
		      (val type 1 known)
		      (val (car type) (cdr type) known))
         (declare array-map (get-field var ast) ast)]
        
        [(is-a? ast Num%)
         (set-field! expect ast entry)]
        
        [(is-a? ast Array%)
	 (set-field! expect ast entry)

	 (set! entry 1)
	 (define index (get-field index ast))
         (define index-known (send index accept this))
	 (set! entry (get-field expect ast))

         (unless index-known
           (set-field! cluster (lookup array-map ast) #t))
         
         (define pack (lookup env ast))
         (define type (val-type pack))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))
	 (when (> expand entry)
	       (send ast partition-mismatch entry))

	 (set-field! type ast type)
	 (set-field! expand ast expand)
	 (and known-type index-known)]
        
        [(is-a? ast Var%)
	 (set-field! expect ast entry)

         (define pack (lookup env ast))
         (define type (val-type pack))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))
	 (when (> expand entry)
	       (send ast partition-mismatch entry))

	 (set-field! type ast type)
	 (set-field! expand ast expand)
	 known-type]

	[(is-a? ast Op%)
	 "int"]
        
        [(is-a? ast UnaExp%)
	 (set-field! expect ast entry)
	 (define e1 (get-field e1 ast))
         (define e1-known (send e1 accept this))
         (define op-type (send (get-field op ast) accept this))
	 (set-field! type ast op-type)
	 e1-known]
        
        [(is-a? ast BinExp%)
	 (set-field! expect ast entry)
	 (define e1 (get-field e1 ast))
	 (define e2 (get-field e1 ast))
         (define e1-known (send e1 accept this))
         (define e2-known (send e2 accept this))
         (define op-type (send (get-field op ast) accept this))
	 (set-field! type ast op-type)
	 (and e1-known e2-known)]
        
        [(is-a? ast FuncCall%)
         (define func-ast (lookup env ast))
         (define type (get-field type (get-field return func-ast)))

	 ;; set type and expand
	 (if (string? type)
	     (begin
	       (set-field! type ast type)
	       (set-field! expand ast 1))
	     (begin
	       (set-field! type ast (car type))
	       (set-field! expand ast (cdr type))))

	 ;; set expect
	 (if entry
	     (set-field! expect ast entry)
	     (set-field! expect ast (get-field expand ast)))

	 ;; check expand against expect
	 (when (> (get-field expand ast) (get-field expand ast))
	       (send ast partition-mismatch (get-field expand ast)))
         
         (define args (get-field args ast))
         (define params (get-field stmts (get-field args func-ast)))
         
         (define old-entry entry)
         (for ([arg args]
	       [param params])
	      (let ([param-type (get-field type param)])
		(if (string? param-type)
		    (set! entry 1)
		    (set! entry (cdr param-type)))
		(let ([arg-known (send arg accept this)])
		  (set-field! known-type param (and (get-field known-type param) arg-known)))))
	        
	 (set! entry old-entry)
	 #f
         ]
        
        [(is-a? ast Assign%)
         (define lhs (get-field lhs ast))
         (define rhs (get-field rhs ast))

	 (define lhs-known (send lhs accept this))

	 (set! entry (get-field expand lhs))
	 (define rhs-known (send rhs accept this))
	 (when (and lhs-known (not rhs-known))
	       (update env lhs (val (get-field type lhs) (get-field expand lhs) #f)))]
        
        [(is-a? ast If%)
         (set! entry 1)
         (send (get-field condition ast) accept this)

	 (push-scope)
	 (send (get-field true-block ast) accept this)
	 (pop-scope)

	 (when (get-field false-block ast)
	       (push-scope)
	       (send (get-field false-block ast) accept this)
	       (pop-scope))
         ]
        
        [(is-a? ast While%)
         (set! entry 1)
         (send (get-field condition ast) accept this)
	 (push-scope)
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [(is-a? ast For%)
	 (push-scope)
	 (declare env (get-field name (get-field iter ast)) (val "int" 1 #t))
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [(is-a? ast Program%)
         (pretty-display "DESUGAR: Program")
         (define stmts (get-field stmts ast))
         
         ;; update env front to back
         (for ([stmt stmts])
	      (if (is-a? stmt FuncDecl%)
		  (declare env (get-field name stmt) stmt)
		  (send stmt accept this)))
         
         ;; desugar back to front
         (for ([stmt (reverse stmts)])
           (when (is-a? stmt FuncDecl%)
		 (send stmt accept this)))
         ]
        
        [(is-a? ast Block%)
         (pretty-display "DESUGAR: Block")
         (for ([stmt (get-field stmts ast)])
	      (set! entry #f)
	      (send stmt accept this))))
         ]
        
        
        [(is-a? ast FuncDecl%)
	 (push-scope)
	 (send (get-field return ast) accept this)
	 (send (get-field args ast) accept this)
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [else
         (raise (format "visitor-linker: unimplemented for ~a" ast))]
        
        ))))
  
  
