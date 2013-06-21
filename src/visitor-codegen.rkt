#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt")

(define code-generator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field memory)

    (define/public (visit ast)
      (cond
       [(or (is-a? ast VarDecl%)
	    (is-a? ast ArrayDecl%))
	(list)]

       [(is-a? ast Num%)
	(list (get-field n (get-field n ast)))]

       [(is-a? ast Array%)
	
