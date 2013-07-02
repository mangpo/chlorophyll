#lang s-exp rosette
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(require "ast.rkt" "ast-util.rkt")

(provide ast-from-string ast-from-file)
 
(define-tokens a (NUM VAR ARITHOP1 ARITHOP2 ARITHOP3 RELOP EQOP))
(define-empty-tokens b (@ NOT BAND BXOR BOR AND OR EOF 
			       LPAREN RPAREN LBRACK RBRACK LSQBR RSQBR
			       = SEMICOL COMMA COL EXT
                               INT VOID CLUSTER FOR WHILE IF ELSE FROM TO RETURN
                               -> FILTER WORK BLOCKING PIPELINE ADD
			       READ WRITE
                               PLACE HERE ANY))

(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))

(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

(define-lex-abbrevs
  (comment (re-: "/*" (complement (re-: any-string "*/" any-string)) "*/"))
  (line-comment (re-: "//" (re-* (char-complement #\newline)) #\newline))
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (arith-op1 (re-or "*" "/" "%"))
  (arith-op2 (re-or "+" "-" "~"))
  (arith-op3 (re-or "<<" ">>"))
  (rel-op (re-or "<" "<=" ">=" ">"))
  (eq-op (re-or "==" "!="))
  (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
  (identifier-characters-ext (re-or "_" digit10 identifier-characters))
  (identifier (re-seq identifier-characters
                      (re-* identifier-characters-ext))))
  
(define greensyn-lexer
  (lexer-src-pos
   ("int"   (token-INT))
   ("void"  (token-VOID))
   ("return" (token-RETURN))
   ;; ("known" (token-KNOWN))
   ("cluster" (token-CLUSTER))
   ("#read"  (token-READ))
   ("#write" (token-WRITE))
   ("for"   (token-FOR))
   ("while" (token-WHILE))
   ("if"    (token-IF))
   ("else"  (token-ELSE))
   ("from"  (token-FROM))
   ("to"    (token-TO))
   ("place" (token-PLACE))
   ("here"  (token-HERE))
   ("any"   (token-ANY))
   ("@" (token-@))
   ("->" (token-->))
   ("filter" (token-FILTER))
   ("work" (token-WORK))
   ("blocking" (token-BLOCKING))
   ("pipeline" (token-PIPELINE))
   ("add" (token-ADD))
   (arith-op1 (token-ARITHOP1 lexeme))
   (arith-op2 (token-ARITHOP2 lexeme))
   (arith-op3 (token-ARITHOP3 lexeme))
   (rel-op (token-RELOP lexeme))
   (eq-op (token-EQOP lexeme))
   ("&" (token-BAND))
   ("^" (token-BXOR))
   ("|" (token-BOR))
   ("!" (token-NOT))
   ("&&" (token-AND))
   ("||" (token-OR))
   ("(" (token-LPAREN))
   (")" (token-RPAREN))
   ("{" (token-LBRACK))
   ("}" (token-RBRACK))
   ("[" (token-LSQBR))
   ("]" (token-RSQBR))
   (";" (token-SEMICOL))
   ("::" (token-EXT))
   (":" (token-COL))
   ("," (token-COMMA))
   ("=" (token-=))
   ((re-+ number10) (token-NUM (string->number lexeme)))
   (identifier      (token-VAR lexeme))


   ;; recursively calls the lexer which effectively skips whitespace
   (whitespace   (position-token-token (greensyn-lexer input-port)))
   (line-comment (position-token-token (greensyn-lexer input-port)))
   (comment (position-token-token (greensyn-lexer input-port)))

   ((eof) (token-EOF))))

;; (define-syntax-rule (BinExp exp1 operation exp2)
;;   (new BinExp% [op (new Op% [op operation])] [e1 exp1] [e2 exp2]))

(define-syntax (BinExp stx)
  (syntax-case stx ()
    [(BinExp exp1 operation exp2 position) 
     #'(new BinExp% [op (new Op% [op operation] [pos position])] [e1 exp1] [e2 exp2])]
    [(BinExp exp1 operation exp2 pl position) 
     #'(new BinExp% [op (new Op% [op operation] [place pl] [pos position])] [e1 exp1] [e2 exp2])]
    ))

(define-syntax (UnaExp stx)
  (syntax-case stx ()
    [(UnaExp operation exp1 position) 
     #'(new UnaExp% [op (new Op% [op operation] [pos position])] [e1 exp1])]
    [(UnaExp operation exp1 pl position) 
     #'(new UnaExp% [op (new Op% [op operation] [place pl] [pos position])] [e1 exp1])]
    ))

(define chunk 32)

(define (default-array-place begin end)
  (let ([to (+ begin chunk)])
    (if (<= end to)
        (list (new RangePlace% [from begin] [to end]))
        (cons (new RangePlace% [from begin] [to to]) (default-array-place to end)))))

(define greensyn-parser
  (parser
   (start program)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos) 
      (raise-syntax-error 'parser
			  (format "syntax error at '~a' in src l:~a c:~a" 
			      tok-name
			      (position-line start-pos)
			      (position-col start-pos)))))
   (tokens a b)
   (precs 
    (left OR)
    (left AND)
    (left BOR)
    (left BXOR)
    (left BAND)
    (left EQOP) 
    (left RELOP) 
    (left ARITHOP3)
    (left ARITHOP2)
    (left ARITHOP1)
    (left NOT))
   (src-pos)
   (grammar
    (place-exp
         ((NUM) $1)
         ((VAR) $1)
         ;((HERE) (new Place% [at "here"] [pos $1-start-pos]))
         ((ANY) (new Place% [at "any"] [pos $1-start-pos]))
         ((PLACE LPAREN ele RPAREN) (new Place% [at $3] [pos $1-start-pos]))
         )
    
    (array-place
         ((LSQBR NUM COL NUM RSQBR = place-exp) (new RangePlace% [from $2] [to $4] [place $7]))
         ((LSQBR NUM COL NUM RSQBR) (new RangePlace% [from $2] [to $4]))
	 )
    
    (array-place-exp
         ((array-place) (list $1))
         ((array-place-exp COMMA array-place) (append $1 (list $3)))
         )

    ;; place-dist
    (place-dist
         ((place-exp) $1)
         ((LBRACK array-place-exp RBRACK) $2))

    (place-dist-list
         ((place-dist) (list $1))
         ((place-dist COMMA place-dist-list) (cons $1 $3)))

    (place-dist-tuple
         ((LPAREN place-dist-list RPAREN) (new TypeExpansion% [place-list $2])))

    (place-dist-expand
         ((place-dist) $1)
         ((place-dist-tuple) $1))

    ;; place-type-dist
    (place-type-dist
         ((place-exp) $1)
         ((LBRACK array-place-exp SEMICOL ele RBRACK) (cons $2 $4)))

    (place-type-dist-list
         ((place-type-dist) (list $1))
         ((place-type-dist COMMA place-type-dist-list) (cons $1 $3)))

    (place-type-dist-tuple
         ((LPAREN place-type-dist-list RPAREN) (new TypeExpansion% [place-list $2])))

    (place-type-dist-expand
         ((place-type-dist) $1)
         ((place-type-dist-tuple) $1))
         
    (const ((NUM)           (new Const% [n $1] [pos $1-start-pos])))

    (lit ((const)           (new Num% [n $1])))

    (id  ((VAR)             (new Var% [name $1] [pos $1-start-pos]))
	 ((VAR EXT NUM)     (new Var% [name $1] [sub $3] [pos $1-start-pos])))

    (array 
         ((VAR LSQBR exp RSQBR) (new Array% [name $1] [pos $1-start-pos] [index $3]))
         ((VAR EXT NUM LSQBR exp RSQBR) 
          (new Array% [name $1] [sub $3] [pos $1-start-pos] [index $5])))

    (ele ((id) $1)
	 ((array) $1))

    (funccall
         ((VAR LPAREN args RPAREN)    (new FuncCall% [name $1] [args $3] [pos $1-start-pos]))
	 ((READ LPAREN VAR RPAREN)   (new Recv% [port (string->symbol $3)]))
	 ((WRITE LPAREN VAR COMMA exp RPAREN)  
	  (new Send% [port (string->symbol $3)] [data $5])))

    (exp ((lit) $1)
	 ((ele) $1)

         ((NOT exp)         (UnaExp "!" $2 $1-start-pos))
         ((ARITHOP2 exp)     (prec NOT) (UnaExp $1 $2 $1-start-pos))
         ((exp ARITHOP1 exp) (BinExp $1 $2 $3 $2-start-pos))
         ((exp ARITHOP2 exp) (BinExp $1 $2 $3 $2-start-pos))
         ((exp ARITHOP3 exp) (BinExp $1 $2 $3 $2-start-pos))
         ((exp RELOP exp)    (BinExp $1 $2 $3 $2-start-pos))
         ((exp EQOP exp)     (BinExp $1 $2 $3 $2-start-pos))
         ((exp BAND exp)     (BinExp $1 "&" $3 $2-start-pos))
         ((exp BXOR exp)     (BinExp $1 "^" $3 $2-start-pos))
         ((exp BOR exp)      (BinExp $1 "|" $3 $2-start-pos))
         ((exp AND exp)      (BinExp $1 "&&" $3 $2-start-pos))
         ((exp OR exp)       (BinExp $1 "||" $3 $2-start-pos))
         
         ((NOT @ place-type-dist-expand exp)         (prec NOT) (UnaExp "!" $4 $3 $1-start-pos))
         ((ARITHOP2 @ place-type-dist-expand exp)     (prec NOT) (UnaExp $1 $4 $3 $1-start-pos))
         ((exp ARITHOP1 @ place-type-dist-expand exp) (prec ARITHOP1) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp ARITHOP2 @ place-type-dist-expand exp) (prec ARITHOP2) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp ARITHOP3 @ place-type-dist-expand exp) (prec ARITHOP3) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp RELOP @ place-type-dist-expand exp)    (prec RELOP) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp EQOP @ place-type-dist-expand exp)     (prec EQOP) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp BAND @ place-type-dist-expand exp)     (prec BAND) (BinExp $1 "&" $5 $4 $2-start-pos))
         ((exp BXOR @ place-type-dist-expand exp)     (prec BXOR) (BinExp $1 "^" $5 $4 $2-start-pos))
         ((exp BOR @ place-type-dist-expand exp)      (prec BOR) (BinExp $1 "|" $5 $4 $2-start-pos))
         ((exp AND @ place-type-dist-expand exp)      (prec AND) (BinExp $1 "&&" $5 $4 $2-start-pos))
         ((exp OR @ place-type-dist-expand exp)       (prec OR) (BinExp $1 "||" $5 $4 $2-start-pos))

	 ((LPAREN exp RPAREN) $2)
	 ((funccall) $1)
         )

    ;; (known-type
    ;;      (() "")
    ;;      ((KNOWN) "known"))

    (cluster
         (() #f)
         ((CLUSTER) #t))

    (data-type
         ((INT) "int")
         ((INT EXT NUM) (cons "int" $3))
         ((VOID) "void")
         )

    (place-list
	 ((place-exp) (list $1))
	 ((place-exp COMMA place-list) (cons $1 $3)))

    (place-tuple
         ((LPAREN place-list RPAREN) (new TypeExpansion% [place-list $2])))

    (data-place-type
	 ;; get symbolic place if there is no @ specified
         ((data-type)                 (cons $1 (get-sym))) 
         ((data-type @ place-exp)     (cons $1 $3))
	 ((data-type @ place-tuple)   (cons $1 $3)))

    ;; a,b,c
    (var-list
         ((VAR) (list $1))
         ((VAR COMMA var-list) (cons $1 $3))) 

    ;; a, abs(b), c+d
    (arg-list
         ((exp) (list $1))
	 ((exp COMMA arg-list) (cons $1 $3)))

    (args
         (() (list))
	 ((arg-list) $1))

    ;; int a, int b, int c
    (param
         ((data-place-type VAR)
            (new Param% [var-list (list $2)] [type (car $1)]
                 [place (cdr $1)]
                 [pos $2-start-pos])))

    (param-list
         ((param) (list $1))
         ((param COMMA param-list) (cons $1 $3)))

    (params 
         (() (list))
         ((param-list) $1))

    (var-decl
         ; var declaration
         ((data-place-type var-list SEMICOL) 
            (new VarDecl% [var-list $2] [type (car $1)] [place (cdr $1)]
                 [pos $2-start-pos]))
         
         ; array declaration
         ((data-type LSQBR RSQBR VAR LSQBR NUM RSQBR SEMICOL)
            (new ArrayDecl% [var $4] [type $1] [cluster #f] [bound $6]
	 	 [place-list (default-array-place 0 $6)]
                 [pos $4-start-pos]))

         ; array declaration with placement
         ((data-type LSQBR RSQBR @ place-dist-expand
                      VAR LSQBR NUM RSQBR SEMICOL)
            (new ArrayDecl% [var $6] [type $1] [cluster #f] [bound $8] 
                 [place-list (if (or (list? $5) (is-a? $5 TypeExpansion%))
                                 $5
                                 (list (new RangePlace% [from 0] [to $8] [place $5])))]
                 [pos $6-start-pos]))

         ; array declaration
         ((CLUSTER data-type LSQBR RSQBR VAR LSQBR NUM RSQBR SEMICOL)
            (new ArrayDecl% [var $5] [type $2] [cluster #t] [bound $7]
	 	 [place-list (default-array-place 0 $7)]
                 [pos $5-start-pos]))

         ; array declaration with placement
         ((CLUSTER data-type LSQBR RSQBR @ place-dist-expand
                      VAR LSQBR NUM RSQBR SEMICOL)
            (new ArrayDecl% [var $7] [type $2] [cluster #t] [bound $9] 
                 [place-list (if (or (list? $6) (is-a? $6 TypeExpansion%))
                                 $6
                                 (list (new RangePlace% [from 0] [to $9] [place $6])))]
                 [pos $7-start-pos])))
         
    (stmt 
         ; assignment
         ((ele = exp SEMICOL) 
            (new Assign% [lhs $1] [rhs $3] [pos $1-start-pos]))

         ; var declaration/array declaration
         ((var-decl) $1)

         ; for loop
         ((FOR LPAREN VAR FROM NUM TO NUM RPAREN LBRACK block RBRACK)
            (new For% [iter (new Var% [name $3] [known-type #t] [pos $3-start-pos])] 
                 [known #t] [from $5] [to $7] [place-list (new Place% [at "any"])] 
                 [body $10] [pos $1-start-pos]))

         ; for loop with placement
         ((FOR LPAREN VAR FROM NUM TO NUM RPAREN 
               @ place-dist-expand
               LBRACK block RBRACK)
            (new For% [iter (new Var% [name $3] [known-type #t] [pos $3-start-pos])] 
                 [known #t] [from $5] [to $7] [place-list $10] 
                 [body $12] [pos $1-start-pos]))

	 ; while loop
	 ((WHILE LPAREN exp RPAREN LBRACK block RBRACK)
	    (new While% [condition $3] [body $6] [pos $1-start-pos]))

         ; if
         ((IF LPAREN exp RPAREN LBRACK block RBRACK)
            (new If% [condition $3] 
                 [true-block $6] 
                 [pos $1-start-pos]))

         ; if-else
         ((IF LPAREN exp RPAREN LBRACK block RBRACK ELSE LBRACK block RBRACK)
            (new If% [condition $3] 
                 [true-block $6] 
                 [false-block $10] 
                 [pos $1-start-pos]))

         ; return
         ((RETURN exp SEMICOL)
            ;; (new Assign% [lhs (new Var% [name "#return"] [pos $1-start-pos])] 
            ;;      [rhs $2] [pos $1-start-pos]))
	    (new Return% [val $2] [pos $1-start-pos]))

         ; function call
         ((funccall SEMICOL) $1)
         
         ; stream-related statements
         ((WORK BLOCKING LBRACK block RBRACK) (new WorkBlocking% [block $4]))
         ((ADD funccall SEMICOL) (new Add% [call $2]))
         )
    
    (stmts
         ((stmt)       (list $1))
         ((stmt stmts) (cons $1 $2)))
    
    (filter-decl
        ((data-place-type -> data-place-type FILTER VAR LPAREN params RPAREN LBRACK block RBRACK)
         (new AbstractFilterDecl% [name $5] [args (new Block% [stmts $7])] [body $10]
              [input  (new InputDecl% [var-list (list "#input")] 
                           [type (car $1)] [place (cdr $1)])]
              [output (new OutputDecl% [var-list (list "#output")] 
                           [type (car $1)] [place (cdr $1)])]
              )))
    
    (pipeline-decl
        ((data-place-type -> data-place-type PIPELINE VAR LPAREN params RPAREN LBRACK block RBRACK)
         (new PipelineDecl% [name $5] [args (new Block% [stmts $7])] [body $10]
              [input  (new InputDecl% [var-list (list "#input")] 
                           [type (car $1)] [place (cdr $1)])]
              [output (new OutputDecl% [var-list (list "#output")] 
                           [type (car $1)] [place (cdr $1)])]
              )))
    
    (block ((stmts) (new Block% [stmts $1])))

    (func-decl
         ((data-place-type VAR LPAREN params RPAREN LBRACK block RBRACK)
          (new FuncDecl% [name $2] [args (new Block% [stmts $4])] [body $7] 
               [return (new ReturnDecl% [var-list (list "#return")] 
			    [type (car $1)] [place (cdr $1)])]
               [pos $2-start-pos])))

     (decl
          ;((var-decl) $1)
          ((func-decl) $1)
          ((filter-decl) $1)
          ((pipeline-decl) $1))

    (decls
         ((decl) (list $1))
         ((decl decls) (cons $1 $2)))
         
    (program
         ((decls) (new Program% [stmts $1])))

)))

(define (lex-this lexer input) 
  (lambda () 
    (let ([token (lexer input)])
      ;(pretty-display token)
      token)))

(define (ast-from-string s)
  (let ((input (open-input-string s)))
    (ast input)))

(define (ast-from-file file)
  (let ((input (open-input-file file)))
    (port-count-lines! input)
    (ast input)))

(define (ast input)
  (greensyn-parser (lex-this greensyn-lexer input)))

