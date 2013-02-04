#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(require "ast.rkt")

(provide ast-from-string)
 
(define-tokens a (NUM VAR ARITHOP1 ARITHOP2 RELOP EQOP))
(define-empty-tokens b (@ BNOT BAND BXOR BOR AND OR EOF 
			       LPAREN RPAREN LBRACK RBRACK
			       = SEMICOL
                               INT KNOWN))

(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))

(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (arith-op1 (re-or "*" "/"))
  (arith-op2 (re-or "+" "-"))
  (rel-op (re-or "<" "<=" ">=" ">"))
  (eq-op (re-or "==" "!="))
  (identifier-characters (re-or (char-range "A" "z")
                                "?" ":" "$" "%" "^" "&"))
  (identifier-characters-ext (re-or digit10 identifier-characters))
  (identifier (re-seq (re-+ identifier-characters) 
                      (re-* identifier-characters-ext))))
  
(define simple-math-lexer
  (lexer-src-pos
   ("@" (token-@))
   ("!" (token-BNOT))
   (arith-op1 (token-ARITHOP1 lexeme))
   (arith-op2 (token-ARITHOP2 lexeme))
   (rel-op (token-RELOP lexeme))
   (eq-op (token-EQOP lexeme))
   ("&" (token-BAND))
   ("^" (token-BXOR))
   ("|" (token-BOR))
   ("&&" (token-AND))
   ("||" (token-OR))
   ("(" (token-LPAREN))
   (")" (token-RPAREN))
   ("{" (token-LBRACK))
   ("}" (token-RBRACK))
   (";" (token-SEMICOL))
   ("=" (token-=))
   ("int" (token-INT))
   ("known" (token-KNOWN))
   ((re-+ number10) (token-NUM (string->number lexeme)))
   (identifier      (token-VAR lexeme))
   ;; recursively calls the lexer which effectively skips whitespace
   (whitespace (position-token-token (simple-math-lexer input-port)))
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

(define simple-math-parser
  (parser
   (start block)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos) 
      (pretty-display "Error")))
   (tokens a b)
   (precs 
    (left OR)
    (left AND)
    (left BOR)
    (left BXOR)
    (left BAND)
    (left EQOP) 
    (left RELOP) 
    (left ARITHOP2)
    (left ARITHOP1)
    (left BNOT))
   (src-pos)
   (grammar
    (place-exp
         ((NUM) $1)
         ((VAR) $1))

    (lit ((NUM)             (new Num% [n $1] [pos $1-start-pos]))
         ((NUM @ place-exp) (new Num% [n $1] [place $3] [pos $1-start-pos])))

    (id  ((VAR)             (new Var% [name $1] [pos $1-start-pos]))
         ((VAR @ place-exp) (new Var% [name $1] [place $3] [pos $1-start-pos])))

    (exp ((lit) $1)
         ((id)  $1)

         ((BNOT exp)         (UnaExp "!" $2 $1-start-pos))
         ((exp ARITHOP1 exp) (BinExp $1 $2 $3 $2-start-pos))
         ((exp ARITHOP2 exp) (BinExp $1 $2 $3 $2-start-pos))
         ((exp RELOP exp)    (BinExp $1 $2 $3 $2-start-pos))
         ((exp EQOP exp)     (BinExp $1 $2 $3 $2-start-pos))
         ((exp BAND exp)     (BinExp $1 "&" $3 $2-start-pos))
         ((exp BXOR exp)     (BinExp $1 "^" $3 $2-start-pos))
         ((exp BOR exp)      (BinExp $1 "|" $3 $2-start-pos))
         ((exp AND exp)      (BinExp $1 "&&" $3 $2-start-pos))
         ((exp OR exp)       (BinExp $1 "||" $3 $2-start-pos))
         
         ((BNOT @ place-exp exp)         (prec BNOT) (UnaExp "!" $4 $3 $1-start-pos))
         ((exp ARITHOP1 @ place-exp exp) (prec ARITHOP1) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp ARITHOP2 @ place-exp exp) (prec ARITHOP2) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp RELOP @ place-exp exp)    (prec RELOP) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp EQOP @ place-exp exp)     (prec EQOP) (BinExp $1 $2 $5 $4 $2-start-pos))
         ((exp BAND @ place-exp exp)     (prec BAND) (BinExp $1 "&" $5 $4 $2-start-pos))
         ((exp BXOR @ place-exp exp)     (prec BXOR) (BinExp $1 "^" $5 $4 $2-start-pos))
         ((exp BOR @ place-exp exp)      (prec BOR) (BinExp $1 "|" $5 $4 $2-start-pos))
         ((exp AND @ place-exp exp)      (prec AND) (BinExp $1 "&&" $5 $4 $2-start-pos))
         ((exp OR @ place-exp exp)       (prec OR) (BinExp $1 "||" $5 $4 $2-start-pos))

	 ((LPAREN exp RPAREN) $2))

    (known-type
         (() "")
         ((KNOWN) "known"))

    (data-type
         ((INT) "int"))

    (stmt 
         ((VAR = exp SEMICOL) 
            (new Assign% [lhs (new Var% [name $1] [pos $1-start-pos])] [rhs $3]))

         ((known-type data-type VAR SEMICOL) 
            (new VarDecl% [var $3] [type $2] [known-type (equal? $1 "known")] [pos $3-start-pos]))

         ((known-type data-type @ place-exp VAR SEMICOL) 
            (new VarDecl% [var $5] [type $2] [known-type (equal? $1 "known")] [place $4] 
                 [pos $3-start-pos]))
         )

    (stmts
         ((stmt)       (list $1))
         ((stmt stmts) (cons $1 $2)))

    (block ((stmts) (new Block% [stmts $1])))

)))

(define (lex-this lexer input) (lambda () (lexer input)))

(define (ast-from-string s)
  (let ((input (open-input-string s)))
    (simple-math-parser (lex-this simple-math-lexer input))))

