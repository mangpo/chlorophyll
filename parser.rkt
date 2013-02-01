#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(require "ast.rkt")
(provide (all-defined-out))
 
(define-tokens a (NUM VAR ARITHOP1 ARITHOP2 RELOP EQOP))
(define-empty-tokens b (@ BNOT BAND BXOR BOR AND OR EOF 
			       LPAREN RPAREN LBRACK RBRACK
			       SEMICOL))

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
   ((re-+ number10) (token-NUM (string->number lexeme)))
   (identifier      (token-VAR lexeme))
   ;; recursively calls the lexer which effectively skips whitespace
   (whitespace (position-token-token (simple-math-lexer input-port)))
   ((eof) (token-EOF))))

;; (define-syntax-rule (BinExp exp1 operation exp2)
;;   (new BinExp% [op (new Op% [op operation])] [e1 exp1] [e2 exp2]))

(define-syntax (BinExp stx)
  (syntax-case stx ()
    [(BinExp exp1 operation exp2) 
     #'(new BinExp% [op (new Op% [op operation])] [e1 exp1] [e2 exp2])]
    [(BinExp exp1 operation exp2 p) 
     #'(new BinExp% [op (new Op% [op operation] [place p])] [e1 exp1] [e2 exp2])]
    ))

(define-syntax (UnaExp stx)
  (syntax-case stx ()
    [(UnaExp operation exp1) 
     #'(new UnaExp% [op (new Op% [op operation])] [e1 exp1])]
    [(UnaExp operation exp1 p) 
     #'(new UnaExp% [op (new Op% [op operation] [place p])] [e1 exp1])]
    ))

(define simple-math-parser
  (parser
   (start exp)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos) 
      (pretty-display "error")))
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
    (left BNOT)
    (left @))
   (src-pos)
   (grammar
    (place-exp
         ((NUM) $1)
         ((VAR) $1))
         
    (exp ((NUM)             (new Num% [n $1] [pos $1-start-pos]))
         ((NUM @ place-exp) (new Num% [n $1] [place $3] [pos $1-start-pos]))
         ((VAR)             (new Var% [name $1] [pos $1-start-pos]))
         ((VAR @ place-exp) (new Var% [name $1] [place $3] [pos $1-start-pos]))
         
         ((BNOT exp)         (UnaExp "!" $2))
         ((exp ARITHOP1 exp) (BinExp $1 $2 $3))
         ((exp ARITHOP2 exp) (BinExp $1 $2 $3))
         ((exp RELOP exp)    (BinExp $1 $2 $3))
         ((exp EQOP exp)     (BinExp $1 $2 $3))
         ((exp BAND exp)     (BinExp $1 "&" $3))
         ((exp BXOR exp)     (BinExp $1 "^" $3))
         ((exp BOR exp)      (BinExp $1 "|" $3))
         ((exp AND exp)      (BinExp $1 "&&" $3))
         ((exp OR exp)       (BinExp $1 "||" $3))

         ((BNOT @ place-exp exp)         (UnaExp "!" $4 $3))
         ((exp BNOT @ place-exp exp)     (BinExp $1 "!" $5 $4))
         ((exp ARITHOP1 @ place-exp exp) (BinExp $1 $2 $5 $4))
         ((exp ARITHOP2 @ place-exp exp) (BinExp $1 $2 $5 $4))
         ((exp RELOP @ place-exp exp)    (BinExp $1 $2 $5 $4))
         ((exp EQOP @ place-exp exp)     (BinExp $1 $2 $5 $4))
         ((exp BAND @ place-exp exp)     (BinExp $1 "&" $5 $4))
         ((exp BXOR @ place-exp exp)     (BinExp $1 "^" $5 $4))
         ((exp BOR @ place-exp exp)      (BinExp $1 "|" $5 $4))
         ((exp AND @ place-exp exp)      (BinExp $1 "&&" $5 $4))
         ((exp OR @ place-exp exp)       (BinExp $1 "||" $5 $4))

	 ((LPAREN exp RPAREN) $2)
         
         ))))

(define (lex-this lexer input) (lambda () (lexer input)))

;(define test "(-1@-2 & 100) <@a (!2@a || 20) /@a -1@a +@a 10@a")
(define test "(-1@-2 & 100) < (!2 || 20) / -1 + 10")

(define ast
  (let ((input (open-input-string test)))
    (simple-math-parser (lex-this simple-math-lexer input))))


(send ast pretty-print)

;(define (loop input)
;  (define out (simple-math-lexer input))
;  (pretty-display (position-token-token out))
;  (simple-math-parser out)
;  (when (not (equal? (position-token-token out) 'EOF))
;    (loop input)))

;(let ([input (open-input-string "3 - 3.3 +@5 hi123")])
;  (port-count-lines! input)
;  (loop input))
