#lang racket

(require "ast.rkt" "parser.rkt")

(define test "known int x; x = (-1@1 &@2 100@3) <@4 (!@5 2@5 ||@6 20@7) +@8 -1@9 *@10 2@11;")
;(define test "20 +@a -1 *@a 10")

(define my-ast (ast-from-string test))

(andmap (lambda (i) (send i pretty-print)) my-ast)
;(send ast pretty-print)