#lang racket

(require "header.rkt"
         "parser.rkt"
         "ast-util.rkt"
         "visitor-desugar.rkt"
         "visitor-printer.rkt"
         "visitor-linker.rkt" )

(define (parse file)
  ;(define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (pretty-display "=============== before link ===============")
  (send my-ast pretty-print)
  (define need-temp (send my-ast accept (new linker%)))
  (pretty-display "=============== after link ===============")
  (send my-ast pretty-print)
  ;(send my-ast accept (new temp-inserter%))
  ;(pretty-display "=============== after temp-insert ===============")
  ;(send my-ast pretty-print)
  ;(send my-ast accept (new desugar%))
  ;(pretty-display "=============== after desugar ===============")
  ;(send my-ast pretty-print)
  my-ast)

(parse "../examples/test.cll")