#lang racket/base
(require "param.rkt")

(provide read-token)

(define (read-token ip state)
  (define-values (lexeme type paren start end backup new-state)
    (let ([lex (current-expression-editor-lexer)])
      (if (procedure-arity-includes? lex 3)
          (lex ip 0 state)
          (let-values ([(lexeme type paren start end) (lex ip)])
            (values lexeme type paren start end 0 #f)))))
  (if (eq? type 'white-space)
      (read-token ip new-state)
      (values (translate-type type paren)
              (if (eq? type 'symbol)
                  (string->symbol lexeme)
                  lexeme)
              (if start (sub1 start) (file-position ip))
              (if end (sub1 end) (file-position ip))
              new-state)))

(define (translate-type type paren)
  (case type
    [(parenthesis)
     (case paren
       [(|(|) 'lparen]
       [(|)|) 'rparen]
       [(|[|) 'lbrack]
       [(|]|) 'rbrack]
       [else 'atomic])]
    [(eof) 'eof]
    [(error) 'error]
    [else
     'atomic]))
