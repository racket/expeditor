#lang racket/base
(require "param.rkt")

(provide read-token
         opener->closer)

(define (read-token ip state)
  (define-values (lexeme orig-type paren start end backup new-state)
    (let ([lex (current-expression-editor-lexer)])
      (if (procedure-arity-includes? lex 3)
          (lex ip 0 state)
          (let-values ([(lexeme type paren start end) (lex ip)])
            (values lexeme type paren start end 0 #f)))))
  (define-values (type value)
    (translate-type orig-type paren lexeme))
  (if (eq? type 'white-space)
      (read-token ip new-state)
      (values type
              value
              (if start (sub1 start) (file-position ip))
              (if end (sub1 end) (file-position ip))
              new-state)))

(define (translate-type type paren lexeme)
  (case type
    [(parenthesis)
     (define new-type
       (for/or ([p (in-list (current-expression-editor-parentheses))])
         (cond
           [(eq? paren (car p)) 'opener]
           [(eq? paren (cadr p)) 'closer]
           [else #f])))
     (if new-type
         (values new-type paren)
         (values type lexeme))]
    [(symbol)
     (values type (let ([lexeme (if (syntax? lexeme) (syntax->datum lexeme) lexeme)])
                    (if (string? lexeme)
                        (string->symbol lexeme)
                        lexeme)))]
    [else (values type lexeme)]))

(define (opener->closer paren)
  (for/or ([p (in-list (current-expression-editor-parentheses))])
    (and (eq? paren (car p))
         (cadr p))))
