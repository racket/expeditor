#lang racket/base
(require racket/fixnum)

;; See "../expeditor.rkt"

;;; parameters

(provide ee-common-identifiers
         ee-default-repeat
         ee-auto-indent
         ee-auto-paren-balance
         ee-flash-parens
         ee-paren-flash-delay
         ee-noisy
         ee-standard-indent
         ee-history-limit
         current-expression-editor-lexer
         current-expression-editor-reader
         current-expression-editor-post-skipper
         current-expression-editor-ready-checker
         current-expression-editor-completer
         current-expression-editor-history)

(define (fxnonnegative? v)
  (and (fixnum? v)
       (fx>= v 0)))

(define ee-common-identifiers
  (make-parameter
   ; general theory: exclude short ids and ids that will come up early
   ; in an alphabetical search with short prefix.  include common ids that
   ; come up annoyingly late in such a search.
   '(append apply call/cc call-with-values define display display-string
            define-syntax define-record null? quote quotient reverse read-char
            substring string-ref string-length string? string=? string-set!
            syntax-case syntax-rules unless vector-ref vector-length vector?
            vector-set! vector)
    (lambda (x)
      (unless (and (list? x) (andmap symbol? x))
        (error 'ee-common-identifiers "~s is not a list of symbols" x))
      x)))

;;; default repeat value for ^U
(define ee-default-repeat
  (make-parameter 4
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-default-repeat "~s is not an integer" x))
      x)))

(define ee-auto-indent (make-parameter #t (lambda (x) (and x #t))))

(define ee-auto-paren-balance (make-parameter #t (lambda (x) (and x #t))))

(define ee-flash-parens (make-parameter #t (lambda (x) (and x #t))))

;;; paren balance delay factor in milliseconds
(define ee-paren-flash-delay
  (make-parameter 100
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-paren-flash-delay "~s is not an integer" x))
      x)))

;;; enable/disable bell
(define ee-noisy (make-parameter #f (lambda (x) (and x #t))))

;;; standard indent length
(define ee-standard-indent
  (make-parameter 2
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-standard-indent "~s is not an integer" x))
      x)))

(define ee-history-limit
  (make-parameter 256
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-history-length "~s is not a nonnegative fixnum" x))
      x)))

(define current-expression-editor-lexer
  (make-parameter (lambda (ip)
                    (define start (add1 (file-position ip)))
                    (define ch (read-char ip))
                    (define end (add1 (file-position ip)))
                    (case ch
                      [(#\( #\) #\[ #\] #\{ #\})
                       (values (string ch) 'parenthesis (string->symbol (string ch)) start end)]
                      [else
                       (if (eof-object? ch)
                           (values eof 'eof #f start end)
                           (values (string ch) 'atomic #f start end))]))
                  (lambda (p)
                    p)))

(define current-expression-editor-reader
  (make-parameter (lambda (ip)
                    (read ip))
                  (lambda (p)
                    p)))

(define current-expression-editor-post-skipper
  (make-parameter (lambda (ip)
                    (let loop ([n 0])
                      (define ch (read-char ip))
                      (cond
                        [(eof-object? ch) n]
                        [(char-whitespace? ch) (loop (add1 n))]
                        [else n])))
                  (lambda (p)
                    p)))

(define current-expression-editor-ready-checker
  (make-parameter (lambda (ip)
                    (with-handlers ([exn:fail:read? (lambda (exn) #f)])
                      (read ip)
                      #t))
                  (lambda (p)
                    p)))

(define current-expression-editor-history
  (make-parameter null))

(define current-expression-editor-completer
  (make-parameter (lambda (prefix)
                    (values (parameterize ([current-namespace (make-base-namespace)])
                              (namespace-mapped-symbols))
                            (ee-common-identifiers)))))
