#lang racket/base
(require racket/class
         racket/gui/base
         framework
         syntax-color/default-lexer
         syntax-color/module-lexer
         expeditor/private/param
         expeditor/private/object
         scribble/private/indentation)

;; Check that `like-text%` simulates `color:text%`,
;; based on tests from Greg Hendershott

(define (insert t str)
  (define lp (send t last-position))
  (send t insert str lp lp)
  (send t freeze-colorer))

(define-syntax-rule (check-equal? a b more)
  (let ([av a]
        [bv b])
    (unless (equal? av bv)
      (eprintf "failed:\n  ~s = ~s\n  ~s = ~s\n ~s\n" 'a av 'b bv more)
      (error "stop"))))

(define (check str lexer paren-matches
               #:indentation [indentation #f])
  (define o (parameterize ([current-expeditor-lexer lexer]
                           [current-expeditor-parentheses paren-matches])
              (new-object str)))
  (define t (new color:text%))
  (send t start-colorer symbol->string lexer paren-matches)
  (insert t str)
  (check-equal? (send o last-position)
                (send t last-position)
                #f)

  ;; Test that our implementations of {forward backward}-match are
  ;; equivalent to those of color:text%.
  (define lp (string-length str))
  (for ([pos (in-range 0 (string-length str))])
    (send t set-position pos pos)
    (check-equal? (send o forward-match pos lp)
                  (send t forward-match pos lp)
                  (format "forward-match ~v" pos))
    (check-equal? (send o backward-match pos lp)
                  (send t backward-match pos lp)
                  (format "backward-match ~v" pos))
    (check-equal? (send o backward-containing-sexp pos 0)
                  (send t backward-containing-sexp pos 0)
                  (format "backward-containing-sexp ~v" pos))
    (for* ([dir (in-list '(forward backward))]
           [comments? (in-list '(#f #t))])
      (check-equal? (send o skip-whitespace pos dir comments?)
                    (send t skip-whitespace pos dir comments?)
                    (format "skip-whitespace ~v ~v ~v" pos dir comments?))))

  ;; Test that we supply enough entire color-text% methods, and that
  ;; they behave equivalently to from racket-text%, as needed by a
  ;; lang-supplied drracket:indentation
  ;; function.
  (when indentation
    (for ([pos (in-range 0 (string-length str))])
      (check-equal? (indentation o pos)
                    (indentation t pos)
                    (format "~v ~v" determine-spaces pos)))))

(define at-exp-racket-str
  "#lang at-exp racket\n(1) word #(2) #hash((1 . 2))\n@racket[]|{\n#(2)\n}|\n")

'default
(check at-exp-racket-str default-lexer racket:default-paren-matches)
'racket
(check at-exp-racket-str module-lexer racket:default-paren-matches
       #:indentation determine-spaces)
