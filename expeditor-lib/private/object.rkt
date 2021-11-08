#lang racket/base
(require racket/class
         "param.rkt")

(provide new-object)

(define (new-object s)
  (new like-text% [content s]))

(struct token (type paren start end))

(define (lex-all ip)
  (define lex (current-expeditor-lexer))
  (let loop ([state #f])
    (define-values (lexeme type paren start end backup new-state)
      (if (procedure-arity-includes? lex 3)
          (lex ip 0 state)
          (let-values ([(lexeme type paren start end) (lex ip)])
            (values lexeme type paren start end 0 state))))
    (cond
      [(eq? type 'eof) '()]
      [else (cons (token type paren (sub1 start) (sub1 end))
                  (loop new-state))])))

(define like-text%
  (class object%
    (init-field content)

    (super-new)

    (define-values (position-paragraphs paragraph-starts)
      (let loop ([pos 0] [para 0] [pos-para #hasheqv()] [para-pos #hasheqv((0 . 0))])
        (cond
          [(= pos (string-length content))
           (values (hash-set pos-para pos para) para-pos)]
          [(char=? #\newline (string-ref content pos))
           (loop (add1 pos) (add1 para)
                 (hash-set pos-para pos para)
                 (hash-set para-pos (add1 para) (add1 pos)))]
          [else
           (loop (add1 pos) para (hash-set pos-para pos para) para-pos)])))
    
    ;; position -> token
    (define mapping
      (let ([tokens (lex-all (let ([p (open-input-string content)])
                               (port-count-lines! p)
                               p))])
        (let loop ([tokens tokens] [pos 0] [mapping #hasheqv()])
          (cond
            [(null? tokens) mapping]
            [else
             (define t (car tokens))
             (unless (= pos (token-start t))
               (error 'editor "token discontinuity ~s vs. ~s" pos (token-start t)))
             (loop (cdr tokens)
                   (token-end t)
                   (for/fold ([mapping mapping]) ([i (in-range (- (token-end t) (token-start t)))])
                     (hash-set mapping (+ pos i) t)))]))))

    (define/public (get-text s e)
      (substring content s e))

    (define/public (classify-position* pos)
      (define t (or (hash-ref mapping pos #f)
                    (hash-ref mapping (sub1 pos) #f) ; make end position work
                    (error 'classify-position "lookup failed: ~e" pos)))
      (token-type t))

    (define/public (classify-position pos)
      (define attribs (classify-position* pos))
      (if (symbol? attribs)
          attribs
          (hash-ref attribs 'type 'unknown)))

    (define/public (get-token-range pos)
      (define t (hash-ref mapping pos #f))
      (if t
          (values (token-start t)
                  (token-end t))
          (values #f #f)))

    (define/public (last-position)
      (string-length content))

    (define/public (position-paragraph pos [eol? #f])
      (or (hash-ref position-paragraphs pos #f)
          (error 'position-paragraph "lookup failed: ~e" pos)))
    
    (define/public (paragraph-start-position para)
      (or (hash-ref paragraph-starts para #f)
          (error 'paragraph-start-position "lookup failed: ~e" para)))

    (define/public (paragraph-end-position para)
      (define n (hash-ref paragraph-starts (add1 para) #f))
      (if n
          (sub1 n)
          (last-position)))

    (define/public (backward-match pos cutoff)
      (let loop ([pos (sub1 pos)] [depth -1] [need-close? #t])
        (cond
          [(pos . < . 0) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (define sym (string->symbol (get-text s e)))
              (let paren-loop ([parens (current-expeditor-parentheses)])
                (cond
                  [(null? parens) #f]
                  [(eq? sym (caar parens))
                   (and (not need-close?)
                        (if (= depth 0)
                            s
                            (loop (sub1 s) (sub1 depth) #f)))]
                  [(eq? sym (cadar parens))
                   (loop (sub1 s) (add1 depth) #f)]
                  [else
                   (paren-loop (cdr parens))]))]
             [(whitespace comment)
              (loop (sub1 s) depth need-close?)]
             [else (if need-close?
                       s
                       (loop (sub1 s) depth #f))])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (define sym (string->symbol (get-text s e)))
              (let paren-loop ([parens (current-expeditor-parentheses)])
                (cond
                  [(null? parens) #f]
                  [(eq? sym (caar parens))
                   (loop e (add1 depth))]
                  [(eq? sym (cadar parens))
                   (if (depth . <= . 1)
                       e
                       (loop e (sub1 depth)))]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (loop e depth)])])))))
