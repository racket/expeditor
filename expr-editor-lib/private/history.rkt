#lang racket/base
(require racket/fixnum
         "param.rkt"
         "ee.rkt")

;; See "../expeditor.rkt"

(provide history-search-bwd history-search-fwd
         update-history! history-fast-forward! entry-modified?
         ee-save-history ee-load-history)

  (define search
    (lambda (ee pred? get-bwd set-bwd! get-fwd set-fwd!)
      (let loop ([bwd (get-bwd ee)]
                 [now (eestate-histnow ee)]
                 [fwd (get-fwd ee)])
        (and (not (null? bwd))
             (let ([s (car bwd)])
               (if (pred? s)
                   (begin
                     (set-bwd! ee (cdr bwd))
                     (set-eestate-histnow! ee s)
                     (set-fwd! ee (cons now fwd))
                     s)
                   (loop (cdr bwd) s (cons now fwd))))))))

  (define history-search-bwd
    (lambda (ee pred?)
      (search ee pred? eestate-histbwd set-eestate-histbwd!
        eestate-histfwd set-eestate-histfwd!)))

  (define history-search-fwd
    (lambda (ee pred?)
      (search ee pred? eestate-histfwd set-eestate-histfwd!
        eestate-histbwd set-eestate-histbwd!)))

  (define history->list
    (lambda (ee)
      (cdr `(,@(reverse (eestate-histfwd ee))
             ,(eestate-histnow ee)
             ,@(eestate-histbwd ee)))))

  (define trim-history
    (lambda (ls)
      (let ([n (ee-history-limit)])
        (if (> (length ls) n)
            (list-ref ls n)
            ls))))

  (define update-history!
    (lambda (ee entry)
      (define (all-whitespace? s)
        (let ([n (string-length s)])
          (let f ([i 0])
            (or (fx= i n)
                (and (memv (string-ref s i) '(#\space #\newline))
                     (f (fx+ i 1)))))))
      (let ([s (entry->string entry)] [ls (history->list ee)])
        (set-eestate-histbwd! ee
          (if (or (all-whitespace? s)
                  (and (not (null? ls))
                       (equal? s (car ls))))
              ls
              (begin
                (set-eestate-histnew! ee (fx+ (eestate-histnew ee) 1))
                (trim-history (cons s ls))))))
      (set-eestate-histnow! ee "")
      (set-eestate-histfwd! ee '())))

  (define history-fast-forward!
    (lambda (ee)
      (set-eestate-histbwd! ee (history->list ee))
      (set-eestate-histnow! ee "")
      (set-eestate-histfwd! ee '())))

  (define (entry-modified? ee entry)
    (not (string=? (entry->string entry) (eestate-histnow ee))))

    (define read-history
      (lambda (ip)
        (with-handlers* ([exn:fail? (lambda (exn) #f)])
          (let loop ([ls '()])
            (let ([x (read ip)])
              (if (eof-object? x)
                  ls
                  (begin
                    (unless (string? x) (error 'read-history "oops"))
                    (loop (cons x ls)))))))))

    (define ee-save-history
      (lambda (ee filename)
        (unless (string? filename)
          (error 'ee-save "~s is not a string" filename))
        (define-values (ip op) (open-input-output-file
                                filename
                                #:exists 'update))
        (let ([ls (let ([curls (history->list ee)])
                    (cond
                      [(read-history ip)
                       => (lambda (savls)
                            (trim-history
                             (append
                              (list-ref curls (eestate-histnew ee))
                              savls)))]
                      [else curls]))])
          (file-truncate op)
          (fprintf op "~
            ;;; This file contains a saved history for the (Petite) Chez Scheme~n
            ;;; expression editor.  The history is represented as a sequence of~n
            ;;; strings, each representing a history entry, with the most recent~n
            ;;; entries listed last.~n
            ~n
            ;;; Exit each Scheme session running the expression editor before~n
            ;;; saving changes so they aren't wiped out when the session ends.\n\n")
          (for-each (lambda (s) (fprintf op "~s\n" s)) (reverse ls))
          (close-input-port ip)
          (close-output-port op))))

    (define ee-load-history
      (lambda (ee filename)
        (unless (string? filename)
          (error 'ee-load-history "~s is not a string" filename))
        (let* ([ip (open-input-file filename)]
               [ls (read-history ip)])
          (close-input-port ip)
          (unless ls
            (error 'ee-load-history "missing or malformed history file ~s"
                   filename))
          (set-eestate-histnew! ee 0)
          (set-eestate-histbwd! ee ls)
          (set-eestate-histnow! ee "")
          (set-eestate-histfwd! ee '()))))
