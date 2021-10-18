#lang racket/base
(require ffi/unsafe/vm
         '#%terminal)

;; See "../main.rkt"

(provide init-term
         $ee-read-char
         $ee-write-char
         ee-flush
         get-screen-size
         raw-mode
         no-raw-mode
         enter-am-mode
         exit-am-mode
         nanosleep
         pause
         get-clipboard

         move-cursor-up
         move-cursor-down
         $move-cursor-left
         $move-cursor-right
         clear-eol
         clear-eos
         $clear-screen
         scroll-reverse
         bell
         $carriage-return
         line-feed)

(define init-term terminal-init)
(define $ee-read-char/blocking terminal-read-char)
(define $ee-write-char terminal-write-char)
(define ee-flush terminal-flush)
(define get-screen-size terminal-get-screen-size)
(define raw-mode (lambda () (terminal-raw-mode #t)))
(define no-raw-mode (lambda () (terminal-raw-mode #f)))
(define post-output-mode (lambda () (terminal-postoutput-mode #t)))
(define no-post-output-mode (lambda () (terminal-postoutput-mode #f)))
(define enter-am-mode (lambda () (terminal-automargin-mode #t)))
(define exit-am-mode (lambda () (terminal-automargin-mode #f)))
(define nanosleep terminal-nanosleep)
(define pause terminal-pause)
(define get-clipboard terminal-get-clipboard)

(define move-cursor-up (lambda (amt) (terminal-move-cursor 'up amt)))
(define move-cursor-down (lambda (amt) (terminal-move-cursor 'down amt)))
(define $move-cursor-left (lambda (amt) (terminal-move-cursor 'left amt)))
(define $move-cursor-right (lambda (amt) (terminal-move-cursor 'right amt)))
(define clear-eol (lambda () (terminal-clear 'eol)))
(define clear-eos (lambda () (terminal-clear 'eos)))
(define $clear-screen (lambda () (terminal-clear 'screen)))
(define scroll-reverse terminal-scroll-reverse)
(define bell terminal-bell)
(define $carriage-return terminal-carriage-return)
(define line-feed terminal-line-feed)

(define ($ee-read-char block?)
  (cond
    [block?
     (post-output-mode)
     (sync (current-input-port))
     (no-post-output-mode)
     (or ($ee-read-char/blocking #f)
         ($ee-read-char block?))]
    [else
     ($ee-read-char/blocking #f)]))
