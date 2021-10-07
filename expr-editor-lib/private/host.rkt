#lang racket/base
(require ffi/unsafe/vm)

;; See "../expeditor.rkt"

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

(define-syntax-rule (foreign-procedure name (arg ...) result)
  (vm-eval '(foreign-procedure name (arg ...) result)))

(define init-term (foreign-procedure "(cs)ee_init_term" () boolean))
(define $ee-read-char (foreign-procedure "(cs)ee_read_char" (boolean) scheme-object))
(define $ee-write-char (foreign-procedure "(cs)ee_write_char" (wchar_t) void))
(define ee-flush (foreign-procedure "(cs)ee_flush" () void))
(define get-screen-size (foreign-procedure "(cs)ee_get_screen_size" () scheme-object))
(define raw-mode (foreign-procedure "(cs)ee_raw" () void))
(define no-raw-mode (foreign-procedure "(cs)ee_noraw" () void))
(define enter-am-mode (foreign-procedure "(cs)ee_enter_am_mode" () void))
(define exit-am-mode (foreign-procedure "(cs)ee_exit_am_mode" () void))
(define nanosleep (foreign-procedure "(cs)ee_nanosleep" (unsigned-32 unsigned-32) void))
(define pause (foreign-procedure "(cs)ee_pause" () void))
(define get-clipboard (foreign-procedure "(cs)ee_get_clipboard" () scheme-object))

(define move-cursor-up (foreign-procedure "(cs)ee_up" (integer-32) void))
(define move-cursor-down (foreign-procedure "(cs)ee_down" (integer-32) void))
(define $move-cursor-left (foreign-procedure "(cs)ee_left" (integer-32) void))
(define $move-cursor-right (foreign-procedure "(cs)ee_right" (integer-32) void))
(define clear-eol (foreign-procedure "(cs)ee_clr_eol" () void))
(define clear-eos (foreign-procedure "(cs)ee_clr_eos" () void))
(define $clear-screen (foreign-procedure "(cs)ee_clear_screen" () void))
(define scroll-reverse (foreign-procedure "(cs)ee_scroll_reverse" (integer-32) void))
(define bell (foreign-procedure "(cs)ee_bell" () void))
(define $carriage-return (foreign-procedure "(cs)ee_carriage_return" () void))
(define line-feed (foreign-procedure "(cs)ee_line_feed" () void))
