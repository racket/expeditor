#lang scribble/manual
@(require racket/list)

@(define (onekey s) (regexp-replace #rx"\\^" (regexp-replace #rx"Esc-" s "Meta-") "Ctl-"))
@(define (binding-table . keys) (apply itemlist  keys))
@(define (key* keys prod . content)
   (item (if (string? keys)
             (onekey keys)
             (add-between (map onekey keys) " or "))
         " --- "
         content))
@(define-syntax-rule (key keys code . content) (key* 'keys 'code . content))

@(define (subsection* s) (subsection #:style '(unnumbered) s))

@title{Expeditor: Terminal Expression Editor}

@defmodule[expeditor]

The expeditor (the @bold{exp}ression @bold{editor}) supports
multi-line editing with indentation and completion support within a
terminal. It's based on Chez Scheme's expression editor, but can be
adapted to Racket languages using the same hooks and APIs as DrRacket.
Normally, the expeditor is run automatically by @racketmodname[xrepl
#:indirect], which is turn loaded by the @exec{racket} command-line
executable.


@section{Default Key Bindings}

In the keybinding descriptions below, a ``Meta-'' combination can be
typed as the Esc key (pressed then released) followed by the rest of
the combination. A terminal will typically report a combination using
the Alt or Option key as that Esc sequence. In a ``Ctl-'' combination,
the letter case of the key doesn't matter (i.e., doesn't require
holding the Shift key).

@subsection*{Evaluation, Indentation, and Completion}

@binding-table[

  @key[("Return" "^J") ee-newline/accept]{Reads and evaluates the
       current entry, if it is complete, and otherwise inserts a
       newline and auto-indents. The notion of ``complete'' depends on
       a language, but typically includes requirements like no
       unbalanced parentheses.}

  @key[("Esc-Return") ee-newline]{Inserts a newline and indents.}
       
  @key[("Esc-^J") ee-accept]{Reads and evaluates the current entry,
       even if it is not otherwise recognized as complete.}

  @key["^O" ee-open-line]{Creates a new line for input, similar to a
       non-accepting @onekey{Return}, but does not move the cursor to
       the new line or indent.}

  @key["Tab" ee-id-completion/indent]{Either indents or completes,
       depending on the text before the cursor. If no text is present
       before the cursor on the same line, then the line is indented
       or cycled to the next possible indentation. If the cursor is
       after an identifier, it's completed or a list of possible
       completions is shown. Completion depends on the language, but
       it is typically drawn from the set of available top-level
       bindings.}

  @key["^R" ee-next-id-completion]{Steps through the next possible
       completion when there are multiple possible completions.}

  @key["Esc-Tab" ee-indent]{Indents the current line or cycles though
       possible indentations. The cursor is moved to just after the
       indentation before the rest of the line content.}

  @key[("Esc-q" "Esc-Q" "Esc-^Q") ee-indent-all]{Reindents the full editor
       region.}

]

@subsection*{Navigation}

@binding-table[

  @key[("Left" "^B") ee-backward-char]{Moves the cursor back
       one character.}
       
  @key[("Right" "^F") ee-forward-char]{Moves the cursor forward
       one character.}
       
  @key[("Up" "^P") ee-previous-line]{Moves the cursor up to the
       previous line---unless the cursor is at the start of the
       editor-region, in which case replaces the editor region with the
       previous history entry.}

  @key[("Down" "^N") ee-next-line]{Moves the cursor down to the next
       line---unless the cursor is at the end of the editor region, in
       which case replaces the editor region with the next history
       entry.}
       
  @key[("Home" "^A") ee-beginning-of-line]{Moves the cursor to the
       start of the current line.}
       
  @key[("End" "^E") ee-end-of-line]{Moves the cursor to the
       end of the current line.}

  @key[("PageUp" "^X[") ee-backward-page]{Moves the cursor up to the
       previous page.}
       
  @key[("PageDown" "^X]") ee-backward-page]{Moves the cursor down to the
       next page.}

  @key["Esc-<" ee-beginning-of-entry]{Moves the cursor to the
       start of the editor region.}
       
  @key["Esc->" ee-end-of-entry]{Moves the cursor to the
       end of the editor region.}

  @key[("Esc-f" "Esc-F" "^Right") ee-forward-word]{Moves the cursor
       forward one whitespace-delimited word.}
       
  @key[("Esc-b" "Esc-B" "^Left") ee-backward-word]{Moves the cursor
       backward one whitespace-delimited word.}

  @key["Esc-]" ee-goto-matching-delimiter]{Moves the cursor to the
       opener or closer opposite the one under the cursor.}
       
  @key["^]" ee-flash-matching-delimiter]{Flashes the cursor on the
       opener or closer opposite the one under the cursor.}

  @key[("Esc-^F" "Esc-^Right") ee-forward-exp]{Moves the cursor forward
       one expression, where the definition of ``expression'' is
       language-specific.}
       
  @key[("Esc-^B" "Esc-^Left") ee-backward-exp]{Moves the cursor backward
       one language-specific expression.}

  @key[("Esc-^U") ee-upward-exp]{Moves the cursor upward/outward
       one language-specific expression.}
       
  @key[("Esc-^D") ee-downward-exp]{Moves the cursor downward/inward
       one language-specific expression.}

  @key["^X-^X" ee-exchange-point-and-mark]{Moves the cursor to the
       location of the @tech{mark} while setting the @tech{mark} to
       the cursor's current position.}

]

@subsection*{History}

@binding-table[

  @key[("Esc-Up" "Esc-^P") ee-history-bwd]{Replaces the editor region with the
       previous history entry.}

  @key[("Esc-Down" "Esc-^N") ee-history-fwd]{Replaces the editor region with the
       next history entry.}
  
  @key[("Esc-p") ee-history-bwd-prefix]{Replaces the editor region
       with the previous history entry that starts the same as the current
       editor content.}

  @key[("Esc-P") ee-history-bwd-contains]{Replaces the editor region
       with the previous history entry that includes the same as the
       current editor content.}
       
  @key[("Esc-n") ee-history-fwd-prefix]{Replaces the editor region
       with the next history entry that starts the same as the current
       editor content.}

  @key[("Esc-N") ee-history-fwd-contains]{Replaces the editor region
       with the next history entry that includes the same as the
       current editor content.}

]

@subsection*{Deletion, Insertion, and Transposition}

@binding-table[

  @key[("Backspace" "^H") ee-backward-delete-char]{Deletes the previous
       character, if any.}

  @key["^D" ee-eof/delete-char]{Deletes the next character, if
       any---unless the editor region is empty, in which case returns
       an end-of-file as the input.}

  @key["Delete" ee-delete-char]{Deletes the next character, if
       any.}

  @key["^U" ee-delete-line]{Deletes the content of the current line,
       no matter where the cursor is within the line.}
  
  @key[("^K" "Esc-k") ee-delete-to-eol]{Deletes the content of the
       current line following the cursor, or merges the next line with
       the current one if the cursor is at the end of the line.}
  
  @key[("^G") ee-delete-entry]{Deletes the full content of the editor
       region.}
  
  @key[("^C") ee-reset-entry/break]{Deletes the full content of the editor
       region, and also moves to the end of the history---unless the editor
       region is empty, in which case a break signal is sent to the current
       thread.}

  @key[("Esc-d") ee-delete-word]{Deletes one whitespace-delimited word
       after the cursor.}

  @key[("Esc-Delete" "Esc-^K") ee-delete-exp]{Deletes one expression
       after the cursor, where the definition of ``expression'' is
       language-specific.}

  @key[("Esc-Backspace" "Esc-^H") ee-backward-delete-exp]{Deletes one
       expression before the cursor.}

  @key[("^@" "^^") ee-set-mark]{Set the @tech{mark} to be the same
       position as the cursor. The @deftech{mark} is a kind of second
       cursor, but invisible, that is used by various editing
       operations.}

  @key["^W" ee-delete-between-point-and-mark-or-backward]{Deletes content between
       the cursor and the @tech{mark}. When no mark is set, deletes
       one expression before the cursor.}

  @key["^Y" ee-yank-kill-buffer]{Inserts content previously deleted,
       where multiple consecutive deletions accumulate to one set of
       content to insert.}

  @key["^V" ee-yank-selection]{Inserts the content of the system
       clipboard.}

  @key["^T" ee-transpose-word]{Transposes space-delimited words to the
       left and right of the cursor.}

  @key["Esc-^T" ee-transpose-exp]{Transposes language-specific
       expressions to the left and right of the cursor.}

]

@subsection*{Process Control}

@binding-table[

  @key["^L" ee-redisplay]{Refreshes the editor region's display.}

  @key["^Z" ee-suspend-process]{Suspends the current process.}

]
