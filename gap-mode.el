;; GAP Programming mode for automatic indentation of GAP code.
;;
;; Michael Smith                        smith@pell.anu.edu.au
;; Australian National University
;; February 1993
;;
;;! version 1.96, 23:10 Thu 11 May 1995
;;!
;;
;;
;; Major mode for writing GAP programs.
;; Provides automatic indentation of GAP code.
;;
;; Installation:
;;   Copy this file to somewhere in your load path, then put the
;;   following lines in your .emacs file:
;;
;;      (autoload 'gap-mode "gap-mode" "GAP editing mode" t)
;;      (setq auto-mode-alist (apply 'list
;;                                   '("\\.g$" . gap-mode)
;;                                   '("\\.gap$" . gap-mode)
;;                                   auto-mode-alist))
;;
;; Then visiting any file ending in ".g" or ".gap" will automatically put
;; you in gap-mode.  Alternatively, to enter gap-mode at anytime, just type
;;    M-x gap-mode
;;
;; While in gap-mode, type "C-h m" for help on its features.
;;
;;! ----------------------------------------------------------------------
;;! v1.96 -
;;! * Added a flag to choose whether the complete command (ESC-tab) simply
;;!   calls dynamic abbreviation (dabbrev-expand), the default, or tries
;;!   to complete the word by asking a running gap process.
;;! v1.95 -
;;! * Fixed bug in 'gap-insert-local-variables. It was only picking up
;;!   variables that were the first on the line - a big problem.
;;!   Finally fixed the treatment of local function definitions. It will
;;!   now skip over locally defined functions when compiling the local
;;!   variable list.
;;! v1.92 -
;;! * Defined my own "memberequal" function for checking strings in lists.
;;! v1.90 -
;;! * Fixed a bug in gap-insert-local-variables (stray "," appearing).
;;! * Added variables gap-local-statement-format and
;;!   gap-local-statement-margin for controlling format of local
;;!   variable statement inserted. It now wraps the line correctly.
;;! v1.85 -
;;! * Added variables gap-insert-debug-name, gap-insert-debug-string to
;;!   allow customization of debugging/print statements inserted by function
;;!   gap-insert-debug-print.
;;! v1.80 -
;;! * New function gap-insert-local-variables for inserting a local variable
;;!   statement for the current function at the point.
;;! v1.70 -
;;! * New function gap-insert-debug-print for inserting Inform(... lines.
;;! v1.60 -
;;! * Fixed the add-local-variable function so that it skips over local
;;!   statements of functions defined within the current function.
;;! v1.55 -
;;! * Added a regular expression for gin-mode, and changed the fill region
;;!   function to check if gin-mode is on, if so and in a comment, do
;;!   fill-paragraph instead of indent region. Does this make sense?
;;! v1.51 -
;;! * Fixed silly error due to copying a magma-mode function across.
;;! v1.50 -
;;! * Fixed the function that leaps across if..else..fi and similar stmts.
;;! v1.40 -
;;! * Added new function 'gap-add-local-variable.
;;! v1.30 -
;;! * changed code to make it more compatible with outline-minor-mode.
;;!   Many changes to regular expressions, adding "\C-m" whenever "\n"
;;!   occurs, and modifiying many beginning-of-line etc functions.
;;! v1.25 -
;;! * eliminated bug introduced in last modification.
;;! v1.20 -
;;! * Made the special continued line handling more versatile.
;;! v1.10 -
;;! * Cleaned up code immensely. Should be much easier to understand.
;;! * Fixed some bugs in special indentation checking where it could get
;;!   confused with the contents of GAP strings (eg a ":=" in a string).
;;! v1.01 -
;;! * Just changed some defaults.
;;! v1.00 -
;;! * First release version.

;;! Autoload functions from gap-process.
(require 'gap-process)

;;{{{ defcustoms/defvars

(defgroup gap nil
  "Support for the GAP programming language."
  :group 'languages
  :prefix "gap"
  ;; :link '(function-link gap-mode)
  )

(defcustom gap-indent-brackets t
  "Whether to check back for unclosed brackets in determining
indentation level. This is good for formatting lists and matrices."
  :group 'gap
  :type 'boolean
  :safe t)

;; TODO: make integer or nil
(defcustom gap-bracket-threshold 8
  "If indentation due to bracketing will indent more than this value,
use this value instead.  nil is equivalent to infinity."
  :group 'gap
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Bracket indentation limit"))
  :safe t)

(defcustom gap-indent-step 4
  "Amount of extra indentation for each level of grouping in GAP code."
  :group 'gap
  :type 'integer
  :safe t)

(defcustom gap-indent-step-continued 2
  "Amount of extra indentation to add for normal continued lines."
  :group 'gap
  :type 'integer
  :safe t)

;; TODO: make sure this actually works once we restart
(defcustom gap-indent-comments t
  "Variable controlling how the indent command works on comments.  A comment
will be indented to the next tab-stop if gap-indent-comments is:
  0    and the cursor is on the # character
  1    and the cursor is 1 character to the right of the # character
  t    and the cursor is anywhere to the right of the # character
If nil then use calculated indentation level only."
  :group 'gap
  :type '(choice (const :tag "Cursor on the # character" 0)
                 (integer :tag "Cursor n characters to the right of #")
                 (other :tag "Cursor to the right of the # character" t)
                 (const :tag "Calculated indentation only" nil))
  :safe t)

(defcustom gap-indent-comments-flushleft nil
  "If non-nil then indent comments based on `gap-indent-comments' regardless
of whether the comment is flush-left or not.  Set this to nil to treat
flush-left comments as special---i.e. not to be indented by pressing TAB."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-auto-indent-comments t
  "Controls whether the region indentation commands will change
indentation of comment lines."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-pre-return-indent t
  "If non-nil indent the line before breaking to next line on RET keypress."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-post-return-indent t
  "If non-nil autoindent after a RET keypress."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-electric-semicolon t
  "If non-nil a semicolon will create a newline as well.
At the beginning of a line, instead add semicolon to the end of the
previous line so that two semicolons in a row does the right thing."
  :group 'gap
  :type 'boolean
  :safe t)

;; TODO: this probably shouldn't be a defcustom, but I don't know enough about `gin-mode'
;; I think if it's not used anymore, then I'll just rip it out.
(defcustom gin-retain-indent-re "[ \t]*#+[ \t]*\\|[ \t]+"
  "Regular expression for gin-mode's filling command to allow it to
fill GAP comments"
  :group 'gap
  :type 'regexp
  :safe t)

(defcustom gap-fill-if-gin nil
  "Set to t to intelligently fill paragraphs if point is in comment and
indent region command is run."
  :group 'gap
  :type 'boolean
  :safe t)

;; TODO: this could have a better type
(defcustom gap-tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44
                                 48 52 56 60 64 68 72 74 78)
  "Gap-mode tab-stop-list.  Note this is effectively only used in the
indentation of comments---all GAP code indentation depends on the
variable gap-indent-step."
  :group 'gap
  :type '(repeat integer)
  :safe t)

(defcustom gap-mode-hook nil
  "Function to be called after turning `gap-mode' on for buffer."
  :group 'gap
  :type 'hook
  :safe t)

(defcustom gap-local-statement-format '(3 2)
  "Two element list determining format of local var statement inserted.
First element is number of spaces after \"local\", the second is number
of spaces after each comma."
  :group 'gap
  :type '(list integer integer)
  :safe t)

(defcustom gap-local-statement-margin (if fill-column fill-column 75)
  "Column at which to wrap local variable statement."
  :group 'gap
  :type 'integer
  :safe t)

(defcustom gap-local-statements-at-beginning nil
  "If non-nil then local variable statements should be inserted
on first line of a function instead of the previous line."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-regenerate-local-statements nil
  "If non-nil then local variable statements will be
'regenerated' instead if they already exist instead of
unconditinally inserted."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-insert-debug-name "Info"
  "Function name to use when inserting a debugging/print statement."
  :group 'gap
  :type 'string
  :safe t)

(defcustom gap-insert-debug-string "#I  %s: "
  "String to use when inserting a debugging/print statement.
A %s is substituted with the name of the current function."
  :group 'gap
  :type 'string
  :safe t)

;; TODO: make a gap-completion-function which could be dabbrev or not.
;; Then deprecate this
(defcustom gap-use-dabbrev t
  "If true then the complete command will simply call dabbrev instead
of communicating with a running GAP process."
  :group 'gap
  :type 'boolean
  :safe t)

(defcustom gap-debug-indent nil
  "Show the facts that gap-indent bases its decision on."
  :group 'gap
  :type 'boolean
  :safe t)

;;}}}
;; TODO: Add function to create documentation block
;;{{{ gap-mode, syntax and font-lock

(defvar gap-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table) ;; cope with outline mode
    ;; operators
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?~  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    ;; TODO: make sure this comment is correct, or just make . into punctuation
    ;; Symbol (sort of a hack so that x.y is a single symbol for help purposes)
    ;; We will make .. into punctuation later
    (modify-syntax-entry ?.  "_" table)
    table)
  "Syntax table used while in gap mode.")

(defvar gap-font-lock-syntactic-keywords
  '(("\\.\\." 0 ".")              ; Make .. into puctuation
    ("\\\\."  0 "_")              ; Make \character a symbol character
    ;; Make 'c' and '\n' into strings
    ("\\('\\)\\([^\\]\\|\\\\.\\)\\('\\)"
     (1 "\"")
     (2 "w")
     (3 "\""))))

(defvar gap-font-lock-keywords
  `(;; Keywords
    (,(concat "\\_<"
              (regexp-opt
               (list "and" "do" "elif" "else" "end" "fi" "for"
                     "function" "if" "in" "local" "mod" "not"
                     "od" "or" "repeat" "return" "then" "until"
                     "while" "quit" "QUIT" "break" "rec" "continue"))
              "\\_>")
     . font-lock-keyword-face)

    ;; "Special" keywords
    ;; Should true/false/fail be font-lock-constant-face?
    (,(concat "\\_<"
              (regexp-opt
               (list "IsBound" "Unbind" "TryNextMethod" "Info" "Assert" "SaveWorkspace"
                     "true" "false" "fail" ))
              "\\_>")
     . font-lock-builtin-face)

    ;; Functions -- based on capitalization and proximity to parenthesis
    ("\\_<\\([A-Z]\\(?:\\w\\|\\s_\\)+\\)\\s *(" 1 font-lock-function-name-face nil)

    ;; Functions -- based on assignment
    ("\\_<\\(\\(?:\\w\\|\\s_\\)+\\)\\s *:=\\s *\\(function\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-keyword-face)) ; Shouldn't funcion already be highlighted?

    ;; Variables as they are assigned
    ("\\_<\\(\\(?:\\w\\|\\s_\\)+\\)\\s *\\(:=\\)"
     (1 font-lock-variable-name-face)
     (2 'bold))

    ;; Docstrings for GAP functions seem to have the following format.
    ;; I don't know what the different letters mean, so assume any are valid.
    ("^#\\([A-Z]\\)\\s +\\(.*\\)"
     (1 font-lock-warning-face t)
     (2 'font-lock-doc-string-face t))
    ("^##\\s +\\(.*\\)"
     1 'font-lock-doc-string-face t)

    ;; TODO: could use an eval form to scan the buffer for function
    ;; declarations and mark them...
    )
  "Font lock Keywords for GAP.
For format of ths variable see `font-lock-keywords'.")

(defvar gap-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Indenting, formatting
    (define-key map ";"       'gap-electric-semicolon)
    (define-key map [return]  'gap-newline-command)
    (define-key map "\M-q"    'gap-format-region)
    (define-key map "\M-\C-q" 'gap-format-buffer)
    (define-key map "\C-c#"   'comment-or-uncomment-region)
    ;; Inserting
    (define-key map "\C-cl"   'gap-insert-local-variables)
    (define-key map "\C-ca"   'gap-add-local-variable)
    (define-key map "\C-cd"   'gap-insert-debug-print)
    ;; Marking and moving by blocks
    (define-key map "\C-c%"   'gap-match-group)
    ;; Interpreter
    (define-key map [M-tab]   'gap-completion) ; Should probably use
    (define-key map "\M-?"    'gap-help)
    ;; Evaluating things in the interpreter
    (define-key map "\C-ce"    'gap-eval-defun)
    (define-key map "\C-c\C-e" 'gap-eval-last-statement)
    (define-key map "\C-c\C-r" 'gap-eval-region)
    (define-key map "\C-c\C-f" 'gap-eval-buffer)
    ;; Menu
    (easy-menu-define gap-menu map "GAP Mode menu"
      `("GAP" :help "GAP-specific Features"
        ["Format region" gap-format-region :active mark-active
         :help "Indent and format comments in region"]
        ["Format buffer" gap-format-buffer
         :help "Indent and format comments in buffer"]
        ["Comment Region" comment-region :active mark-active
         :help "Comment region"]
        ["Uncomment Region" uncomment-region :active mark-active
         :help "Uncomment region"]
        "-"
        ["Insert Local Variables" gap-insert-local-variables
         :help "Insert statement local variables"]
        ["Add Local Variable" gap-add-local-variable
         :help "Insert statement local variables"]
        ["Insert Debug Statement" gap-insert-debug-print
         :help "Insert debug print statement"]
        "-"
        ;; ["Mark def/class" mark-defun
        ;;  :help "Mark innermost definition around point"]
        ;; ["Start of block" python-beginning-of-block
        ;;  :help "Go to start of innermost definition around point"]
        ;; ["End of block" python-end-of-block
        ;;  :help "Go to end of innermost definition around point"]
        ;; ["Start of def/class" beginning-of-defun
        ;;  :help "Go to start of innermost definition around point"]
        ;; ["End of def/class" end-of-defun
        ;;  :help "Go to end of innermost definition around point"]
        ["Jump to matching beginning/end of grouping" gap-match-group
         :help "Mark innermost definition around point"]
        "-"
        ["Start GAP interpreter or switch to it" gap
         :help "Run inferior GAP in separate buffer"]
        ["Eval buffer" gap-eval-buffer :active (gap-okay-to-run)
         :help "Evaluate entire buffer in inferior GAP session"]
        ["Eval region" gap-eval-region :active (and mark-active (gap-okay-to-run))
         :help "Evaluate region in inferior GAP session"]
        ["Eval function" gap-eval-defun :active (gap-okay-to-run)
         :help "Evaluate current function definition in inferior GAP session"]
        ["Eval last statement" gap-eval-last-statement :active (gap-okay-to-run)
         :help "Evaluate statement before point in inferior GAP session"]
        "-"
        ["Help on symbol" gap-help :active (gap-okay-to-run)
         :help "Use inferior GAP to get help for symbol at point (if running)"]
        ["Complete symbol" gap-completion :active (gap-okay-to-run)
         :help "Complete (qualified) symbol before point"]
        ))
    map))

;; TODO: make beginning/end-of-block commands
(define-derived-mode gap-mode fundamental-mode "GAP"
  "Major mode for writing GAP programs.  The following keys are defined:

 \\[gap-indent-command]      to intelligently indent current line.
 \\[gap-newline-command]      newline with indentation of current and new line.
 \\[gap-format-region]    to indent each line of the region.
 \\[gap-format-buffer]  to indent each line of the whole buffer.
 \\[gap-match-group]    to find matching beginning or end of grouping at point.
          See the documentation for command gap-match-group.
 \\[gap-comment-region]   to comment out region: with arg to uncomment region.
 \\[gap-add-local-variable]   to add identifier to local variables of function.
 \\[gap-insert-local-variables]   to insert a local variables statement for the current function.

If a GAP process is running in buffer *gap*, then also:

 \\[gap-completion]  complete identifier at point
 \\[gap-help]  get GAP help on (any) topic

Variables: (with default given)

  gap-indent-step = (default 4)
        the amount of indentation to add at each level of a group

  gap-indent-step-continued =  (default 2)
        the extra indentation for continued lines that aren't special
        in some way.

See also the documentation for the variables:
  gap-pre-return-indent
  gap-post-return-indent
  gap-indent-comments
  gap-indent-comments-flushleft
  gap-auto-indent-comments
  gap-indent-brackets
  gap-bracket-threshold
  gap-tab-stop-list
  gap-mode-hook

and documentation for the functions:
  gap-percent-command

The indentation style is demonstrated by the following example,
assuming default indentation variables:

test := function (x,y)
    # this is a test
    local n,
          m,
          x;
    if true then
        Print( \"if true then \",
               \"nothing\");
    fi;
    x := [ [ 1, 2, 3 ],
           [ 5, 6, 8 ],
           [ 9, 8, 7 ] ];
    y := 1 + 2 + 3 +
         4 + 5 + 6;
    z := Filtered( List( origlist,
               x -> f( x + x^2 + x^3 + x^4 + x^5,
                       x^-1, x^-2, x^-3)),
               IsMat);
end;"
  :group 'gap
  :syntax-table gap-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(gap-font-lock-keywords nil nil nil nil
                                (font-lock-syntactic-keywords
                                 . gap-font-lock-syntactic-keywords)))
  (set (make-local-variable 'indent-line-function)
       'gap-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'gap-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'gap-end-of-defun)
  (set (make-local-variable 'comment-start) "#")
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'tab-stop-list) gap-tab-stop-list))

;;}}}
;;{{{ user commands

;; TODO: make sure xemacs has this
(make-obsolete 'gap-comment-region 'comment-or-uncomment-region "Sep 4 2010")
(defun gap-comment-region (arg p1 p2)
  "Comment region, or with `prefix-arg' uncomment region.
There is nothing specific to GAP in this code except that the
comment marker is set to be #.

See `comment-or-uncomment-region' for a more general version, as
well as `comment-region' and `uncomment-region' at least in
GNU/emacs."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region (beg-of-line-from-point p1)
                        (end-of-line-from-point p2))
      (goto-char (point-min))
      (let ((first t))
        (while (or first
                   (re-search-forward "[\n\C-m]" nil t))
          (setq first nil)
          (cond ((= arg 1)
                 (insert "#"))
                ((and (> arg 1)
                      (looking-at "#"))
                 (delete-char 1))))))))

(defun gap-newline-command ()
  "Insert newline optionally indenting before and after.
The behavior is determined by the variables
`gap-pre-return-indent' and `gap-post-return-indent'."
  (interactive)
  (open-line 1)
  (when gap-pre-return-indent
    (gap-indent-line))
  (forward-char 1)
  (when gap-post-return-indent
    (gap-indent-line)
    (back-to-indentation)))

(defun gap-electric-semicolon (arg)
  "Insert a semicolon and run `gap-indent-line'.
With numeric ARG, just insert that many semicolons.  With
 \\[universal-argument], just insert a single colon."
  (interactive "*P")
  (cond ((or arg (not gap-electric-semicolon))
         (self-insert-command (if (not (integerp arg)) 1 arg)))
        ((and gap-electric-semicolon
              (<= (current-column)
                 (save-excursion (back-to-indentation) (current-column))))
         (save-excursion
           (forward-line -1)
           (end-of-line)
           (self-insert-command 1)))
        (t
         (self-insert-command 1)
         (gap-newline-command))))

(defun gap-indent-line ()
  "Indent current line intelligently according to GAP semantics.
Affected by the variables `gap-indent-step',
`gap-indent-step-continued', `gap-indent-brackets', and
`gap-bracket-threshold'.  Printing of indentation information is
enabled by `gap-debug-indent'."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let ((cur (current-column))
          (ind (gap-calculate-indent)))
      (if (= cur ind)
          nil
        (indent-to-left-margin)
        (indent-to ind))))
  (if (< (current-column)
         (save-excursion (back-to-indentation) (current-column)))
      (back-to-indentation)))

(defun gap-indent-command (col)
  "Smart GAP indent command.
With `prefix-arg' indents this line to column given by argument.
If line is a comment starting in column 1 then do nothing.
If point is immediately following a comment character (#) then
call tab-to-tab-stop, which moves comment up to four characters
right (default).  Otherwise indent the line intelligently by
calling `gap-indent-line'.

The variable `indent-line-function' is `gap-indent-command' by
default so that `indent-for-tab-command' works intelligently.

Behaviour depends on the gap-mode variables `gap-tab-stop-list',
`gap-indent-comments', and `gap-indent-comments-flushleft', as
well as those affecting behavior of `gap-indent-line'."
  (interactive "P")
  (if col
      (progn
        (back-to-indentation)
        (indent-to col))
    (if (and (not gap-indent-comments-flushleft)
             (save-excursion
               (beginning-of-line)
               (looking-at "#")))
        nil
      (if (or (and (numberp gap-indent-comments)
                   (= (char-after (- (point) gap-indent-comments)) ?#))
              (and gap-indent-comments
                   (gap-point-in-comment)))
          (progn
            (save-excursion
              (beginning-of-line)
              (while (not (gap-point-in-comment))
                (re-search-forward "#"))
              (forward-char -1)
              (to-tab-stop)
              (message (format "column %d" (current-column)))
              (forward-char 1)))
        (gap-indent-line)))))

;; TODO: This command seems schizophrenic
(defun gap-format-region ()
  "Indent all lines in the region or call `fill-paragraph'.

If `gap-fill-if-gin' is non-nil, and point is in a comment, then
calls `fill-paragraph'.  The variable `gin-retain-indent-re' is
used to ensure that indentation is preserved in this case.
Otherwise calls `gap-indent-line' on each non-comment line and on
each comment line if `gap-auto-indent-comments' is non-nil."
  (interactive)
  ;; Make it compatible with gin-mode, in the sense that if gap-fill-if-gin
  ;; is true, and buffer is in gin-mode, and point is in comment, then do
  ;; fill paragraph instead of indenting region.
  (if (and gap-fill-if-gin
           (boundp 'gin-mode)
           gin-mode
           (gap-point-in-comment))
      (fill-paragraph nil)
    (let (ret p)
      (if (> (point) (mark))
          (exchange-point-and-mark))
      (setq p (point))
      (gap-indent-line)
      (while (re-search-forward "[\n\C-m]" (end-of-line-from-point (mark)) t)
        (if (gap-looking-at "^[ \t]*[\n\C-m]")
            (indent-to-left-margin)
          (if (and (not gap-auto-indent-comments)
                   (gap-looking-at "^[ \t]*#"))
              nil
            (gap-indent-line))))
      (goto-char p)
      (exchange-point-and-mark))))

(defun gap-format-buffer ()
  "Calls `gap-format-region' on entire buffer."
  (interactive)
  (set-mark (point-max))
  (goto-char (point-min))
  (gap-format-region))

(defun gap-insert-local-variables (&optional arg)
  "Insert a local variable statement for the current function.
With `prefix-arg' temporarily invert the value of
`gap-regenerate-local-statements'.

If `gap-regenerate-local-statements' is non-nil then regenerate
an existing local statement or insert a new one.

If `gap-local-statements-at-beginning' is non-nil the local
statement is inserted on the first line after the argument list
of the function definition.  Otherwise the local statement is
inserted before the line the cursor is on.  This function assumes
that a variable is local if occurs on the left-hand side of an
assignment statement or occurs as the index variable of a do
loop.  You may have to trim globals from the list if you assign
values to them.

This function will skip over any embedded local function declarations, and
may be invoked within a local function definition to generate a local
statement for that function.

Formatting of the local statement is determined by
`gap-local-statement-format' and `gap-local-statement-margin'."
  ;; Not very efficient, but it seems to work
  (interactive "P")
  (let ((formal nil)
        (names nil)
        (regenerate (if arg
                        (not gap-regenerate-local-statements)
                      gap-regenerate-local-statements))
        p1 p2 name)
    ;; Find variables
    (save-excursion
      (if (not (gap-find-matching "\\<function\\>" "\\<end\\>" nil t t))
          (error "no end of function!"))
      (setq p2 (point))
      (if (not (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1 t))
          (error "no beginning of function"))
      (if (not (looking-at "function *("))
          (error "bad beginning of function"))
      (goto-char (match-end 0))
      (while (looking-at " *\\([a-z][a-z0-9_]*\\),?")
        (setq formal (append formal
                             (list (buffer-substring
                                    (match-beginning 1) (match-end 1)))))
        (goto-char (match-end 0)))
      (while (gap-searcher 're-search-forward
                           (concat
                            "\\(" "\\(^\\|;\\) *\\([a-z][a-z0-9_]*\\) *:= *"
                            "\\|" "\\(^\\|;\\) *for +\\([a-z][a-z0-9_]*\\)"
                            " +in\\>" "\\)")
                           p2 t '(match-beginning 0))
        (cond ((looking-at "\\(^\\|;\\) *\\([a-z][a-z0-9_]*\\) *:= *")
               (setq name (buffer-substring (match-beginning 2) (match-end 2)))
               (goto-char (match-end 0))
               (if (looking-at "function *(")
                   (progn
                     (goto-char (match-end 0))
                     (if (not (gap-find-matching "\\<function\\>"
                                                 "\\<end\\>" nil t t))
                         (error "No local function end?!")))))
              ((looking-at "\\(^\\|;\\) *for +\\([a-z][a-z0-9_]*\\) +in\\>")
               (setq name (buffer-substring (match-beginning 2) (match-end 2)))
               (goto-char (match-end 0)))
              (t (error "gap-insert-local-variables incorrect code!")))
        (if (not (memberequal name names))
            (setq names (append names (list name))))))
    (save-excursion
      ;; Goto to insertion point
      (if (not gap-local-statements-at-beginning)
          (beginning-of-line)
        (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1 t)
        (forward-sexp 2)
        (forward-line))
      (when regenerate
        (let ((p (point)))
          ;; Search forwards then backwards for local command.
          ;; For some reason the other direction doesn't work
          ;; TODO: should take a page from gap-add-local-variable
          (gap-find-matching "\\<function\\>" "\\<end\\>" "\\<local\\>" t t) ; goto begin
          (when (looking-at "end")
            (gap-find-matching "\\<function\\>" "\\<end\\>" "\\<local\\>" -1 t))
          ;; If we found a local statement, delete it, else return to where we were
          (if (looking-at "local")
              (delete-region (point)
                             (progn (gap-search-forward-end-stmt p2 1 'end)
                                    ;; Delete a "newline" since we are going to insert it
                                    (when (looking-at "\\s \\|$")
                                      (forward-char 1))
                                    (point)))
            (goto-char p))))
      ;; Insert the local statement (if any)
      (let (lnames)
        (while (car names)
          (if (memberequal (car names) formal)
              (setq names (cdr names))
            (setq lnames (append lnames (list (car names))))
            (setq names (cdr names))))
        (if (not lnames)
            (error "No local variables!")
          (insert "local")
          (insert-char ?  (nth 0 gap-local-statement-format))
          (gap-indent-line)
          (while (car lnames)
            (if (< (+ (current-column) (length (car lnames)))
                   gap-local-statement-margin)
                (insert (car lnames))
              (insert "\n" (car lnames))
              (gap-indent-line))
            (setq lnames (cdr lnames))
            (if lnames
                (progn
                  (insert ",")
                  (insert-char ?  (nth 1 gap-local-statement-format)))))
          (insert ";\n"))))))

;; TODO: add an option to automatically insert the local statement if not found.
(defun gap-add-local-variable (ident)
  "Add a local variable to the local statement of the current function.
Prompts for name with default the identifier at the point. If
there is no local variable statement yet, signals error."
  (interactive
   (let ((enable-recursive-minibuffers t)
         (try-word (gap-ident-around-point))
         val)
     (setq val (read-string (format "Variable name (default %s): "
                                    try-word)))
     (if (string-equal val "")
         (setq val try-word))
     (list val)))
  (save-excursion
    (let ((pos (point))
          local-start)
      (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1)
      (goto-char (match-end 0))
      (gap-find-matching "\\<function\\>" "\\<local\\>" "\\<function\\>" t)
      (if (not (looking-at "local"))
          (error "No local statement. Add one first.")
        (setq local-start (point))
        (gap-search-forward-end-stmt pos 1 'end)
        (forward-char -1)
        (if (save-excursion
              (re-search-backward (concat "\\<" ident "\\>")
                                  local-start t))
            (error "The variable '%s' is already in the local statement." ident)
          (insert ", " ident))))))

(defun gap-insert-debug-print ()
  "Insert a print statement for debugging purposes.
The statement inserted depends on `gap-insert-debug-name' and
`gap-insert-debug-string'."
  (interactive)
  (let (name)
    (save-excursion
      (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1)
      (beginning-of-line)
      (setq name (gap-ident-around-point)))
    (beginning-of-line)
    (open-line 1)
    (indent-to (gap-calculate-indent))
    (insert gap-insert-debug-name "( \""
            (format gap-insert-debug-string name) "\" );")
    (backward-char 3)))

;; TODO: hook this into more general completion
(defun gap-completion (&optional full)
  "Try to complete word at point.
if `gap-use-dabbrev' is non-nil call `dabbrev-expand' (dynamic
abbreviation).  Otherwise contact a running gap process to get a
GAP completion of the word."
  (interactive "*")
  (if gap-use-dabbrev
      (dabbrev-expand full)
    (gap-complete full) ;; defined in gap-process.el
    ))

(defun to-tab-stop ()
  "Version of tab-to-tab-stop that inserts before point."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (let ((tabs tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
        (insert-before-markers
         (make-string (- (car tabs) (current-column)) 32))
      (insert ? ))))

(defun gap-match-group ()
  "Find matching delimiter in GAP
If point is on a character with bracket syntax, then use built in
lisp function forward-list to find matching bracket.  Otherwise,
check to see if point is on the first character of 'do', 'od',
'if', 'elif', 'else', 'fi', 'function', 'end'.  If it is, jump to
the matching delimiter."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1) t)
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1) t)
        ((not (gap-point-in-comment-string))
         (cond ((looking-at "\\<if\\>")
                (goto-char (match-end 0))
                (gap-find-matching "\\<if\\>" "\\<fi\\>"
                                   "\\<\\(else\\|elif\\)\\>" t))
               ((looking-at "\\<fi\\>")
                (gap-find-matching "\\<if\\>" "\\<fi\\>" nil -1))
               ((looking-at "\\<\\(else\\|elif\\)\\>")
                (goto-char (match-end 0))
                (gap-find-matching "\\<if\\>" "\\<fi\\>"
                                   "\\<\\(else\\|elif\\)\\>" t))
               ((looking-at "\\<do\\>")
                (goto-char (match-end 0))
                (gap-find-matching "\\<do\\>" "\\<od\\>" nil t))
               ((looking-at "\\<od\\>")
                (gap-find-matching "\\<do\\>" "\\<od\\>" nil -1))
               ((looking-at "\\<function\\>")
                (goto-char (match-end 0))
                (gap-find-matching "\\<function\\>" "\\<end\\>" nil t))
               ((looking-at "\\<end\\>")
                (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1))
               (t nil)))
        (t nil)))

(defun gap-percent-command (arg)
  "This Gap-mode function is for people who are used to the % command in vi.
Binding this function to the '%' key in Gap-mode will: match whatever beginning
or end of a group that the point is on, otherwise just insert a % symbol."
  (interactive "p")
  (if (not (gap-match-group))
      (self-insert-command (or arg 1))))

;; This seems really innefficient and inelegent, but it's fast enough
;; OMM, so I'm going to stick with it for now
(defun gap-beginning-of-defun (arg)
  "Function to use for `beginning-of-defun-function'."
  (interactive "^p")
  ;; move inside the function definition if at the beginning of a line
  (end-of-line)
  ;; If we are not inside a defun, then skip to the inside of the previous one
  (and (not (save-excursion
              (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1 t)))
       (gap-find-matching "\\<function\\>" "\\<end\\>" "\\<end\\>" -1 t)
       (backward-char 1))
  ;; Work our way out to the outermost block
  (let ((p (point)))
    (while (save-excursion
             (and (gap-find-matching "\\<function\\>" "\\<end\\>" nil -1 t)
                  (looking-at "function[^(]*(")
                  (or (forward-char 1) t)   ; just so we don't match the same
                  (gap-find-matching "\\<function\\>" "\\<end\\>" nil t t)
                  (> (point) p)             ; We have to enclose the point we are at
                  (setq p (point))))
      (goto-char p)
      (forward-char 4)
      ))
  (backward-char 4)
  ;; We are at end of function
  ;; Handle moving forward
  (while (and (< arg 0)
              (gap-search-forward-end-stmt nil 1 'end) ; Move past end
              (gap-find-matching "\\<function\\>" "\\<end\\>" "\\<function\\>" t t)
              (gap-match-group))
    (setq arg (1+ arg)))
  ;; Goto beginning of function
  (gap-match-group)
  ;; Handle moving backwards
  (while (and (> arg 1)
              (gap-find-matching "\\<function\\>" "\\<end\\>" "\\<end\\>" -1 t)
              (gap-match-group))
    (setq arg (1- arg)))
  (beginning-of-line)
  ;; Signal that we found one
  t)

(defun gap-end-of-defun ()
  "Function to use for `end-of-defun-function'."
  (interactive)
  ;; Skip past the function statement, so that the searching will find
  ;; the end of this function definition
  (gap-search-forward-end-stmt nil 1 'end)
  (if (gap-find-matching "\\<function\\>" "\\<end\\>" nil t t)
      (gap-search-forward-end-stmt nil 1 'end)
    (goto-char (point-max)))
  (when (looking-at "\\s *$")
    (forward-line 1)))

;; TODO: Need to make this scroll and keep the input
;; TODO: may need to add a final return
(defun gap-eval-region (begin end)
  "Send region to GAP interpreter.
If GAP is not running it will signal an error or start it
depending on the value of `gap-auto-start-gap'."
  (interactive "r")
  (ensure-gap-running nil)
  (let ((process (get-buffer-process gap-process-buffer)))
    (setq gap-send-state 'echo)
    ;; (setq gap-send-state 'normal)
    (set-process-filter process 'gap-output-filter)
    (process-send-string process
                         (buffer-substring-no-properties begin end))))

(defun gap-eval-buffer ()
  "Send entire buffer to GAP interpreter.
See `gap-eval-region'."
  (interactive)
  (gap-eval-region (point-min) (point-max)))

(defun gap-eval-defun ()
  "Send current function definition to GAP interpreter.
See `gap-eval-region'."
  (interactive)
  (save-excursion
    (let* ((beg (progn (gap-beginning-of-defun 1) (point)))
           (end (progn (gap-end-of-defun)         (point))))
      (gap-eval-region beg end))))

(defun gap-eval-last-statement ()
  "Send previous statement to the GAP interpreter.
See `gap-eval-region'."
  (interactive)
  (save-excursion
    (let* ((beg (progn (gap-search-back-end-stmt nil 1 'beg)
                       (gap-search-back-end-stmt nil 1 'end)
                       (gap-skip-forward-to-token nil 1)
                       (point)))
           (end (progn (gap-search-forward-end-stmt nil 1 'end)
                       (point))))
      (gap-eval-region beg end))))

;;}}}
;;{{{ indentaton functions and variables

(defvar gap-end-of-statement
  ;; in the function section we use [^(] instead of . so that it will
  ;; match across lines.  I simply used ( and ) so that there would be
  ;; some semblance of matching and these are characters which
  ;; shouldn't be there.
  (concat "\\(;\\|\\<then\\>\\|\\<else\\>\\|\\<do\\>\\|"
          "\\<repeat\\>\\|\\<function\\>[^(]*([^)]*)\\)")
  "Regular expression matching the end of a GAP statement.")

(defvar gap-increment-indentation-regexp
  (concat "^[ \t]*\\("
          "if\\>"
          "\\|else\\>"
          "\\|elif\\>"
          "\\|for\\>"
          "\\|while\\>"
          "\\|repeat\\>"
          "\\|.*\\<function\\>"
          "\\)")
  "Regular expression matching a GAP statement which should increase indentation.")

(defvar gap-decrement-indentation-regexp
  (concat "^[ \t]*\\("
          "fi\\>"
          "\\|od\\>"
          "\\|else\\>"
          "\\|elif\\>"
          "\\|until\\>"
          "\\|end\\>"
          "\\)")
  "Regular expression matching a GAP statement which should decrease indentation.")

(defvar gap-continued-special-list
  (list
   ;; '( REGEXP  N  OFFSET  TERMINATE)
   '("#!#" nil 0 t)
   '("\\<local\\>[ \t\n]*\\([^ \t\n]\\)" 1 0 nil)
   '("\\<return\\>[ \t\n]*\\([^ \t\n]\\)" 1 0 t)
   ;;'(":=[ \t\n]*function[ \t\n]*(.*)" nil 4 t)
   '(":=[ \t\n]*\\([^ \t\n]\\)" 1 0 nil)
   '("\\<if\\>[ \t\n]*\\([^ \t\n]\\)" 1 0 nil)
   '("\\<until[ \t\n]*\\([^ \t\n]\\)" 1 0 nil))
  "
Determines special continued lines and indentation for them.
For each element of this list: search forward (from start of line initially
and from last match otherwise) for REGEXP entry. If second entry is nil, jump
back to the indentation, otherwise if a number N jump to the beginning of
the Nth group of the regexp. Take current indentation and add the third
OFFSET entry).  Take the maximum of values so obtained for each element.
If TERMINATE is t, then don't check any later ones if matched.")

(defun gap-ident-around-point ()
  "Return the identifier around the point as a string."
  (save-excursion
    (let (beg)
      (if (not (looking-at "\\(\\>\\|\\w\\)"))
          ""
        (and (< (point) (point-max))
             (forward-char 1))
        (re-search-backward "\\<" nil t)
        (setq beg (point))
        (re-search-forward "\\>" nil t)
        (buffer-substring beg (point))))))

(defun gap-point-in-comment-string ()
  "Returns non-nil if point is inside a comment or string."
  (save-excursion
    (let* ((p (point))
           (line (buffer-substring (beg-of-line-from-point) p)))
      (string-match "\\([^\\\\]\"\\|#\\)"
                    (gap-strip-line-of-strings line)))))

(defun gap-point-in-comment ()
  "Returns non-nil if point is inside a comment."
  (save-excursion
    (let* ((p (point))
           (line (buffer-substring (beg-of-line-from-point) p)))
      (string-match "^[^\"]*#" (gap-strip-line-of-strings line)))))


(defun gap-strip-line-of-strings (line)
  "Removes GAP strings from LINE."
  (while (string-match "[^\\\\]\\(\"\"\\|\"[^\"]*[^\\\\]\"\\)" line)
    (setq line (concat (substring line 0 (match-beginning 1))
                       (substring line (match-end 1)))))
  line)

(defun gap-strip-line-of-brackets (line)
  "Removes set of brackets from LINE.
Currently not used."
  (while (or (string-match "([^()]*)" line)
             (string-match "\\[[^\\[\\]]*\\]" line)
             (string-match "{[^{}]*}" line))
    (setq line (concat (substring line 0 (match-beginning 0))
                       (substring line (match-end 0)))))
  line)

(defun gap-strip-line-of-comments (line)
  "Removes GAP comments from LINE."
  (while (string-match "#.*[\n\C-m]" line)
    (setq line (concat (substring line 0 (match-beginning 0))
                       (substring line (match-end 0)))))
  line)

(defun gap-strip-strings-comments (stmt)
  "Remove GAP strings and comments from the statement.
See `gap-strip-line-of-comments' and `gap-strip-line-of-strings'."
  (gap-strip-line-of-comments
   (gap-strip-line-of-strings stmt)))

(defun gap-skip-forward-to-token (limit ret)
  "Skip forward from point to first character that is not in a comment."
  (while (and (if (not (re-search-forward "[^ \t\n\C-m]" limit ret))
                  nil
                (goto-char (match-beginning 0))
                t)
              (if (looking-at "#")
                  (re-search-forward "[\n\C-m]" limit ret)
                nil))))

(defun end-of-line-from-point (&optional p)
  "Return point at end of current line."
  (save-excursion
    (if p (goto-char p))
    (gap-end-of-line)
    (end-of-line)
    (point)))

(defun beg-of-line-from-point (&optional p)
  "Return at beginning of current line."
  (save-excursion
    (if p (goto-char p))
    (gap-beginning-of-line)
    (point)))

(defun gap-beginning-of-line ()
  "Go to the beginning of the current line.
Accounts for selective display (^m)."
  (if (re-search-backward "[\n\C-m]" nil 1)
      (forward-char 1)))

(defun gap-end-of-line ()
  "Go to the end of the current line.
Accounts for selective display (^m)."
  (if (re-search-forward "[\n\C-m]" nil 1)
      (forward-char -1)))

(defun lines-indentation (&optional p)
  "Return number of characters of indentation on the current line.
With `prefix-arg', return information for the line as if point
were at that buffer position. "
  (save-excursion
    (if p (goto-char p))
    (+ (- (progn (gap-beginning-of-line) (point)))
       (progn (skip-chars-forward " \t") (point)))))

(defun gap-looking-at (s)
  "Like looking-at, but accounts for selective display (^m)."
  (save-excursion
    (if (eq (substring s 0 1) "^")
        (progn
          (setq s (concat "[\n\C-m]" (substring s 1)))
          (forward-char -1)))
    (looking-at s)))

(defun gap-back-to-indentation ()
  "Go to first before the non indentation character."
  (gap-beginning-of-line)
  (skip-chars-forward " \t"))

;;! Fix member function?!
(defun memberequal (x y)
  "Like memq, but uses `equal' for comparison.
This is a subr in Emacs 19."
  (while (and y (not (equal x (car y))))
    (setq y (cdr y)))
  y)

;; Note- for the purposes of indentation calculations, the following
;; statement segments are considered to be fully contained statements:
;;    ... function (...)
;;    for ... do
;;    while ... do
;;    od;
;;    repeat
;;    if ... then
;;    else
;;    elif .. then
;;    fi;


(defun gap-debug-inform (base ind prev this &optional note)
  "Print statement detailing why the current line is indented as it is."
  (message
   (concat (if base (format "Base:%d  " base))
           (if ind (format "Ind:%d  " ind))
           (if prev (format "Prev:|%s|  "
                            (if (< (length prev) 20)
                                prev
                              (concat (substring prev 0 9) "..."
                                      (substring prev -9)))))
           (if this (format "This:|%s|"
                            (if (< (length this) 20)
                                this
                              (concat (substring this 0 9) "..."
                                      (substring this -9)))))
           (if note (format "  (%s)" note))
           )))


(defun gap-calculate-indent ()
  "Calculate indentation of current line."
  (save-excursion
    (gap-beginning-of-line)
    (let ((pos (point))
          this-stmt this-beg this-end
          last-stmt last-beg last-end
          ind)

      ;; extract this statement
      (gap-search-back-end-stmt nil 1 'end)
      (setq last-end (point))

      (gap-skip-forward-to-token pos 1)
      (setq this-beg (point))
      (gap-search-forward-end-stmt (end-of-line-from-point pos) 1 'end)
      (setq this-end (point))
      (setq this-stmt (gap-strip-strings-comments
                       (buffer-substring this-beg this-end)))

      ;; First check if this is a continued line and handle that.
      (if (setq ind (gap-calc-continued-stmt
                     this-stmt this-beg this-end pos))
          ind

        ;; Not a continued line. Find the previous statement.
        (goto-char last-end)
        (gap-search-back-end-stmt nil 1 'beg) ; jump to beginning of
                                        ; the end of last stmt
        (gap-search-back-end-stmt nil 1 'end) ; jump to end of the end of the
                                        ; stmt before the last stmt
        (gap-skip-forward-to-token nil t)     ; skip forward to start of last
        (setq last-beg (point))
        (setq last-stmt (gap-strip-strings-comments
                         (buffer-substring last-beg last-end)))

        ;; Now find the indentation
        (setq ind (gap-calc-new-stmt
                   this-stmt this-beg this-end
                   last-stmt last-beg last-end)))

      ;; return the indentation
      ind)))

(defun gap-calc-new-stmt (this-stmt this-beg this-end
                          last-stmt last-beg last-end)
  "Find indentation for a new statement in GAP."
  (let ((ind 0)
        base)
    (goto-char last-beg)
    (gap-back-to-indentation)

    ;; Indent based on current and previous statements
    (if (string-match gap-increment-indentation-regexp last-stmt)
        (setq ind (+ ind gap-indent-step)))
    (if (string-match gap-decrement-indentation-regexp this-stmt)
        (setq ind (- ind gap-indent-step)))

    ;; We are at the beginning of the previous line
    (let ((p (point))
          (last-was-decr nil)
          (more-than-one-command-on-last-line nil))
      (goto-char last-beg)
      ;; Increment/decrement for all statements on the previous line
      (while (and (not (eq (point) (save-excursion (gap-back-to-indentation) (point))))
                  (gap-search-back-end-stmt nil 1 'beg)
                  (gap-search-back-end-stmt nil 1 'end))
        (gap-skip-forward-to-token nil t)
        (let ((str (buffer-substring (point) last-beg)))
          (if (string-match gap-increment-indentation-regexp str)
              (setq ind (+ ind gap-indent-step)))
          (if (string-match gap-decrement-indentation-regexp str)
              (setq ind (- ind gap-indent-step)
                    last-was-decr t)
            (setq last-was-decr nil)))
        (setq last-beg (point)
              more-than-one-command-on-last-line t)) ; end while

      ;; If the last statement that we saw (working backwards) was a
      ;; decrement, then we can ignore it since it was already taken
      ;; into account:
      ;; if 4>3 then x:= 3;
      ;; fi; if 4>3 then
      ;;     x:= 3;
      ;; fi;
      (if last-was-decr
          (setq ind (+ ind gap-indent-step)))
      ;; Handle cases like
      ;; if 4>3 then return([]); fi;
      (if (and more-than-one-command-on-last-line
               (string-match gap-decrement-indentation-regexp last-stmt))
          (setq ind (- ind gap-indent-step))))

    ;; Determine the base from the most recent statement which is the
    ;; first on the line (this is where we ended from the while loop)
    (setq base (progn (gap-back-to-indentation) (current-column))
          ind (+ base ind))
    (if gap-debug-indent
        (gap-debug-inform base ind last-stmt this-stmt))
    ind))


(defun gap-calc-continued-stmt (this-stmt this-beg this-end pos)
  "Calculate indentation for a statement which has been continued from the previous line."
  ;; now check to see if we have a continued line or not
  (save-excursion
    (goto-char this-beg)
    (if (not (save-excursion (re-search-forward "[\n\C-m]" pos t)))
        nil
      ;; we are on a continued line. Handle it and return indentation.
      (let ((bracks (if gap-indent-brackets
                        (gap-calc-brackets this-beg pos)
                      nil))
            ind-special
            ind)

        ;; Right.  Now check to see if our special
        ;; continued line reg-exp matches this statment
        (goto-char this-beg)

        ;; If it is not a special continued line, then the indentation
        ;; will be...
        (setq ind (+ (lines-indentation this-beg)
                     gap-indent-step-continued))

        ;; Now must check whether statement matches special indentation
        ;; regular expression.

        (setq ind-special nil)
        (let ((special-list gap-continued-special-list))
          (while special-list
            (let ((regexp (nth 0 (car special-list)))
                  (match (nth 1 (car special-list)))
                  (offset (nth 2 (car special-list)))
                  (term (nth 3 (car special-list))))
              (if (not (gap-searcher 're-search-forward
                                     regexp
                                     pos t
                                     (if (numberp match)
                                         '(match-beginning match))))
                  ;; No match, try next one.
                  (setq special-list (cdr special-list))
                ;; Found a match! Great
                (if term
                    (setq special-list nil)
                  (setq special-list (cdr special-list)))
                (if (null match)
                    (gap-back-to-indentation))
                (setq ind-special (max (if (null ind-special) 0 ind-special)
                                       (+ (current-column) offset)))))))

        ;; Now decide on the actual indentation.
        (cond ( (and bracks ind-special)
                ;; both special stmt and within brackets.
                (setq ind
                      (max ind-special
                           (if gap-bracket-threshold
                               (min (car bracks)
                                    (+ (max ind-special (cdr bracks))
                                       gap-bracket-threshold))
                             (car bracks))))
                (if gap-debug-indent
                    (gap-debug-inform ind-special ind nil this-stmt
                                      "Special & Brackets")))
              ( bracks
                ;; within brackets.
                (setq ind
                      (if gap-bracket-threshold
                          (min (car bracks)
                               (+ (cdr bracks) gap-bracket-threshold))
                        (car bracks)))
                (if gap-debug-indent
                    (gap-debug-inform (cdr bracks) ind nil this-stmt
                                      "Brackets")))
              ( ind-special
                ;; just on special indentation line (no bracketing)
                (setq ind ind-special)
                (if gap-debug-indent
                    (gap-debug-inform nil ind nil this-stmt
                                      "Special")))
              ( t
                ;; otherwise, don't adjust standard indentation
                (if gap-debug-indent
                    (gap-debug-inform nil ind this-stmt
                                      "Continued"))))
        ind))))


(defun gap-calc-brackets (this-beg pos)
  "Check to see if there is unfinished bracket list and if there is,
return a pair (ind . base) for indentation due to bracketing, and the
base indentation of the line starting the bracket grouping"
  (goto-char pos)
  (let ((brack-level -1) ind-brack base-brack)
    (while (and (< brack-level 0)
                (gap-searcher 're-search-backward
                              "\\(\\s(\\|\\s)\\)" this-beg t))
      (cond ((looking-at "\\s(")
             (setq brack-level (1+ brack-level)))
            ((looking-at "\\s)")
             (setq brack-level (1- brack-level)))))
    (if (not (= brack-level 0))
        ;; Not within unclosed brackets.
        nil
      ;; Yes we are within unclosed brackets.
      (setq base-brack (current-indentation))
      (forward-char 1)
      (skip-chars-forward " \t")
      (setq ind-brack (current-column))
      ;; return cons of indentation level due to bracks, and the base
      (cons ind-brack base-brack))))

(defun gap-searcher (search-func object &optional bound silent move)
  "Use function SEARCH-FUNC to search for OBJECT.  Also passes BOUND for
specifying the character position bounding the search, SILENT to tell
search routines that they should not signal errors.
  The result is a search that skips matches that occur in comments or
strings in the GAP code.
  If MOVE is non-nil the move to the buffer position returned by evaling
MOVE after each search. This is for moving to the beginning or end of
groups in the regexp. eg use '(match-beginning 0)."
  (let ((done nil)
        return pos)
    (while (not done)
      (if (not (apply search-func object bound silent nil))
          (setq done t
                return nil)
        ;; move to position asked
        (setq pos (if move
                      (eval move)
                    (point)))
        ;; Make sure that we haven't hit a string/comment!
        (if (gap-point-in-comment-string)
            ;; in comment/string! Not finished yet. Try again.
            nil
          ;; Found the position.
          (goto-char pos)
          (setq done t
                return t))))
    return))

(defun gap-find-matching (breg ereg &optional also forw noerr)
  "Search forward to find EREG or backward to find BREG.

Skips over BREG/EREG pairs.  For example, this allows searching
for the end of the current function definition while ignoring any
functions defined locally.  If regexp ALSO is defined, then also
stop on it if found.

If FORW is nil then it will attempt to determine which direction
to search.  If FORW it t, then match forward instead of trying to
figure it out, and if FORW is -1, then it will match backward.

If NOERR is non-nil then do not produce an error if nothing is
found, simply return nil."
  (let ((p (point))
        (searcher 're-search-forward)
        (inc breg)  ;; Everytime we see this, increment counter
        (dec ereg)  ;; Everytime we see this, decrement counter
        (c 1)
        (d t) ;; d=t => direction forward
        (p1 (point)))
    (cond ((eq forw nil)
           (cond ((or (looking-at breg) (and also (looking-at also)))
                  (setq p1 (match-end 0)))
                 ((looking-at ereg)
                  (setq p1 (match-beginning 0))
                  (setq searcher 're-search-backward
                        inc ereg
                        dec breg
                        d nil))))
          ((eq forw -1)
           (setq p1 (point))
           (setq searcher 're-search-backward
                 inc ereg
                 dec breg
                 d nil)))
    (goto-char p1)
    (while (and (> c 0) (apply searcher (concat "\\(" breg "\\|" ereg
                                                (if also "\\|") also "\\)")
                               nil t nil))
      (setq p1 (match-beginning 0))
      (if (not (gap-point-in-comment-string))
          (save-excursion
            (goto-char p1)
            (if (and (= c 1) also (looking-at also))
                (setq c 0)
              (setq c (+ c (cond ((looking-at inc) 1)
                                 ((looking-at dec) -1)
                                 (t 0)))))
            (if (= c 0) (setq p (point))))))
    (if (not (= c 0))
        (if noerr
            (setq p nil)
          (error "No match!"))
      (goto-char p))
    p))

(defun gap-search-back-end-stmt (limit ret goto)
  "Search backward from point for the end of a GAP statement.
If GOTO is the symbol end, then goto the end of the statement,
else to the beginning of the end."
  (if (not (gap-searcher 're-search-backward ; searcher to use.
                         gap-end-of-statement ; regular expression.
                         limit      ; bound for search.
                         (if ret 1 t)   ; return nil if no match
                                        ; and goto bound if RET.
                         (if (eq goto 'end)
                             '(match-end 0)
                           '(match-beginning 0))))
      ;; not found. Move to limit if so asked
      nil
    ;; now make sure we skip over multiple semi-colons
    (while (and (not (eq goto 'end))
                (looking-at ";")
                (> (point) (point-min)))
      (forward-char -1))
    t))

(defun gap-search-forward-end-stmt (limit ret goto)
  "This function searches forward from point for the end of a GAP
statement, making sure to skip over comments and strings."
  (if (not (gap-searcher 're-search-forward   ; searcher to use.
                         gap-end-of-statement ; regular expression.
                         limit      ; bound for search.
                         (if ret 1 t)   ; return nil if no match
                                        ; and goto bound if RET.
                         (if (eq goto 'end)
                             '(match-end 0)
                           '(match-beginning 0))))
      ;; not found. Move to limit if so asked
      nil
    ;; now make sure we skip over multiple semi-colons
    (while (and (not (eq goto 'end))
                (looking-at ";")
                (> (point) (point-min)))
      (forward-char -1))
    t))

;;}}}

(provide 'gap-mode)
