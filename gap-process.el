;;; gap-process.el --- Run a GAP session in Emacs
;;
;; Author: Michael Smith <smith@pell.anu.edu.au>
;;	Gary Zablackis
;;	Goetz Pfeiffer
;;	Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Version: 2.0.1
;; Keywords: gap, comint
;; URL: https://gitlab.com/gvol/gap-mode

;; This file is part NOT of GNU Emacs.

;;; Commentary:

;; Runs a GAP session in an Emacs buffer and is based on comint-mode.
;; To run GAP, just type "M-x gap".
;;
;; Command completion is available by pressing "TAB".  Completions are
;; sent to a separate *Completions*.
;;
;; Help is available at any time (that GAP is not busy) by pressing
;; "?".  Output is to *GAP Help* buffer.
;;
;; To install, put this file somewhere in your load path, and add the
;; following line to your .emacs:
;;
;;    (autoload 'gap "gap-process" "*Run GAP" t)
;;
;; You will probably have to set the path to your gap executable and
;; command line options, either via (customize-group 'gap) or by
;; adding (a properly modified version of)
;;
;;    (setq gap-executable "/path/to/gap"
;;          gap-start-options '("-f" "-b" "-m" "2g"))
;;
;; to your .emacs file.
;;
;; Note that the commands `gap-complete' and `gap-help' can be bound
;; to keys in other buffers to issue commands to the running GAP
;; process.  If no process is running, they will either cause an error
;; or start a GAP process depending on the value of
;; `gap-auto-start-gap'.

;;; History:
;;
;; Michael Smith                        smith@pell.anu.edu.au
;; Australian National University
;; February 1993
;;
;; Changed from version 1.50,  15:44 Thu 22 Apr 1993
;; by Gary Zablackis (4/2005)
;;
;; A modification and extension of the GAP mode of Goetz Pfeiffer.
;;
;; Uses comint-mode instead of shell-mode (which means you must have the
;; comint package installed).
;; v1.50 -
;; * Fixed problem of disappearing output when point not in *gap* buffer.
;;   Also ensure that output scrolls if visible in window.
;; v1.20 -
;; * Allow switching to existing process buffer if it exists, also piping
;;   current buffer into it if prefix arg.
;; v1.12 -
;; * Included "brk>" in prompt-regexp. How could I forget this
;; v1.10 -
;; * Now gap-complete-double-cols controls formatting of completions buff.
;; v1.08 -
;; * Fixed gap-prompt-regexp so that a > in line would not be confused.
;; v1.07 -
;; * Fixed cosmetic problem with *help* output. Now instead of stripping
;;   all the ^H and ^M from the output, leave it unchanged and tidy up
;;   buffer after help output has finished.
;; v1.06 -
;; * Fixed bug in gap-ident-around-point that caused non-word characters
;;   to end up in the extracted identifier.
;; v1.05 -
;; * Added C-l to call comint-previous-similar-input, which is almost
;;   the same as the GAP C-l previous input command. Moved recenter
;;   to C-c C-l to make room.

;;; Code:

(require 'comint)
(require 'ansi-color) ;; We need ansi-color-filter-apply
;; TODO: problem with interrupting while printing output.
;;{{{ defcustoms

(defcustom gap-executable "/usr/local/algebra/bin/gap"
  "Path to the GAP executable."
  :group 'gap
  :type 'file)

(defcustom gap-start-options (list "-f" "-b" "-m" "2m")
  "The list of initial GAP options.
You may need to specify -f to force line editing."
  :group 'gap
  :type '(repeat string))

(defcustom gap-prompt-regexp "\\(\\(gap\\|brk[_0-9]*\\)>\\|^>\\) *"
  "Regexp used by Newline command in GAP mode to match prompt."
  :group 'gap
  :type 'regexp)

(defcustom gap-directory nil
  "If non-nil, change to this directory before running GAP.
Otherwise will use the default directory of the new *GAP* buffer."
  :group 'gap
  :type '(choice (const nil)
                 file))

(defcustom gap-process-beep nil
  "If non-nil beep when GAP asks."
  :group 'gap
  :type 'boolean)

(defcustom gap-complete-double-cols t
  "Controls final formatting of the GAP completions buffer.
If t and buffer is currently shown with more than 80 columns and
not enough lines, then make the list double columned.  If not nil
or t, then always make the completions list double columnes."
  :group 'gap
  :type '(choice (const :tag "Never use double columns" nil)
                 (const :tag "Sometimes use double columns" t)
                 (other :tag "Always use double columns" 'always)))

(defcustom gap-auto-start-gap nil
  "If non-nil, automatially start an interpreter when required.
Otherwise signals an error."
  :group 'gap
  :type 'boolean)

(defcustom gap-help-max-continues 50
  "The maximum number of times to continue getting help.
Recent GAP is willing to print out the "
  :group 'gap
  :type 'integer)

;;}}}

(defvar gap-help-last-output-begin nil
  "Together with `gap-help-last-output-end', this helps
determine if the output from help has stabilized.")
(defvar gap-help-last-output-end nil
  "Together with `gap-help-last-output-begin', this helps
determine if the output from help has stabilized.")
(defvar gap-help-continues 0
  "How many times we have continued this help session.")

(defvar gap-process-map nil
  "Keymap for use in the inferior GAP buffer.")
(if gap-process-map nil
  (setq gap-process-map (copy-keymap comint-mode-map))
  (define-key gap-process-map "\C-m" 'gap-send)
  (define-key gap-process-map "\t" 'gap-complete)
  (define-key gap-process-map "?" 'gap-help)
  (define-key gap-process-map "=" 'gap-electric-equals)
  (define-key gap-process-map "%" 'gap-electric-percent)
  (define-key gap-process-map "\C-l" 'comint-previous-matching-input-from-input)
  (define-key gap-process-map "\C-c\C-l" 'recenter)
  (define-key gap-process-map "\C-c\C-c" 'comint-interrupt-subjob)
  (define-key gap-process-map "\C-c\C-z" 'comint-stop-subjob)
  )

(defvar gap-send-state nil
  "Variable used by filter to trap echos and completion in GAP output.")

(defvar gap-completion-ident nil
  "Stores identifier that GAP is completing.")

(defvar gap-process-buffer nil
  "Points to a running gap session.")

(defvar gap-pending-input nil
  "Holds input to feed slowly to GAP when starting with buffer as input.")

(defvar gap-mode-gaprc
  (let ((gaprc (concat (file-name-directory
                        (or load-file-name
                            (buffer-file-name)))
                       "emacs.gaprc")))
    (and (file-exists-p gaprc)
         gaprc))
  "The path to the emacs-specific gaprc file.
This file will be loaded after GAP has successfully started up.")

(defvar gap-pending-pointer nil)

(defvar gap-completing-buffer nil
  "The buffer which is currently completing.")

(defun gap-running-p ()
  "Return non-nil if GAP interpreter is running."
  (and gap-process-buffer
       (get-buffer-process gap-process-buffer)
       (eq (process-status (get-buffer-process gap-process-buffer))
           'run)))

(defun gap-okay-to-run ()
  "Return non-nil if it's okay to interact with a GAP process.
See `gap-auto-start-gap'."
  (or gap-auto-start-gap
      (gap-running-p)))

;;;###autoload
(defun gap (&optional send-buffer)
  "Start or switch to a GAP session.
If SEND-BUFFER is non-nil, send the contents of the current
buffer to the GAP session as initial standard input."
  (interactive "P")
  (if (not (gap-running-p))
      (let ((have-input (or gap-mode-gaprc send-buffer))
            proc)
        (setq gap-pending-input
              (if have-input
                  (concat (if gap-mode-gaprc
                              (format "Read(\"%s\");\n"
                                      gap-mode-gaprc)
                            nil)
                          (if send-buffer (buffer-string) nil))))
        (setq gap-pending-pointer 0)
        (setq gap-process-buffer
              (get-start-process gap-executable "GAP"
                                 (if gap-directory
                                     (expand-file-name gap-directory)
                                   nil)
                                 gap-start-options))
        (setq proc (get-buffer-process gap-process-buffer))
        (gap-process-mode)
        (if have-input
            (set-process-filter proc 'gap-startfile-filter)
          (set-process-filter proc 'gap-output-filter)))
    (if send-buffer
        (let (proc)
          (setq gap-pending-input (buffer-string))
          (setq gap-pending-pointer 0)
          (setq proc (get-buffer-process gap-process-buffer))
          (set-process-filter proc 'gap-startfile-filter)
          (comint-send-string proc "\n")))
    (pop-to-buffer gap-process-buffer)))

(define-derived-mode gap-process-mode comint-mode "Inferior Gap"
  "Major mode for interacting with GAP.
\\<gap-process-map>Provides support for completion (via \\[gap-complete]) and GAP's help
system.  Invoking \\[gap-help] will provide help on the current
GAP identifier.

Since `gap-process-mode' inherits from `comint-mode' it's
features are also relevant.  As a convenience for GAP users
\\[comint-previous-matching-input-from-input] has been bound to `comint-previous-matching-input-from-input'
which is much like GAP's C-l (\\[recenter] can be used to recenter)."
  (set (make-local-variable 'comint-prompt-regexp) gap-prompt-regexp)
  (set (make-local-variable 'comint-use-prompt-regexp) t)
  (set (make-local-variable 'comint-eol-on-send) t)

  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (require 'gap-mode) ;; for gap-syntax-table and gap-font-lock-keywords
  (set-syntax-table gap-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(gap-font-lock-keywords))
  (use-local-map gap-process-map)
  (setq gap-send-state 'normal))

(defun gap-send ()
  "Send input to GAP."
  (interactive "*")
  (setq gap-send-state 'echo)
  (comint-send-input))

(defun gap-startfile-filter (proc string)
  "Filter the GAP process while standard input remains to be sent.
To avoid problems with overflowing input buffers, this function
sends the next line of input when it thinks GAP is waiting for
it (using `gap-prompt-regexp')."
  (let ((cbuf (current-buffer)))
    (set-buffer (process-buffer proc))
    (goto-char (point-max))
    (insert (string-strip-chars string "\C-h\C-g\C-m"))
    (set-marker comint-last-output-start (point))
    (set-marker (process-mark proc) (point))
    (if (and gap-process-beep (string-match "\C-g" string))
        (beep))
    (save-excursion
      (beginning-of-line)
      (if (looking-at (concat ".*" gap-prompt-regexp "$"))
          (let ((x (string-match "\n" (substring gap-pending-input
                                                 gap-pending-pointer))))
            (if x
                (progn
                  (comint-send-string proc
                                      (substring gap-pending-input
                                                 gap-pending-pointer
                                                 (+ 1 x gap-pending-pointer)))
                  (setq gap-pending-pointer (+ 1 x gap-pending-pointer)))
              (set-process-filter proc 'gap-output-filter)
              (gap-process-mode)
              (comint-send-string proc (substring gap-pending-input
                                                  gap-pending-pointer))
              (setq gap-pending-input nil)))))
    (set-buffer cbuf)))

(defun gap-output-filter (proc string)
  "Filter the output from a GAP process most of the time.
It depends on the variable `gap-send-state' to determine which of three
possible output states GAP is in:
    'normal for output that should be shown;
    'echo for the GAP echoing of the last command (suppressed);
    'completing when GAP will be trying to complete a symbol before point."
  (let ((cbuf (current-buffer)))
    (cond
     ((eq gap-send-state 'normal)
      (set-buffer (process-buffer proc))
      (set-marker comint-last-output-start (point))
      (scrolling-process-filter
       proc (buttonize-syntax-error (string-strip-chars string "\C-g\C-h\C-m")))

      (set-marker (process-mark proc) (point)))
     ((eq gap-send-state 'echo)
      (set-buffer (process-buffer proc))
      (let ((x (string-match "\n" string)))
        (if x
            (progn
              (setq gap-send-state 'normal)
              (set-marker comint-last-output-start (point))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: this section is only needed for NTEmacs
;;;       (insert (string-strip-chars string                        ;;GEZ: NTEmacs: get back 1st line of output
;;;                                          "\C-g\C-h\C-m"))             ;;GEZ: NTEmacs: get back 1st line of output
;;; NOTE: end NTEmacs specific code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (insert (buttonize-syntax-error
                       (string-strip-chars (substring string (+ x 1))    ;;GEZ: original
                                           "\C-g\C-h\C-m")))              ;;GEZ: original
              (set-marker (process-mark proc) (point))))))
     ((eq gap-send-state 'completing)
      (let ((x (string-match "\C-g" string)))
        (if x  ;; GAP beeped on completing: now ask for all completions
            (progn
              (if gap-process-beep (beep))
              (gap-complete t)
              (setq gap-send-state 'normal))
          ;; We seem to at least get the identifier back all at once,
          ;; sometimes more.
          (when (not (looking-back gap-completion-ident nil))
            (error "Got confused during completion"))
          (when (string-match gap-completion-ident string)
            (delete-char (- (length gap-completion-ident))))
          ;; Insert the completed symbol
          (insert (string-strip-chars string " \C-h\C-g\C-m"))))))
    (set-buffer cbuf)))

(defun gap-help-filter (proc string)
  "Filter the output a help command into a *GAP Help* buffer.
It must handle the continuation prompts by stripping them and
sending spaces to continue the output until finished."
  (let ((cbuf (current-buffer))
        (finished nil))
    (set-buffer (get-buffer-create "*GAP Help*"))
    (setq buffer-read-only nil)                                     ;; GEZ: so we can put help info into the buffer
    (set (make-local-variable 'show-trailing-whitespace) nil)
    (goto-char (point-max))
    (insert string)
    (beginning-of-line)

    ;; Handle "menus"
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "WARNING: terminal is not fully functional"
             nil t)
        (delete-region (line-beginning-position) (line-end-position)))
      (when (re-search-forward "(press RETURN)?" nil t)
        (delete-region (line-beginning-position) (point))
        (comint-send-string proc "\n")))

    ;; Page through all the results
    (when (re-search-forward
           "  -- <space> page, <n> next line, <b> back, <p> back line, <q> quit --"
           nil t)                    ;;GEZ: Add to handle GAP 4.4.x output
      (delete-region (match-beginning 0) (point))
      (ansi-color-apply-on-region (point-min) (point-max))

      ;; Compare last and this output to see if it's the same and
      ;; hence we've reached the end of help.
      (when (and gap-help-last-output-begin
                 gap-help-last-output-end)
        (let ((last (buffer-substring-no-properties
                     gap-help-last-output-begin
                     gap-help-last-output-end))
              (this (buffer-substring-no-properties
                     gap-help-last-output-end
                     (point-max))))
          (when (string-equal last this)
            (setq finished t)
            (delete-region gap-help-last-output-end (point-max)))))
      (setq gap-help-last-output-begin gap-help-last-output-end)
      (setq gap-help-last-output-end (point-max))
      ;; Fall back in case the above fails for some reason (or if
      ;; people just don't like too much help)
      (setq gap-help-continues (1+ gap-help-continues))
      (when (and (not finished)
                 (> gap-help-continues gap-help-max-continues))
        (setq finished t)
        (delete-region (point) (point-max)))
      ;; tell GAP to continue with next page (or not)
      (comint-send-string proc (if finished "q" " ")))

    (when (looking-at (concat "^[ ]*" gap-prompt-regexp "$"))                ;;GEZ: make sure get the end of it all
      (delete-region (point) (point-max))
      (gap-cleanup-help-buffer)
      (goto-char (point-min))
      (set-process-filter proc 'gap-output-filter))
    ;; Handle long lines, broken up by GAP
    (while (re-search-forward "\\\\$" nil t)
      (delete-region (1- (point)) (1+ (point)))
      (goto-char (point-min)))
    (when (re-search-forward (concat "^GAP Help in \\(.*\\) with offset \\([0-9]+\\)$") nil t)                ;;GEZ: make sure get the end of it all
      (let ((help-file (match-string 1))
            (offset (string-to-number (match-string 2))))
        (insert-file-contents help-file nil nil nil t)
        (gap-cleanup-help-buffer)
        (set-buffer-modified-p nil)

        ;; Fancy tricks to make changes to point "stick"
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (forward-line (1- offset))
        (recenter-top-bottom 2)
        (pop-to-buffer cbuf)

        (set-process-filter proc 'gap-output-filter)))
    (set-buffer cbuf)))

(defun gap-cleanup-help-buffer ()
  "Cleans up the *GAP Help* buffer from all the weird GAP output.
Also adds links."
  (ansi-color-apply-on-region (point-min) (point-max))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: this section is only needed for NTEmacs
  (goto-char (point-min))
  (while (re-search-forward
          "gap: 'ioctl' could not turn off raw mode!\n" nil t)
    (replace-match ""))
;;; NOTE: end NTEmacs specific code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (goto-char (point-min))
  (while (re-search-forward
          "^\\( *\^H\\)\\|\\(\C-m\\)\\|\\(\C-h\\)" nil t)              ;;GEZ: get rid of ^H ^H and ^M
    (replace-match ""))

  ;; Turn choice numbers into buttons to click on them.
  ;; Make sure they are obviously clickable.
  (goto-char (point-min))
  (while (re-search-forward
          "^\\[\\([0-9]+\\)\\].*" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         `(button t
                                  category default-button
                                  action (lambda (button)
                                           (gap-help ,(match-string-no-properties 1) nil))
                                  mouse-action (lambda (button)
                                                 (gap-help ,(match-string-no-properties 1) nil)))))

  ;; Cross references -- not obviously clickable so they don't distract
  (goto-char (point-min))
  (while (re-search-forward
          "\\(?:\\([A-Z]\\w+\\)\\s +\\)?(\\(?:see\\s +\\)?\\([0-9.-]+\\))" nil t)
    (let ((help-section (if (match-beginning 1)
                            (match-string-no-properties 1)
                          (match-string-no-properties 2))))
      (add-text-properties (match-beginning 0) (match-end 0)
                           `(button t
                                    category default-button
                                    face default
                                    mouse-face highlight
                                    action (lambda (button)
                                             (gap-help ,help-section nil))
                                    mouse-action (lambda (button)
                                                   (gap-help ,help-section nil)))))))

(defun gap-completions-filter (proc string)
  "Filter all completions of a symbol into a *Completions* buffer.
Fontify completions so that they can be clicked to complete in
the process buffer.  Depending on the value of
`gap-complete-double-cols' make two columns."
  (with-current-buffer (get-buffer-create "*Completions*")
    (setq buffer-read-only nil) ;; GEZ: so we can put completions into the buffer
    (goto-char (point-max))
    (insert (string-strip-chars string "\C-g\C-m\C-h"))
    (beginning-of-line)
    ;; We have all the completions since we see a prompt
    ;; (set-process-filter proc 'gap-output-filter)
    (when (looking-at (concat gap-prompt-regexp ".*"
                              gap-completion-ident
                              (make-string (length gap-completion-ident) ?\ )))
      ;; Delete the prompt
      (delete-region (point) (point-max))
      ;; Make it double columned
      (let ((win (get-buffer-window (current-buffer)))
            (lines (count-lines (point-min) (point)))
            p1 rect)
        (when (and gap-complete-double-cols
                   (or (and win
                            (> lines (window-height win))
                            (> (window-width win) 79))
                       (not (eq gap-complete-double-cols t))))
          (forward-line (- 1 (/ lines 2)))
          (beginning-of-line)
          (setq p1 (point))
          (goto-char (point-max))
          (insert (make-string (- 39 (move-to-column 39)) ? ))
          (setq rect (delete-extract-rectangle p1 (point)))
          (goto-char (point-min)) (forward-line 2)
          (insert (make-string (- 39 (move-to-column 39)) ? ))
          (insert-rectangle rect)))
      ;; Buttonize
      (goto-char (point-min))
      (while (re-search-forward (concat "\\_<" gap-completion-ident "\\(?:\\s_\\|\\sw\\)+") nil t)
        (let ((fun `(lambda (button)
                      (quit-window)
                      (set-buffer gap-completing-buffer)
                      (if (not (looking-back ,gap-completion-ident))
                          (error "Not completing")
                        (delete-backward-char ,(length gap-completion-ident))
                        (insert ,(match-string-no-properties 0))))))
          (add-text-properties (match-beginning 0) (match-end 0)
                               (list
                                'button t
                                'category 'default-button
                                'face 'default
                                'mouse-face 'highlight
                                'action fun
                                'mouse-action fun))))
      ;; Return to normal output
      (set-process-filter proc 'gap-output-filter))))

(defun gap-complete (&optional full)
  "Complete the partial identifier at point.
With FULL, send two TABs to GAP to get a full list of completions."
  (interactive "*")
  (ensure-gap-running)
  (let ((process (get-buffer-process gap-process-buffer)))
    (if (not (looking-at "\\_>"))
        (if (not (re-search-forward "\\_>" nil t))
            (error "Complete what?")))
    (setq gap-completion-ident (gap-ident-around-point))
    (if (not full)
        (progn
          ;;  ask for completion and clear input line
          (setq gap-send-state 'completing)
          (process-send-string process (concat gap-completion-ident
                                               "\t\C-x")))

      (setq gap-send-state 'normal)
      (unwind-protect
          (progn
            (with-output-to-temp-buffer "*Completions*"
              (let ((buffer-read-only nil))
                (help-print-return-message)))
            (setq gap-completing-buffer (current-buffer))
            (set-process-filter process 'gap-completions-filter)
            (process-send-string process (concat gap-completion-ident
                                                 "\t\t\C-x")))))))

(defun ensure-gap-running (&optional noerr)
  "Ensure that a GAP process is running, or throw an error.
If `gap-auto-start-gap' is non-nil then start a new process if
one is not running.  If NOERR is non-nil return nil instead of
throwing an error.

This is meant to be called in a function such as `gap-complete'
before doing something which requires the GAP interpreter to be
running."
  (or (gap-running-p)
      (when gap-auto-start-gap (gap))
      (unless noerr (error "GAP not running"))
      nil))

(defun gap-help (topic arg)
  "Display GAP help about TOPIC in the *GAP Help* buffer.
If ARG is non-nil start a GAP process regardless of value of
`gap-auto-start-gap'."
  (interactive
   (let ((enable-recursive-minibuffers t)
         (try-word (gap-ident-around-point))
         val)
     (if (string-equal try-word "gap>")
         (setq val (read-string "GAP topic: "))
       (setq val (read-string (format "GAP topic (default %s): "
                                      try-word)))
       (if (string-equal val "")
           (setq val try-word)))
     (list val current-prefix-arg)))
  (let ((gap-auto-start-gap (or arg gap-auto-start-gap)))
    (ensure-gap-running))
  (let ((process (get-buffer-process gap-process-buffer)))
    (unwind-protect
        (progn
          (with-output-to-temp-buffer "*GAP Help*"
            (help-print-return-message))
          (setq gap-help-last-output-begin nil
                gap-help-last-output-end nil
                gap-help-continues 0)
          (set-process-filter process 'gap-help-filter)
          (process-send-string process (concat "?" topic "\n"))))))

(defun get-start-process (progm &optional name dir args startfile)
  "Run program PROGM in buffer *NAME* (or if NAME is nil use *PROGM*).
Optionally sets the default directory to DIR.  If already running, just switch.
Has a optional list ARGS of command line arguments, and file STARTFILE
containing initial standard input to process."
  (interactive)
  (setq name (or name (file-name-nondirectory progm)))
  (let ((buffname (concat "*" name "*")))
    (cond ((not (comint-check-proc buffname))
           (let ((buff (get-buffer-create buffname)))
             (set-buffer buff)
             (display-buffer buff)
             (if dir (cd dir))
             (apply 'make-comint name progm startfile args)))
          (t
           (display-buffer buffname)
           (get-buffer buffname)))))

(defun string-strip-chars (string strip)
  "Take STRING and remove characters in STRIP.
Also strips ANSI escape sequences."
  (while (> (length strip) 0)
    (let ((pos 0))
      (setq pos (string-match (substring strip 0 1) string pos))
      (while (not (eq pos nil))
        (setq string (concat (substring string 0 pos)
                             (substring string (+ pos 1))))
        (setq pos (string-match (substring strip 0 1) string pos)))
      (setq strip (substring strip 1))))
  ;; We call ansi-color-filter-apply rather than adding it to
  ;; comint-preoutput-filter-functions since that doesn't get called
  ;; for the completions and help buffers.
  (ansi-color-filter-apply string))

;; Jim Thompson's scrolling output filter
(defun scrolling-process-filter (proc str)
  "Handle all output from the process PROC.
If the process buffer is visible, try to keep the end on screen."
  (let ((obuf (current-buffer)))
    (set-buffer (process-buffer proc)) ;this IS needed
    (save-excursion
      (goto-char (process-mark proc))
      (insert-before-markers str))
    (set-buffer obuf)))

(defface gap-clickable-syntax-error
  '((t (:underline t :inherit font-lock-warning-face)))
  "Face for clickable syntax error messages in *GAP* buffer."
  :group 'gap)

(define-button-type 'gap-syntax-error
  'face 'gap-clickable-syntax-error
  'help-echo "mouse-2, RET: visit this syntax error"
  )

;; TODO: fix the case when half the syntax error comes in at different times
(defun buttonize-syntax-error (string)
  "Propertize STRING so that syntax error messages are clickable."
  (let ((start 0))
    (while (string-match "^Syntax error: .* in \\(.*\\) line \\([0-9]+\\)$" string start)
      (let ((fun `(lambda (button)
                    (find-file-other-window ,(match-string-no-properties 1 string))
                    (save-restriction
                      (widen)
                      (goto-char (point-min))
                      (forward-line ,(string-to-number (match-string-no-properties 2 string)))))))
        (add-text-properties (match-beginning 0) (match-end 0)
                             (list
                              'button t
                              'category (button-category-symbol 'gap-syntax-error)
                              'action fun
                              'mouse-action fun)
                             string))
      (setq start (match-end 0)))
    string))

(provide 'gap-process)

;;; gap-process.el ends here
