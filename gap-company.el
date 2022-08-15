;;; gap-company.el --- Use a GAP process as a company back end
;;
;; Author: 	Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Version: 2.1.0
;; Keywords: gap, company
;; URL: https://gitlab.com/gvol/gap-mode
;; Package-Requires: ((company))

;; This file is part NOT of GNU Emacs.

;;; Commentary:

;; This uses a GAP process to provide a company completion back end.

;;; History:

;;; Code:

(require 'company)
(require 'gap-process)

(defun company-gap-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-gap-backend))

    ;; Extract the item to complete
    (prefix (and (memq major-mode '(gap-mode gap-process-mode emacs-lisp-mode))
                 ;; TODO: This might have to be more complex,
                 ;; depending on how GAP handles arrays and such.
                 (company-grab-symbol)))

    (candidates
     ;; In particular, it fails with "HELP_VIEWER_INFO."
     (when (string-match "[a-zA-Z0-9_]$" arg)
       (ensure-gap-running)
       ;; TODO: I should check that it's at a prompt
       (setq gap-completion-ident arg)
       (setq gap-completing-buffer (current-buffer))
       (let ((process (get-buffer-process gap-process-buffer))
             (prefix (progn (string-match "^\\(.*?\\)\\([a-zA-Z0-9_]+\\)$" arg)
                            (match-string 1 arg)))
             (completions nil))
         (unwind-protect
             (with-current-buffer (get-buffer-create "*GAP Company Completions*")
               (erase-buffer)

               (setq gap-send-state 'company-completing)
               (set-process-filter process 'gap-company-completions-filter)
               ;; Send completion request and wait.
               ;; I suspect there's a much better way to do this.
               (process-send-string process (concat arg "\t\t\C-x"))
               (while (eq gap-send-state 'company-completing)
                 (accept-process-output process 0.001))
               ;; We finished completing, so extract all the completions.
               (goto-char (point-min))
               (while (re-search-forward "^ +\\(\\S +\\)$" nil t)
                 (setq completions (cons
                                    (concat prefix (match-string 1))
                                    completions)))
               completions)))))))

(defun gap-company-completions-filter (proc string)
  "Filter all completions of a symbol into a *Completions* buffer.
Fontify completions so that they can be clicked to complete in
the process buffer.  Depending on the value of
`gap-complete-double-cols' make two columns."
  ;; (message "string: %s" string)
  (with-current-buffer (get-buffer-create "*GAP Company Completions*")
    (goto-char (point-max))
    (insert (string-strip-chars string "\C-g\C-m\C-h"))
    (beginning-of-line)
    ;; We have all the completions since we see a prompt
    (when (looking-at (concat gap-prompt-regexp ".*"
                              gap-completion-ident ".*"
                              (make-string (length gap-completion-ident) ?\ )))
      ;; Delete the prompt
      (delete-region (point) (point-max))
      ;; Return to normal output
      (set-process-filter proc 'gap-output-filter)
      (setq gap-send-state 'normal))))

(add-to-list 'company-backends 'company-gap-backend)

(provide 'gap-company)

;;; gap-company.el ends here
