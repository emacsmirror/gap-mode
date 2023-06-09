ev;;; gap-flymake.el --- Flymake support for the GAP programming language.  -*- lexical-binding: t; -*-
;;
;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Version: 2.2.0
;; Keywords: gap flymake
;; URL: https://gitlab.com/gvol/gap-mode

;; This file is part NOT of GNU Emacs.

;;; Commentary:

;; Provides flymake backend for GAP using gaplint which is available
;; at https://github.com/james-d-mitchell/gaplint.

;;; Code:

(require 'flymake)
(require 'gap-mode)

(defvar-local gap--flymake-proc nil)

(defun gap-flymake (report-fn &rest _args)
  ;; Based on the example backend in the flymake manual
  "Flymake backend for GAP buffers using gaplint."
  (unless (executable-find "gaplint")
    (error "Cannot find the gaplint executable"))
  (unless (buffer-file-name)
    (error "Running gaplint requires buffer to be backed by a file"))

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `gap-flymake-proc' to a different value
  (when (process-live-p gap--flymake-proc)
    (kill-process gap--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  (let ((source (current-buffer)))
    (setq gap--flymake-proc
          (make-process
           :name "gaplint-flymake" :noquery t
           :buffer (generate-new-buffer " *gaplint-flymake*")
           :command `("gaplint" ,(buffer-file-name))
           :sentinel
           (lambda (proc _event)
             ;; Check that the process has indeed exited, as it might
             ;; be simply suspended.
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   ;; Check that `proc' matches `gap--flymake-proc',
                   ;; else it's an obsolete process.
                   (if (not (with-current-buffer source (eq proc gap--flymake-proc)))
                       (flymake-log :warning "Canceling obsolete check %s" proc)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       ;; Parse the output buffer for diagnostic's
                       ;; messages and locations, collect them in a
                       ;; list of objects, and call `report-fn'.
                       (cl-loop
                        while (search-forward-regexp
                               ;; We're assuming the filename must
                               ;; be the same as the buffer so we
                               ;; can ignore it...
                               "^\\(?:.*\\):\\([0-9]+\\): \\(.*\\) \\[\\([0-9]+\\)\\]$"
                               nil t)
                        for msg = (match-string 2)
                        for (beg . end) = (flymake-diag-region
                                           source
                                           (string-to-number (match-string 1)))
                        for threshold = (match-string 3)
                        for type = (if (string-match "^0$" threshold)
                                       :warning
                                     :error)
                        collect (flymake-make-diagnostic source
                                                         beg
                                                         end
                                                         type
                                                         msg)
                        into diags
                        finally (funcall report-fn diags))))
                 ;; Cleanup the temporary buffer
                 (kill-buffer (process-buffer proc)))))))))

;;;###autoload
(defun gap-setup-flymake-backend ()
  "Turn on the flymake backend for `gap-mode'.

For information on how to configure gaplint see URL
`https://github.com/james-d-mitchell/gaplint'."
  (add-hook 'flymake-diagnostic-functions #'gap-flymake nil t))

;;;###autoload
(eval-after-load 'flymake
  (lambda () (add-hook 'gap-mode-hook #'gap-setup-flymake-backend)))

(provide 'gap-flymake)

;;; gap-flymake.el ends here
