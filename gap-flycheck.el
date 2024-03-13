;;; gap-flycheck.el --- Flycheck support for the GAP programming language.  -*- lexical-binding: t; -*-
;;
;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Version: 2.2.2
;; Keywords: gap flycheck
;; URL: https://gitlab.com/gvol/gap-mode

;; This file is part NOT of GNU Emacs.

;;; Commentary:

;; Provides flycheck linter for GAP using gaplint which is available
;; at https://github.com/james-d-mitchell/gaplint.

;;; Code:

(require 'flycheck)

(flycheck-define-checker gaplint
  "A GAP linter using gaplint."
  :command ("gaplint" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) " [" (one-or-more digit) "]" line-end))
  :modes gap-mode
  :predicate (lambda () (buffer-file-name))
  :enabled (lambda () (executable-find "gaplint")))

;;;###autoload
(defun gap-flycheck-setup ()
  "Set up the linter for GAP.

For information on how to configure gaplint see URL
`https://github.com/james-d-mitchell/gaplint'."
  (interactive)
  (add-to-list 'flycheck-checkers 'gaplint))

;;;###autoload
(eval-after-load 'flycheck #'gap-flycheck-setup)

(provide 'gap-flycheck)

;;; gap-flycheck.el ends here
