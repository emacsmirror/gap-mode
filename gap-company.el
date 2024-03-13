;;; gap-company.el --- Use a GAP process as a company back end -*- lexical-binding: t -*-
;;
;; Author: 	Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Version: 2.2.0
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
     (gap-capf-generate-completions arg))))

(add-to-list 'company-backends 'company-gap-backend)

(provide 'gap-company)

;;; gap-company.el ends here
