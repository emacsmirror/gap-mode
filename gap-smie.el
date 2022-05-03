;;; gap-process.el --- Run a GAP session in Emacs
;;
;; Author: 	Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Version: 2.0.1
;; Keywords: gap, smie
;; URL: https://gitlab.com/gvol/gap-mode

;; This file is part NOT of GNU Emacs.

;;; Commentary:

;; This uses SMIE to improve the indentation of gap-mode.  Hopefully,
;; performance, maintainability, and features will be improved.

;;; History:

;;; Code:

(require 'smie)

(defconst gap-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst (exp) ;not technically, but probably makes sense
            ("function" insts "end")
            ("repeat" insts "until" exp)
            ("while" exp "do" insts "od")
            ("atomic" exp "do" insts "od")
            ("for" in-exp "do" insts "od")
            ("if" if-body "fi")
            ("return" exp)
            ("local" exps))
      (insts (insts ";" insts) (insts ";;" insts) (inst))
      (exp ("(" exps ")")
           ("[" exps "]")
           ("{" exps "}")
           ("not" exp)
           (in-exp)
           (exp ":=" exp)
           (exp ".." exp)
           (exp "and" exp)
           (exp "or" exp)
           (exp "<" exp)
           (exp "<=" exp)
           (exp "=" exp)
           (exp ">=" exp)
           (exp ">" exp)
           (exp "<>" exp)
           (exp "+" exp)
           (exp "-" exp)
           (exp "*" exp)
           (exp "/" exp)
           (exp "mod" exp)
           (exp "^" exp))
      (in-exp (exp "in" exp))
      (exps (exps "," exps) (exp))
      (itheni (insts) (exp "then" insts))
      (ielsei (itheni) (itheni "else" insts))
      (if-body (ielsei) (if-body "elif" if-body)))

    '((assoc ";" ";;"))
    '((assoc ","))
    '((assoc "elif"))
    '((assoc ":=")
      (assoc "not")
      (assoc "..")
      (assoc "and" "or")
      (assoc "<" "<=" "=" ">=" ">" "<>" "in")
      (assoc "+" "-")
      (assoc "*" "/" "mod")
      (assoc "^"))))
  "SMIE Grammar for the GAP language.")

(defun gap-smie-rules (kind token)
  "SMIE indentation rules for the GAP language.
See `smie-rules-function' for meaning of KIND and TOKEN."
  (when (and gap-debug-indent
             (not (use-region-p)))
    (message "%s %s" kind token))
  (pcase (cons kind token)

    ;; Lists etc. should indent differently
    (`(:after . ,(or `"[" `"(" `"{"))
     (when (save-excursion
             (forward-char 1)
             (or (eolp) (forward-comment 1)))
       (cons 'column (+ (current-column) gap-indent-list))))

    (`(:before . ",") (smie-rule-separator kind))

    ;; Handle indentation of XX := function(...) ... end
    (`(:before . "function")
     (when (save-excursion
             (forward-word 1)
             (forward-sexp 1)
             (smie-rule-hanging-p))
       (smie-rule-parent)))

    (`(:after . ")")
     (save-excursion
       (up-list -1)
       (when (equal "function" (car (smie-indent-backward-token)))
         `(column . ,(+ gap-indent-step (smie-indent-virtual))))))

    ;; It was aligning with the token following the if...
    (`(:before . ,(or `"then" `"elif" `"else"))
     0)

    (`(:after . ,(or `"if" `"then" `"elif" `"else"
                     `"do" `"repeat" `"while" `"atomic"))
     gap-indent-step)

    (`(:before . ,(or `";" `";;"))
     (cond
      ((smie-rule-parent-p "function" "repeat" "while" "atomic" "for"
                           "if" "then" "elif" "else" "when")
       (smie-rule-parent gap-indent-step))
      ))

    (`(:after . ,(or "=" ":=" "+" "-" "*" "/" "^"
                     ">" "<" ">=" "<=" "<>" "and" "or" "in"))
     (if (smie-rule-parent-p ";" ";;" nil) gap-indent-step))))

(provide 'gap-smie)

;;; gap-smie.el ends here
