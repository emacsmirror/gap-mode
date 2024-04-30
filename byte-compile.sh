#!/usr/bin/env bash

rm *.elc
for f in *.el; do
    ${EMACS-emacs} \
        -q --batch \
        -L .\
        -L ~/.emacs.d/elpa/company-20* \
        -L ~/.emacs.d/elpa/flycheck-20* \
        --eval "(byte-compile-file \"$f\")"
done
