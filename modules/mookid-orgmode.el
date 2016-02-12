;;; mookid-orgmode --- Configuration for orgmode

;;; Commentary:

;;; Code:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ocaml . t)))

(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


(setq org-latex-create-formula-image-program 'dvipng)

(provide 'mookid-orgmode)
;;; mookid-orgmode.el ends here
