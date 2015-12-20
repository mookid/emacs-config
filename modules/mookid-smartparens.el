;;; mookid-smartparens --- Configuration for smartparens

;;; Commentary:

;;; Code:
(require 'smartparens-config)

(show-smartparens-global-mode nil)
(setq-default sp-autoskip-closing-pair 'always)
(setq-default sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(define-key evil-insert-state-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
(global-set-key (kbd "M-[") 'sp-backward-unwrap-sexp)
(require 'cl-lib)
(cl-macrolet
    ((customize (key val)
		(let ((symb (intern (format "wrap-with-%Ss" key)))
		      (kbinding (format "C-c %s" val)))
		  `(progn
		     (defun ,symb (&optional arg)
		       "Wrap the next form (or selection) using `sp-wrap-with-pair'."
		       (interactive "P")
		       (sp-wrap-with-pair ,val))
		     (global-set-key (kbd ,kbinding) ',symb)))))
  (customize paren   "(")
  (customize bracket "[")
  (customize brace   "{")
  (customize squote  "'")
  (customize dquote  "\""))

(provide 'mookid-smartparens)
;;; mookid-smartparens.el ends here
