;;; mookid-smartparens --- Configuration for smartparens

;;; Commentary:

;;; Code:
(require 'smartparens-config)

(show-smartparens-global-mode nil)
(setq-default sp-autoskip-closing-pair 'always)
(setq-default sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(global-set-key (kbd "M-[") 'sp-backward-unwrap-sexp)
(loop
 for (key . val) in '((paren   . "(")
                      (bracket . "[")
                      (brace   . "{")
                      (squote  . "'")
                      (dquote  . "\""))
 for symb = (intern (concat "wrap-with-" (prin1-to-string key) "s"))
 for kbinding = (concat "C-c " val)
 do (eval
     `(defun ,symb (&optional arg)
        "Wrap the next form (or the selection) using `sp-wrap-with-pair'."
        (interactive "P")
        (sp-wrap-with-pair ,val)))
 do (eval `(global-set-key (kbd ,kbinding) ',symb)))

(provide 'mookid-smartparens)
;;; mookid-smartparens.el ends here
