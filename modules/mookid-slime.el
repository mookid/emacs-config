;;; mookid-slime.el --- Configuration of slime

;;; Commentary:

;;; Code:
(require 'slime-autoloads)
(setq-default inferior-lisp-program "sbcl")
(add-hook 'comint-mode-hook 'rainbow-delimiters-mode)

(provide 'mookid-slime)
;;; mookid-slime.el ends here
