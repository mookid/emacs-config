;;; mookid-slime.el --- Configuration of slime

;;; Commentary:

;;; Code:
(defvar slime-mode-map nil)

(require 'slime-autoloads)
(setq-default inferior-lisp-program "sbcl")
(add-hook 'comint-mode-hook 'rainbow-delimiters-mode)
(setq-default common-lisp-hyperspec-root "file:///Hyperspec/")
(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c h") 'hyperspec-lookup))

(provide 'mookid-slime)
;;; mookid-slime.el ends here
