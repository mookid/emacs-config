;;; mookid-elisp-slime-nav --- Configuration of elisp-slime-nav

;;; Commentary:
;; Entirely from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources

;;; Code:
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(provide 'mookid-elisp-slime-nav)
;;; mookid-elisp-slime-nav.el ends here
