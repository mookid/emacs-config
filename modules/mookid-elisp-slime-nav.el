;;; mookid-elisp-slime-nav --- Configuration of elisp-slime-nav

;;; Commentary:
;; Entirely from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources

;;; Code:
(require 'elisp-slime-nav)
(require 'diminish)
(diminish 'elisp-slime-nav-mode)

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'lisp-interaction-mode 'elisp-slime-nav-mode)

(define-key global-map (kbd "C-\"") 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'mookid-elisp-slime-nav)
;;; mookid-elisp-slime-nav.el ends here
