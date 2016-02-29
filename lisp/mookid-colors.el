;;; mookid-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of Emacs colors.

;;; Code:
(load-theme 'leuven)
(with-eval-after-load 'init
  (set-background-color "grey4")
  (set-foreground-color "ivory2")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "coral")
  (set-face-attribute 'font-lock-doc-face nil :foreground "coral")
  (set-face-attribute 'font-lock-string-face nil :foreground "orange red")
  (set-face-attribute 'font-lock-type-face nil :foreground "orange red")
  (set-face-attribute 'font-lock-keyword-face nil
		      :weight 'bold
		      :foreground "deep sky blue")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "deep sky blue")

  (set-face-attribute 'font-lock-function-name-face nil :foreground "violet")
  (set-face-attribute 'region nil :background "grey17")
  (set-face-attribute 'match nil :foreground "blue")
  (set-face-attribute 'compilation-info nil :foreground "deep sky blue")

  (set-face-attribute 'eldoc-highlight-function-argument nil :inherit 'default)
  (set-face-attribute 'help-argument-name nil :inherit 'default))

(provide 'mookid-colors)
;;; mookid-colors.el ends here
