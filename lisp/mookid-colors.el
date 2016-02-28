;;; mookid-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of Emacs colors.

;;; Code:
(load-theme 'leuven)
(set-background-color "grey4")
(set-foreground-color "ivory2")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "coral")
(set-face-attribute 'font-lock-doc-face nil :foreground "coral")
(set-face-attribute 'font-lock-string-face nil :foreground "orange red")
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-face-attribute 'font-lock-function-name-face nil :foreground "violet")

(provide 'mookid-colors)
;;; mookid-colors.el ends here
