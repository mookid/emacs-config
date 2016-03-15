;;; mookid-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of Emacs colors.

;;; Code:
(load-theme 'wombat)
(defvar *default-light-color* nil "Foreground for most faces.")
(setq *default-light-color* "cornsilk1")
(let ((theme-default-foreground (face-attribute 'default :foreground)))
  (mapc (lambda (face)
	  (when (string= (face-attribute face :foreground)
			 theme-default-foreground)
	    (set-face-foreground face *default-light-color*)))
	(face-list)))

(set-face-attribute 'error nil :foreground "deep pink")

(set-face-attribute 'font-lock-variable-name-face nil :foreground "white")
(set-face-attribute 'font-lock-function-name-face nil :foreground "white")
(set-face-attribute 'font-lock-type-face nil :foreground "white")
(set-face-attribute 'font-lock-string-face nil :foreground "pale green")
(set-face-attribute 'font-lock-builtin-face nil :foreground "lavender")
(set-face-attribute 'font-lock-constant-face nil :foreground "light sky blue")

(provide 'mookid-colors)
;;; mookid-colors.el ends here
