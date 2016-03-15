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

(provide 'mookid-colors)
;;; mookid-colors.el ends here
