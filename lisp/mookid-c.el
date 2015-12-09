;;; mookid-c --- Customizations related to the C language

;;; Commentary:

;;; Code:
(defvar c-mode-base-map)
(defun c-setup ()
  (setq-default c-default-style "linux" c-basic-offset 8)
  (define-key c-mode-base-map (kbd "C-c C-c") 'compile))
(add-hook 'c-initialization-hook 'c-setup)

(provide 'mookid-c)
;;; mookid-c.el ends here
