;;; mookid-c --- Customizations related to the C language

;;; Commentary:

;;; Code:
(defvar c-mode-base-map)
(add-hook 'c-initialization-hook
          (lambda ()
               (setq-default c-default-style "linux" c-basic-offset 8)
               (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))

(provide 'mookid-c)
;;; mookid-c.el ends here
