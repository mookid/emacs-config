;;; mookid-c --- Customizations related to the C language

;;; Commentary:

;;; Code:
(defvar c-mode-base-map)
(defvar c-indentation 8 "The indentation for C code.")
(defun c-setup ()
  "My setup for C."
  (setq-default c-default-style "linux" c-basic-offset c-indentation)
  (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-c C-a") 'ff-find-other-file)
  (setq-default indent-tabs-mode nil)
  (require 'clang-format))

(add-hook 'c-initialization-hook 'c-setup)

(provide 'mookid-c)
;;; mookid-c.el ends here
