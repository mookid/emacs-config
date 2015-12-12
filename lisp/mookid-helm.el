;;; mookid-helm --- Configuration for helm related packages

;;; Commentary:

;;; Code:
(autoload 'with-message "mookid-macros")

(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(helm-mode)

(provide 'mookid-helm)
;;; mookid-helm.el ends here
