;;; mookid-helm --- Configuration for helm related packages

;;; Commentary:

;;; Code:
(autoload 'with-message "mookid-macros")

(with-message
 "Loading helm"
 (require 'helm)
 (global-set-key (kbd "M-x") 'helm-M-x)
 (global-set-key (kbd "C-x C-m") 'helm-M-x)
 (global-set-key (kbd "C-x m") 'helm-M-x)
 (helm-mode))

(with-message "Loading shackle" (require 'mookid-shackle))
(with-message "Loading helm swoop" (require 'mookid-helm-swoop))
(with-message "Loading helm projectile" (require 'mookid-helm-projectile))

(provide 'mookid-helm)
;;; mookid-helm.el ends here
