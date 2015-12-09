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

(with-message
 "Loading shackle"
 (require 'shackle)
 (setq-default helm-display-function #'pop-to-buffer)
 (setq-default shackle-rules
               '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.33)))
 (eval-after-load "init" (shackle-mode)))

(with-message
 "Loading helm swoop"
 (require 'helm-swoop)
 (global-set-key (kbd "C-\\") 'helm-swoop-from-evil-search))

(provide 'mookid-helm)
;;; mookid-helm.el ends here
