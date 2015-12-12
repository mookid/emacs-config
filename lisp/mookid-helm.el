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
 (with-eval-after-load 'helm
   (require 'shackle)
   (setq-default helm-display-function #'pop-to-buffer)
   (setq-default shackle-rules
		 '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.33)))
   (with-eval-after-load "init" (shackle-mode))))

(with-message
 "Loading helm swoop"
 (with-eval-after-load 'helm
   (require 'helm-swoop)
   (global-set-key (kbd "C-\\") 'helm-swoop-from-evil-search)))

(with-message
 "Loading helm projectile"
 (with-eval-after-load 'helm
   (require 'helm-projectile)
   (require 'projectile)
   (projectile-global-mode)
   (setq-default projectile-indexing-method 'native)
   (setq-default projectile-enable-caching t)
   (global-set-key (kbd "<f5>") 'helm-projectile)))

(provide 'mookid-helm)
;;; mookid-helm.el ends here
