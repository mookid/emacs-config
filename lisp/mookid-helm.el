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

(eval-after-load 'helm
  '(with-message
    "Loading shackle"
    (require 'shackle)
    (setq-default helm-display-function #'pop-to-buffer)
    (setq-default shackle-rules
                  '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.33)))
    (eval-after-load "init" (shackle-mode))))

(eval-after-load 'helm
  '(with-message
    "Loading helm swoop"
    (require 'helm-swoop)
    (global-set-key (kbd "C-\\") 'helm-swoop-from-evil-search)))

(eval-after-load 'helm
  '(with-message
    "Loading helm projectile"
    (require 'helm-projectile)
    (projectile-global-mode)
    (setq-default projectile-indexing-method 'native)
    (setq-default projectile-enable-caching t)
    (global-set-key (kbd "<f5>") 'helm-projectile)))

(provide 'mookid-helm)
;;; mookid-helm.el ends here
