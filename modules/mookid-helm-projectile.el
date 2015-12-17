;;; mookid-helm-projectile --- Configuration for helm-projectile

;;; Commentary:

;;; Code:

(with-eval-after-load 'helm
  (require 'helm-projectile)
  (require 'projectile)
  (projectile-global-mode)
  (setq-default projectile-indexing-method 'native)
  (setq-default projectile-enable-caching t)
  (global-set-key (kbd "<f5>") 'helm-projectile)
  (global-set-key (kbd "<C-f5>") 'projectile-switch-project))

(provide 'mookid-helm-projectile)
;;; mookid-helm-projectile.el ends here
