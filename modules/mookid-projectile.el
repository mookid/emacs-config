;;; mookid-projectile --- Configuration for projectile

;;; Commentary:

;;; Code:
(require 'projectile)

(projectile-global-mode)
(setq-default projectile-indexing-method 'native)
(setq-default projectile-enable-caching t)
(global-set-key (kbd "<f5>") 'projectile-find-file)
(global-set-key (kbd "<C-f5>") 'projectile-switch-project)

(setq projectile-completion-system 'ivy)

(provide 'mookid-projectile)
;;; mookid-projectile.el ends here
