;;; mookid-projectile --- Configuration for projectile

;;; Commentary:

;;; Code:
(require 'projectile)

(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-mode-line '(:eval (concat " <" (projectile-project-name) ">")))
(defun mookid-projectile (p)
  "My projectile command.

If P is non nil, call `projectile-find-file' else call `projectile-switch-project'."
  (interactive "P")
  (if p (projectile-switch-project) (projectile-find-file)))
(global-set-key (kbd "<C-S-return>") 'mookid-projectile)

(setq projectile-completion-system 'ivy)

(provide 'mookid-projectile)
;;; mookid-projectile.el ends here
