;;; mookid-merlin --- Configuration for merlin

;;; Commentary:

;; Setup of merlin, using company as a completion backend.

;;; Code:
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(defun merlin-setup ()
  "My setup for merlin."
  (setq-default merlin-use-auto-complete-mode 'easy)
  (defvar company-backends)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))
(add-hook 'merlin-mode-hook 'merlin-setup)

(provide 'mookid-merlin)
;;; mookid-merlin.el ends here
