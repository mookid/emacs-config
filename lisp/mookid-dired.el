;;; mookid-dired.el --- Configuration of dired

;;; Commentary:

;;; Code:
(require 'dired)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "g j") 'dired-jump)
  (define-key dired-mode-map (kbd "j") 'dired-jump)
  (evil-set-initial-state 'dired-mode 'emacs))

(provide 'mookid-dired)
;;; mookid-dired.el ends here
