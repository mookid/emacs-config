;;; mookid-avy --- Configuration for avy

;;; Commentary:

;;; Code:
(require 'avy)
(with-eval-after-load 'evil
  (setq-default avy-all-windows 'all-frames)
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-or-subword-1))

(provide 'mookid-avy)
;;; mookid-avy.el ends here
