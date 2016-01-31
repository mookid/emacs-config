;;; mookid-avy --- Configuration for avy

;;; Commentary:

;;; Code:
(require 'avy)
(autoload 'bind-key-non-insert-mode "mookid-evil")
(with-eval-after-load 'evil
  (setq-default avy-all-windows 'all-frames)
  (bind-key-non-insert-mode (kbd "s") 'avy-goto-word-or-subword-1))

(provide 'mookid-avy)
;;; mookid-avy.el ends here
