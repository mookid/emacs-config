;;; mookid-avy --- Configuration for avy

;;; Commentary:

;;; Code:
(require 'avy)
(setq-default avy-all-windows 'all-frames)
(define-key global-map (kbd "C-:") 'avy-goto-word-or-subword-1)

(provide 'mookid-avy)
;;; mookid-avy.el ends here
