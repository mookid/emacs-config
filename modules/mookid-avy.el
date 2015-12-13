;;; mookid-avy --- Configuration for avy

;;; Commentary:

;;; Code:
(require 'avy)
(setq-default avy-all-windows 'all-frames)
(mapc (lambda (map)
        (eval `(define-key ,map (kbd "s") 'avy-goto-word-or-subword-1)))
      '(evil-motion-state-map
        evil-visual-state-map
        evil-normal-state-map))

(provide 'mookid-avy)
;;; mookid-avy.el ends here
