;;; mookid-flycheck --- Configuration for flycheck mode

;;; Commentary:

;;; Code:
(with-eval-after-load 'flycheck
  (defvar flycheck-mode-map)
  (define-key flycheck-mode-map (kbd "C-S-<next>") 'flycheck-next-error))

(provide 'mookid-flycheck)
;;; mookid-flycheck.el ends here
