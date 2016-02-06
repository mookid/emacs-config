;;; mookid-evil-anzu.el --- Configuration of evil-anzu

;;; Commentary:

;;; Code:
(with-eval-after-load 'evil (require 'evil-anzu) (global-anzu-mode +1))

(global-set-key (kbd "M-%") 'anzu-query-regexp-replace)

(provide 'mookid-evil-anzu)
;;; mookid-evil-anzu.el ends here
