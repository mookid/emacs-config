;;; mookid-anzu.el --- Configuration of anzu

;;; Commentary:

;;; Code:
(require 'anzu)
(require 'diminish)
(diminish 'anzu-mode)

(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)

(provide 'mookid-anzu)
;;; mookid-anzu.el ends here
