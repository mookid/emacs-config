;;; mookid-magit --- Configuration of magit.

;;; Commentary:

;;; Code:
(autoload 'magit-status "magit")

(global-set-key (kbd "<f7>") 'magit-status)
(provide 'mookid-magit)
;;; mookid-magit.el ends here
