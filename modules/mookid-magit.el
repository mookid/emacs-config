;;; mookid-magit --- Configuration of magit.

;;; Commentary:

;;; Code:
(autoload 'magit-status "magit")
(autoload 'magit-mode-quit-window "magit")

(global-set-key (kbd "<f7>") 'magit-status)

(require 'fullframe)
(fullframe magit-status magit-mode-quit-window)

(provide 'mookid-magit)
;;; mookid-magit.el ends here
