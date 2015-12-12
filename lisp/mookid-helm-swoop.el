;;; mookid-helm-swoop --- Configuration for helm-swoop

;;; Commentary:

;;; Code:

(with-eval-after-load 'helm
   (require 'helm-swoop)
   (global-set-key (kbd "C-\\") 'helm-swoop-from-evil-search))

(provide 'mookid-helm-swoop)
;;; mookid-helm-swoop.el ends here
