;;; mookid-shackle --- Configuration for shackle

;;; Commentary:

;;; Code:

(with-eval-after-load 'helm
   (require 'shackle)
   (setq-default helm-display-function #'pop-to-buffer)
   (setq-default shackle-rules
		 '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.33)))
   (with-eval-after-load "init" (shackle-mode)))

(provide 'mookid-shackle)
;;; mookid-shackle.el ends here
