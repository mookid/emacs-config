;;; mookid-evil-numbers.el --- Configuration of evil-numbers

;;; Commentary:

;;; Code:
(with-eval-after-load 'evil
  (require 'evil-numbers)
  (defvar evil-normal-state-map)
  (global-set-key (kbd (format "C-<f1>")) 'evil-numbers/inc-at-pt)
  (global-set-key (kbd (format "C-<f2>")) 'evil-numbers/dec-at-pt))

(provide 'mookid-evil-numbers)
;;; mookid-evil-numbers.el ends here
