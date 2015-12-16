;;; mookid-evil-jumper.el --- Configuration of evil-jumper

;;; Commentary:
;; Use C-i/C-o to jump forward/backward between positions.

;;; Code:
(with-eval-after-load 'evil
  (require 'evil-jumper)
  (evil-jumper-mode t))

(provide 'mookid-evil-jumper)
;;; mookid-evil-jumper.el ends here
