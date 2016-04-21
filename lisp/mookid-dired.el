;;; mookid-dired.el --- Configuration of dired

;;; Commentary:

;;; Code:
(require 'dired)
(require 'dired-x)

(define-key dired-mode-map (kbd "M-<left>") 'dired-jump)
(define-key dired-mode-map (kbd "M-<right>") 'dired-find-file)

(define-key global-map (kbd "C-M-<left>") 'dired-jump)
(define-key global-map (kbd "C-M-<right>") 'mookid-find-thing)

(defun mookid-find-thing ()
  "Find variable, function or file at point."
  (interactive)
  (cond ((not (eq (variable-at-point) 0))
         (call-interactively 'describe-variable))
        ((function-called-at-point)
         (call-interactively 'describe-function))
        (t (find-file-at-point))))

(provide 'mookid-dired)
;;; mookid-dired.el ends here
