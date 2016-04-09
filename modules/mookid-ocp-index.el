;;; mookid-ocp-index --- Configuration for ocp-index

;;; Commentary:

;;; Code:
(require 'ocp-index)
(defvar tuareg-mode-map)
(defun ocp-index-jump-to-definition-at-point-maybe-other-window (p)
  (interactive "P")
  (if p
      (ocp-index-jump-to-definition-at-point-other-window)
    (ocp-index-jump-to-definition-at-point)))

(defun ocp-index-jump-to-sig-at-point-maybe-other-window (p)
  (interactive "P")
  (if p
      (ocp-index-jump-to-sig-at-point-other-window)
    (ocp-index-jump-to-sig-at-point)))

(defun ocp-index-setup ()
  "My setup for ocp-index."
  (define-key global-map (kbd "C-c t") 'caml-types-show-type)
  (define-key global-map (kbd "C-.")
    'ocp-index-jump-to-definition-at-point-maybe-other-window)
  (define-key global-map (kbd "C-,")
    'ocp-index-jump-to-sig-at-point-maybe-other-window))

(add-hook 'tuareg-mode-hook 'ocp-index-setup)

(provide 'mookid-ocp-index)
;;; mookid-ocp-index.el ends here
