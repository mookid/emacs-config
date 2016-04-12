;;; mookid-ocp-index --- Configuration for ocp-index

;;; Commentary:

;;; Code:
(require 'ocp-index)
(defvar tuareg-mode-map)
(defun mookid-ocp-index-definition (p)
  (interactive "P")
  (if p
      (ocp-index-jump-to-definition-at-point-other-window)
    (ocp-index-jump-to-definition-at-point)))

(defun mookid-ocp-index-sig (p)
  (interactive "P")
  (if p
      (ocp-index-jump-to-sig-at-point-other-window)
    (ocp-index-jump-to-sig-at-point)))

(defun mookid-ocp-index-setup ()
  "My setup for ocp-index."
  (define-key global-map (kbd "C-c t") 'caml-types-show-type)
  (define-key global-map (kbd "C-.") 'mookid-ocp-index-definition)
  (define-key global-map (kbd "C-,") 'mookid-ocp-index-sig))

(add-hook 'tuareg-mode-hook 'mookid-ocp-index-setup)

(provide 'mookid-ocp-index)
;;; mookid-ocp-index.el ends here
