;;; mookid-ace-window --- Configuration for ace-window

;;; Commentary:

;;; Code:
(require 'ace-window)
(defun mookid-other-window ()
  "Forwards to `other-window'."
  (interactive)
  (other-window 1))

(define-key global-map (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)
(add-to-list 'aw-dispatch-alist '(?v mookid-split-window-right))
(add-to-list 'aw-dispatch-alist '(?p mookid-other-window))
(add-to-list 'aw-dispatch-alist '(?b mookid-split-window-below))
(add-to-list 'aw-dispatch-alist '(?c mookid-delete-window))

;; Auto balance windows:
(defun mookid-split-window-right ()
  "Forwards to `split-window-below' and rebalances."
  (split-window-right))

(defun mookid-split-window-below ()
  "Forwards to `split-window-below' and rebalances."
  (split-window-below))

(defun mookid-delete-window ()
  "Forwards to `delete-window' and rebalances."
  (delete-window))

(define-key global-map (kbd "C-x 0") 'mookid-delete-window)
(define-key global-map (kbd "C-x 2") 'mookid-split-window-below)
(define-key global-map (kbd "C-x 3") 'mookid-split-window-right)

(mapc (lambda (fun) (advice-add fun :after #'balance-windows))
      '(mookid-split-window-right mookid-split-window-below mookid-delete-window))

(provide 'mookid-ace-window)
;;; mookid-ace-window.el ends here
