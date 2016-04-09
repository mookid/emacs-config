;;; mookid-ace-window --- Configuration for ace-window

;;; Commentary:

;;; Code:
(require 'ace-window)
(defun mookid-other-window ()
  "Call `other-window' with argument 1."
  (interactive)
  (other-window 1))

(define-key global-map (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)
(add-to-list 'aw-dispatch-alist '(?v split-window-right))
(add-to-list 'aw-dispatch-alist '(?p mookid-other-window))
(add-to-list 'aw-dispatch-alist '(?b split-window-below))
(add-to-list 'aw-dispatch-alist '(?c delete-window))

;; Auto balance windows:
(mapc (lambda (fun) (advice-add fun :after #'balance-windows))
      '(split-window-right split-window-below delete-window))

(provide 'mookid-ace-window)
;;; mookid-ace-window.el ends here
