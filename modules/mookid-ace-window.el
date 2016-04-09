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
(setq aw-dispatch-alist (cons '(?v split-window-right) aw-dispatch-alist))
(setq aw-dispatch-alist (cons '(?p mookid-other-window) aw-dispatch-alist))
(setq aw-dispatch-alist (cons '(?b split-window-below) aw-dispatch-alist))
(setq aw-dispatch-alist (cons '(?c delete-window) aw-dispatch-alist))

;; Auto balance windows:
(mapc (lambda (fun) (advice-add fun :after #'balance-windows))
      '(split-window-right split-window-below delete-window))

(provide 'mookid-ace-window)
;;; mookid-ace-window.el ends here
