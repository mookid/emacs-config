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
(setq aw-dispatch-alist (cons '(?v evil-window-vsplit) aw-dispatch-alist))
(setq aw-dispatch-alist (cons '(?\' mookid-other-window) aw-dispatch-alist))
(setq aw-dispatch-alist (cons '(?b evil-window-split) aw-dispatch-alist))
(setq aw-dispatch-alist (cons '(?c evil-window-delete) aw-dispatch-alist))
(provide 'mookid-ace-window)
;;; mookid-ace-window.el ends here
