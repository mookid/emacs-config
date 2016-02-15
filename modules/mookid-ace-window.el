;;; mookid-ace-window --- Configuration for ace-window

;;; Commentary:

;;; Code:
(require 'ace-window)
(autoload 'bind-key-non-insert-mode "mookid-evil")
(with-eval-after-load 'evil
  (bind-key-non-insert-mode (kbd "'") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-dispatch-alist (cons '(?v evil-window-vsplit) aw-dispatch-alist))
  (setq aw-dispatch-alist (cons '(?b evil-window-split) aw-dispatch-alist))
  (setq aw-dispatch-alist (cons '(?c evil-window-delete) aw-dispatch-alist)))
(provide 'mookid-ace-window)
;;; mookid-ace-window.el ends here
