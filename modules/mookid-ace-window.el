;;; mookid-ace-window --- Configuration for ace-window

;;; Commentary:

;;; Code:
(require 'ace-window)
(autoload 'bind-key-non-insert-mode "mookid-evil")
(with-eval-after-load 'evil
  (bind-key-non-insert-mode (kbd "'") 'ace-window))
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(provide 'mookid-ace-window)
;;; mookid-ace-window.el ends here
