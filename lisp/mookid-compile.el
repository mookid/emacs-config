;;; mookid-compile.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Compilation customizations.

;;; Code:
(setq compilation-ask-about-save nil)
(setq-default compilation-always-kill t)
(setq-default compilation-scroll-output 'first-error)

;; disable it for grep mode:
(defun disable-jump-to-error ()
  (kill-local-variable 'compilation-auto-jump-to-next))
(add-hook 'grep-mode-hook 'disable-jump-to-error)
(define-key global-map (kbd "<f12>") 'recompile)
(define-key global-map (kbd "C-<prior>") 'previous-error)
(define-key global-map (kbd "C-<next>") 'next-error)

(provide 'mookid-compile)
;;; mookid-compile.el ends here
