;;; mookid-ivy --- Configuration for ivy

;;; Commentary:

;;; Code:
(require 'ivy)
(require 'diminish)
(diminish 'ivy-mode)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(require 'counsel)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-x") 'counsel-M-x)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)

(defvar evil-normal-state-map)
(define-key evil-normal-state-map (kbd "C-b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "RET") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "<C-return>") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC") 'counsel-M-x)

(define-key grep-mode-map (kbd "RET") 'ivy-switch-buffer)

(provide 'mookid-ivy)
;;; mookid-ivy.el ends here
