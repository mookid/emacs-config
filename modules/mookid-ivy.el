;;; mookid-ivy --- Configuration for ivy

;;; Commentary:

;;; Code:
(require 'ivy)
(require 'diminish)
(diminish 'ivy-mode)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(require 'counsel)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)

(define-key global-map (kbd "C-M-y") 'counsel-yank-pop)
(define-key global-map (kbd "M-x") 'counsel-M-x)
(define-key global-map (kbd "C-x <return>") 'counsel-M-x)
(define-key global-map (kbd "M-m") 'counsel-M-x)
(define-key global-map (kbd "<M-return>") 'ivy-switch-buffer)
(define-key global-map (kbd "<C-return>") 'counsel-find-file)

(add-hook 'grep-setup-hook
          (lambda () (define-key grep-mode-map (kbd "RET") 'ivy-switch-buffer)))

(provide 'mookid-ivy)
;;; mookid-ivy.el ends here
