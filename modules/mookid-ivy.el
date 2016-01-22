;;; mookid-ivy --- Configuration for ivy

;;; Commentary:

;;; Code:
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(require 'counsel)
(define-key isearch-mode-map (kbd "M-s") 'swiper-from-isearch)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
(global-set-key (kbd "C-x m") 'counsel-M-x)

(defvar evil-normal-state-map)
(define-key evil-normal-state-map (kbd "C-b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)

(provide 'mookid-ivy)
;;; mookid-ivy.el ends here
