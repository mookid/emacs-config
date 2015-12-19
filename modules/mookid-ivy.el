;;; mookid-ivy --- Configuration for ivy

;;; Commentary:

;;; Code:
(require 'ivy)
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
(setq ivy-use-virtual-buffers t)

(provide 'mookid-ivy)
;;; mookid-ivy.el ends here
