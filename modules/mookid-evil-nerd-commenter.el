;;; mookid-evil-nerd-commenter --- Configuration for evil-nerd-commenter

;;; Commentary:

;;; Code:
(require 'evil-nerd-commenter)
(define-key global-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(define-key global-map (kbd "C-c c") 'evilnc-copy-and-comment-lines)

(provide 'mookid-evil-nerd-commenter)
;;; mookid-evil-nerd-commenter.el ends here
