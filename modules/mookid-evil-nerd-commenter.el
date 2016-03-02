;;; mookid-evil-nerd-commenter --- Configuration for evil-nerd-commenter

;;; Commentary:

;;; Code:
(require 'evil-nerd-commenter)
(with-eval-after-load 'evil
  (defvar evil-normal-state-map)
  (defvar evil-visual-state-map)
  (define-key evil-normal-state-map (kbd ",") 'evilnc-comment-operator)
  (define-key evil-visual-state-map (kbd ",") 'evilnc-comment-operator)
  (define-key evil-normal-state-map (kbd "M-,") 'evilnc-copy-and-comment-lines)
  (define-key evil-visual-state-map (kbd "M-,") 'evilnc-copy-and-comment-lines))
(provide 'mookid-evil-nerd-commenter)
;;; mookid-evil-nerd-commenter.el ends here
