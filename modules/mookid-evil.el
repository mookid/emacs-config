;;; mookid-evil.el --- Configuration of emacs that depends on the evil package

;;; Commentary:

;;; Code:
(autoload 'with-message "mookid-macros")

(require 'evil)
(evil-mode)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)
(setq-default evil-emacs-state-cursor '("purple" box))
(setq-default evil-normal-state-cursor '("grey" box))
(setq-default evil-visual-state-cursor '("green" box))
(setq-default evil-insert-state-cursor '("red" bar))
(setq-default evil-replace-state-cursor '("deep pink" box))
(setq-default evil-motion-state-cursor '("gray" box))
(define-key evil-normal-state-map
  (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map
  (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map
  (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map
  (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(setq-default evil-cross-lines t)
(defun ex-substitute ()
  "Call ex with the substitute prefix."
  (interactive) (evil-ex "%s/"))
(define-key evil-normal-state-map (kbd "g s") 'ex-substitute)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

(provide 'mookid-evil)
;;; mookid-evil.el ends here
