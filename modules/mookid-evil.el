;;; mookid-evil.el --- Configuration of emacs that depends on the evil package
;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'evil)
(evil-mode)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)

(require cl-lib)
(cl-macrolet
    ((f (key color shape black-fg?)
	(let ((color (prin1-to-string color)))
	  `(setq ,(intern (format "evil-%S-state-cursor" key))
		 ',(list color shape)
		 ,(intern (format "evil-%S-state-tag" key))
		 (propertize ,(prin1-to-string key)
			     'face '((:background
				      ,color
				      :foreground
				      ,(if (eq black-fg? :light)
					   "black"
					 "grey"))))))))
  (cl-loop
   for (key color shape black-fg?) in
   '((insert red bar :light)
     (motion gray box :light)
     (replace pink box :dark)
     (emacs purple box :dark)
     (visual green box :light)
     (normal grey box :light))
   do (f key color shape black-fg?)))

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
(define-key evil-normal-state-map (kbd "g m") 'evil-goto-mark)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

(provide 'mookid-evil)
;;; mookid-evil.el ends here
