;; -*- lexical-binding: t -*-
;;; mookid-evil.el --- Configuration of emacs that depends on the evil package

;;; Commentary:

;;; Code:
(require 'evil)
(evil-mode)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)

(defun gensetq (mode-colors-alist)
  (loop for (mode . custom) in mode-colors-alist
	for color = (symbol-name (cdr (assoc :color custom)))
	for shape = (cdr (assoc :cursor-shape custom))
	append `(,(intern (format "evil-%S-state-cursor" mode))
		 ',(list color shape)
		 ,(intern (format "evil-%S-state-tag" mode))
		 nil)))

(defun gencond (mode-colors-alist)
  (loop for (mode . custom) in mode-colors-alist
	for color = (symbol-name (cdr (assoc :color custom)))
	for foreground = (let ((symb (cdr (assoc :foreground custom))))
			   (when symb (symbol-name symb)))
	collect
	`((,(intern (format "evil-%S-state-p" mode)))
	  '(,color ,foreground))))

(defmacro customize (mode-colors-alist)
  `(progn
     (setq ,@(gensetq mode-colors-alist))
     (add-hook 'post-command-hook
	       (lambda ()
		 (let* ((colors (cond ,@(gencond mode-colors-alist)))
			(background (car colors))
			(foreground-opt (cdr colors))
			)
		   (set-face-background 'mode-line background)
		   (when foreground-opt
		     (set-face-foreground 'mode-line (car foreground-opt))))))))

(customize
 ((insert . ((:color . red) (:foreground . white) (:cursor-shape . bar)))
  (motion . ((:color . gray) (:foreground . black) (:cursor-shape . box)))
  (replace . ((:color . DeepPink) (:foreground . black) (:cursor-shape . hbar)))
  (emacs . ((:color . purple) (:foreground . white) (:cursor-shape . box)))
  (visual . ((:color . green) (:foreground . black) (:cursor-shape . box)))
  (normal . ((:color . grey) (:foreground . black) (:cursor-shape . box)))))

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

(define-key evil-normal-state-map (kbd "`") (kbd "i RET <escape>"))

(provide 'mookid-evil)
;;; mookid-evil.el ends here
