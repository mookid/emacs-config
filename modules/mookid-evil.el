;; -*- lexical-binding: t -*-
;;; mookid-evil.el --- Configuration of emacs that depends on the evil package

;;; Commentary:

;;; Code:
(require 'evil)
(evil-mode)
(defvar evil-motion-state-map)
(defvar evil-normal-state-map)

(require 'cl-lib)
(defun gensetq (mode-colors-alist)
  (cl-loop for (mode . custom) in mode-colors-alist
	   for color = (symbol-name (cdr (assoc :color custom)))
	   for shape = (cdr (assoc :cursor-shape custom))
	   append `(,(intern (format "evil-%S-state-cursor" mode))
		    ',(list color shape)
		    ,(intern (format "evil-%S-state-tag" mode))
		    nil)))

(defun gencond (mode-colors-alist)
  (cl-loop for (mode . custom) in mode-colors-alist
	   for color = (symbol-name (cdr (assoc :color custom)))
	   for foreground = (let ((symb (cdr (assoc :foreground custom))))
			      (when symb (symbol-name symb)))
	   collect
	   `((,(intern (format "evil-%S-state-p" mode)))
	     '(,color ,foreground))))

(defmacro customize (&rest mode-colors-alist)
  `(progn
     (setq ,@(gensetq mode-colors-alist))
     (add-hook 'post-command-hook
	       (lambda ()
		 (let* ((colors (cond ,@(gencond mode-colors-alist)))
			(background (car colors))
			(foreground-opt (cdr colors)))
		   (set-face-background 'mode-line background)
		   (when foreground-opt
		     (set-face-foreground 'mode-line (car foreground-opt))))))))

(customize
 (insert . ((:color . red) (:foreground . white) (:cursor-shape . bar)))
 (motion . ((:color . SteelBlue) (:foreground . black) (:cursor-shape . box)))
 (replace . ((:color . DeepPink) (:foreground . black) (:cursor-shape . hbar)))
 (emacs . ((:color . purple) (:foreground . white) (:cursor-shape . box)))
 (visual . ((:color . green) (:foreground . black) (:cursor-shape . box)))
 (normal . ((:color . SteelBlue) (:foreground . white) (:cursor-shape . box))))

(defalias 'evil-previous-line 'evil-previous-visual-line)
(defalias 'evil-next-line 'evil-next-visual-line)

(setq-default evil-cross-lines t)

(defun ex-substitute ()
  "Call ex with the substitute prefix."
  (interactive) (evil-ex "%s/"))
(define-key evil-normal-state-map (kbd "g s") 'ex-substitute)
(define-key evil-normal-state-map (kbd "g m") 'evil-goto-mark)

(define-key evil-normal-state-map (kbd "`")
  (defun insert-newline () "That's it." (interactive) (newline)))

(define-key evil-visual-state-map (kbd "M-s") 'search-region)

(provide 'mookid-evil)
;;; mookid-evil.el ends here
