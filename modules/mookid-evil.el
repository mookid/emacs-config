;;; mookid-evil.el --- Configuration of evil ;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'evil)
(evil-mode)

(with-eval-after-load 'undo-tree
  (require 'diminish)
  (diminish 'undo-tree-mode))

(defvar evil-motion-state-map)
(defvar evil-normal-state-map)

(require 'cl-lib)

(defmacro customize (&rest mode-colors-alist)
     "Customization code for evil.

Expects MODE-COLORS-ALIST with elements of the form (MODE . CUSTOMIZATIONS).
The currently used CUSTOMIZATIONS are:
* color
* cursor shape"
     (cl-flet
	 ((gensetq (mode-colors-alist)
		   (cl-loop for (mode . custom) in mode-colors-alist
			    for color = (cdr (assoc :color custom))
			    for shape = (cdr (assoc :cursor-shape custom))
			    append `(,(intern (format "evil-%S-state-cursor" mode))
				     ',(list color shape)
				     ,(intern (format "evil-%S-state-tag" mode))
				     nil)))

	  (gencond (mode-colors-alist)
		   (cl-loop for (mode . custom) in mode-colors-alist
			    for color = (cdr (assoc :color custom))
			    for foreground = (let ((symb (cdr (assoc :foreground custom))))
					       (when symb (symbol-name symb)))
			    collect
			    `((,(intern (format "evil-%S-state-p" mode)))
			      '(,color ,foreground)))))
       `(progn
	  (setq ,@(gensetq mode-colors-alist))
	  (add-hook 'post-command-hook
		    (lambda ()
		      (let* ((colors (cond ,@(gencond mode-colors-alist)))
			       (background (car colors))
			       (foreground-opt (cdr colors)))
			  (set-face-background 'mode-line background)
			  (when foreground-opt
			    (set-face-foreground 'mode-line (car foreground-opt)))))))))

(customize
 (insert . ((:color . "red") (:foreground . white) (:cursor-shape . bar)))
 (motion . ((:color . "cyan") (:foreground . black) (:cursor-shape . box)))
 (replace . ((:color . "deep pink") (:foreground . white) (:cursor-shape . hbar)))
 (emacs . ((:color . "purple") (:foreground . white) (:cursor-shape . box)))
 (visual . ((:color . "green") (:foreground . black) (:cursor-shape . box)))
 (normal . ((:color . "cyan") (:foreground . black) (:cursor-shape . box))))

(defalias 'evil-previous-line 'evil-previous-visual-line)
(defalias 'evil-next-line 'evil-next-visual-line)

(setq-default evil-cross-lines t)

(define-key evil-normal-state-map (kbd "g s")
  (defun ex-substitute ()
    "Call ex with the substitute prefix."
    (interactive) (evil-ex "%s/")))

(define-key evil-normal-state-map (kbd "g !")
  (defun ex-external ()
    "Call ex with the external command prefix."
    (interactive) (evil-ex "%!")))

(define-key evil-normal-state-map (kbd "l") 'evil-repeat-find-char)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

(define-key evil-normal-state-map (kbd "`")
  (defun insert-newline () "That's it." (interactive) (newline)))

(global-set-key (kbd "C-s") 'evil-search-forward)

;; set insert mode = emacs mode
(setq evil-move-beyond-eol t)
(define-key evil-motion-state-map (kbd "C-a") 'evil-first-non-blank)
(define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "C-y") 'yank)
(define-key evil-normal-state-map (kbd "(") 'backward-paragraph)
(define-key evil-normal-state-map (kbd ")") 'forward-paragraph)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;; dired settings
(require 'dired)
(define-key evil-normal-state-map (kbd "g j") 'dired-jump)
(define-key dired-mode-map (kbd "j") 'dired-jump)
(evil-set-initial-state 'dired-mode 'emacs)

(provide 'mookid-evil)
;;; mookid-evil.el ends here
