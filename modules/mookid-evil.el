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
 (motion . ((:color . cyan) (:foreground . black) (:cursor-shape . box)))
 (replace . ((:color . DeepPink) (:foreground . black) (:cursor-shape . hbar)))
 (emacs . ((:color . purple) (:foreground . white) (:cursor-shape . box)))
 (visual . ((:color . green) (:foreground . black) (:cursor-shape . box)))
 (normal . ((:color . cyan) (:foreground . black) (:cursor-shape . box))))


(make-face 'mode-line-evil-insert-face)
(make-face 'mode-line-evil-motion-face)
(make-face 'mode-line-evil-replace-face)
(make-face 'mode-line-evil-emacs-face)
(make-face 'mode-line-evil-visual-face)
(make-face 'mode-line-evil-normal-face)

(set-face-attribute 'mode-line-evil-insert-face nil
		    :inherit 'mode-line-face
		    :background "red"
		    :foreground "white")

(set-face-attribute 'mode-line-evil-normal-face nil
		    :inherit 'mode-line-face
		    :background "cyan"
		    :foreground "black")

(set-face-attribute 'mode-line-evil-replace-face nil
		    :inherit 'mode-line-face
		    :background "deep pink"
		    :foreground "black")

(set-face-attribute 'mode-line-evil-emacs-face nil
		    :inherit 'mode-line-face
		    :background "black"
		    :foreground "white")

(set-face-attribute 'mode-line-evil-visual-face nil
		    :inherit 'mode-line-face
		    :background "lawn green"
		    :foreground "black")

(set-face-attribute 'mode-line-evil-motion-face nil
		    :inherit 'mode-line-evil-normal-face)

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

(defun search-region (beg end)
  "Search for the text beween BEG and END."
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (isearch-mode t nil nil nil)
    (isearch-yank-string selection)))

(define-key evil-visual-state-map (kbd "M-s .") 'search-region)

(defmacro bind-key-non-insert-mode (kbd fun)
  "Binds the `key-binding' KBD to FUN in modes different to insert mode."
  `(progn
     ,@(cl-loop
	for map in '(evil-motion-state-map
		     evil-visual-state-map
		     evil-normal-state-map)
	collect `(define-key ,map ,kbd ,fun))))

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
