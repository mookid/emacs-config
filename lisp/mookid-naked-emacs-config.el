;;; mookid-naked-emacs-config.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of Emacs that does not depend on external packages.

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'mookid-root-dir "mookid-values")

;; Remove silly message
(defun display-startup-echo-area-message () "Inhibit welcome message." ())

;; Start with a blank scratch buffer
(setq initial-scratch-message nil)

(global-set-key (kbd "M-r") 'raise-sexp)

(defun last-2 (list)
  "Remove all the elements of LIST except the last two."
  (let ((lst list))
    (while (caddr lst)
      (setq lst (cdr lst)))
    lst))

(defun shorten-path (path)
  "Shortens the string representing a PATH for the modeline."
  (let ((r (string-join (cons "..." (last-2 (split-string path "/"))) "/")))
    (if (< (length r) (length path)) r path)))

(with-message
 "Set mode line format"
 (with-eval-after-load 'mookid-evil
   (make-face 'mode-line-folder-face)
   (make-face 'mode-line-filename-face)
   (set-face-attribute 'mode-line-filename-face nil
		       :weight 'bold)
   (setq-default mode-line-format
		 (list
		  "  "
		  mode-line-position
		  '(:propertize
		    (:eval (when buffer-file-name
			     (shorten-path default-directory)))
		    face mode-line-folder-face)
		  '(:propertize "%b" face mode-line-filename-face)
		  "%n  "
		  mode-line-modes
		  mode-line-misc-info
		  "%-"))))

;; Disable the bell
(setq ring-bell-function 'ignore)

;; Default behaviour for newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; No welcome message
(setq inhibit-startup-message t)

;; Move backup files to a subdirectory of the root directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" mookid-root-dir))))

;; Stop auto save
(setq auto-save-default nil)

;; Auto revert
(global-auto-revert-mode 1)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; No tabs
(setq indent-tabs-mode nil)

;; Delete trailing whitespaces when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable region narrowing
(put 'narrow-to-region 'disabled nil)

;; Save all buffers when focus is lost
(defun save-all-buffers () "Save all buffers." (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all-buffers)

;; Use proportional fonts
(define-globalized-minor-mode
  global-variable-pitch-mode
  buffer-face-mode
  (lambda () (variable-pitch-mode 1)))

(global-variable-pitch-mode 1)

(with-message
 "Remove gui elements"
 (and (fboundp 'fringe-mode) (fringe-mode 0))
 (and (fboundp 'tooltip-mode) (tooltip-mode -1))
 (and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
 (and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
 (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
 (and (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
 (setq pop-up-windows nil))


;; Save history between sessions
(setq-default savehist-file
	      (expand-file-name "savehist" mookid-root-dir))
(savehist-mode t)
(setq history-length 16384)
(setq history-delete-duplicates t)
(setq-default savehist-save-minibuffer-history t)
(setq-default savehist-additional-variables
	      '(kill-ring search-ring regexp-search-ring))

(with-message
 "Configuring parenthesis settings"
 (electric-pair-mode t)
 (setq-default electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 (set-face-background 'show-paren-match "turquoise")
 (setq-default show-paren-delay 0))

(with-message
 "Setting up unicode"
 (defvar default-font nil "The font used almost everywhere.")
 (setq default-font "Source Code Pro Light")
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist `(font . ,default-font))
 (global-prettify-symbols-mode 1))

(with-message
 "Customize proportional font"
 (set-face-attribute 'variable-pitch nil
		     :family "DejaVu Sans"))

(with-message
 "Switch parens with brackets"
 (define-key input-decode-map (kbd "(") (kbd "["))
 (define-key input-decode-map (kbd "[") (kbd "("))
 (define-key input-decode-map (kbd ")") (kbd "]"))
 (define-key input-decode-map (kbd "]") (kbd ")")))

(with-message
 "Window switch bindings"
 ;; unbind all keybindings starting with f2
 (cl-dolist (keystr '("<f2> 2" "<f2> b" "<f2> s"))
   (global-unset-key (kbd keystr)))

 (with-eval-after-load 'init
   (global-set-key (kbd "<f2> <f2>") 'toggle-window-split))

 (defun toggle-window-split ()
   "When there are two windows, convert horizontal to vertical and vice versa."
   (interactive)
   (or (= (count-windows) 2)
       (error "You need exactly 2 windows to do this"))
   (let* ((this-win-buffer (window-buffer))
	  (next-win-buffer (window-buffer (next-window)))
	  (this-win-edges (window-edges (selected-window)))
	  (next-win-edges (window-edges (next-window)))
	  (this-win-2nd (not (and (<= (car this-win-edges)
				      (car next-win-edges))
				  (<= (cadr this-win-edges)
				      (cadr next-win-edges)))))
	  (splitter
	   (if (= (car this-win-edges)
		  (car next-win-edges))
	       'split-window-horizontally
	     'split-window-vertically)))
     (delete-other-windows)
     (let ((first-win (selected-window)))
       (funcall splitter)
       (when this-win-2nd (other-window 1))
       (set-window-buffer (selected-window) this-win-buffer)
       (set-window-buffer (next-window) next-win-buffer)
       (select-window first-win)
       (when this-win-2nd (other-window 1))))))

(with-message
 "Setting up the order for recenter-top-bottom"
 (setq recenter-positions '(top middle bottom)))

;; Mark dired-ignored
(with-eval-after-load 'dired
  (set-face-attribute 'dired-ignored nil
		      :strike-through t
		      :foreground "green"))

;; Wrap long lines
(global-visual-line-mode 1)
(require 'diminish)
(diminish 'visual-line-mode)

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(winner-mode 1)

;; Run Cygwin shell
(setq-default explicit-shell-file-name "C:/bin/bash")

(defun insert-buffer-name ()
  "Insert the previous buffer name.  Usefull for compilation."
  (interactive)
  (insert (buffer-name (other-buffer (current-buffer) 1))))
(global-set-key (kbd "C-c C-v") 'insert-buffer-name)

;; from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html:
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first.  Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning) (region-end)))
	;; ((derived-mode-p 'org-mode)
	;;  ;; `org-edit-src-code' is not a real narrowing
	;;  ;; command. Remove this first conditional if you
	;;  ;; don't want it.
	;;  (cond ((ignore-errors (org-edit-src-code))
	;;         (delete-other-windows))
	;;        ((ignore-errors (org-narrow-to-block) t))
	;;        (t (org-narrow-to-subtree))))
	;; ((derived-mode-p 'latex-mode)
	;;  (LaTeX-narrow-to-environment))
	(t (narrow-to-defun))))

(global-set-key (kbd "C-x n n") 'narrow-or-widen-dwim)

(provide 'mookid-naked-emacs-config)
;;; mookid-naked-emacs-config.el ends here
