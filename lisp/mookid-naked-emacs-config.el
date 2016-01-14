;; -*- lexical-binding: t -*-
;;; mookid-naked-emacs-config.el --- Configuration of naked emacs

;;; Commentary:
;; Configuration of Emacs that does not depend on external packages.

;;; Code:
(autoload 'with-message "mookid-macros")

;; Set color theme
(load-theme 'deeper-blue)

;; Remove silly message
(defun display-startup-echo-area-message () "Inhibit welcome message." ())

;; Use the frame title
(setq frame-title-format
      '("%S " (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Define an alias for dabbrev-expand
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)

;; No transient mark mode; use visual mode of evil instead
(defun disable-transient-mark ()
  "Disable transient mode.

I add this hook because it seems that some package activates it."
  (transient-mark-mode -1))
(add-hook 'after-init-hook 'disable-transient-mark)

;; Easy switch from one window to another
(windmove-default-keybindings)

;; Disable the bell
(setq ring-bell-function 'ignore)

;; Fixed size mini-window
(setq resize-mini-windows nil)

;; Default behaviour for newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Switch to messages buffer at startup
(setq inhibit-startup-message t)
(switch-to-buffer "*Messages*")

;; Move backup files to a subdirectory of ~/.emacs.d
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

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

(with-message
 "Remove gui elements"
 (tooltip-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (scroll-bar-mode +1)
 (blink-cursor-mode -1))

(with-message
 "Compilation settings"
 (setq compilation-ask-about-save nil)
 (setq-default compilation-always-kill t)
 (setq-default compilation-scroll-output 'first-error)
 ;; disable it for grep mode:
 (defun disable-jump-to-error ()
   (kill-local-variable 'compilation-auto-jump-to-next))
 (add-hook 'grep-mode-hook 'disable-jump-to-error)
 (global-set-key (kbd "<f12>") 'recompile)
 (global-set-key (kbd "C-<next>") 'next-error))

;; Save history between sessions
(setq-default savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq history-length 16384)
(setq history-delete-duplicates t)
(setq-default savehist-save-minibuffer-history t)
(setq-default savehist-additional-variables
              '(kill-ring search-ring regexp-search-ring))

(with-message
 "Setting up selective display"
 (let ((depth 1))
   (global-set-key (kbd "<f6>") 'toggle-selective-display)
   (global-set-key (kbd "C-<f6>") 'increase-selective-display)
   (global-set-key (kbd "S-<f6>") 'decrease-selective-display)
   (defun toggle-selective-display ()
     "Hide lines starting with a lot of spaces.

See `increase-selective-display' to increase the number of spaces.
See `decrease-selective-display' to decrease it."
     (interactive)
     (set-selective-display (unless selective-display depth)))
   (cl-flet ((g (offset)
                (setq depth (+ depth offset))
                (set-selective-display depth)))
     (defun increase-selective-display ()
       "Increase the cap for `toogle-selective-display'.

See `toggle-selective-display' and `decrease-selective-display'."
       (interactive) (when (< depth 20) (g 1)))
     (defun decrease-selective-display ()
       "Decrease the cap for `toogle-selective-display'.

See `toggle-selective-display' and `increase-selective-display'."
       (interactive) (when (> depth 1) (g -1))))))

(with-message
 "Configuring parenthesis settings"
 (require 'paren)
 (electric-pair-mode t)
 (setq-default electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 (set-face-background 'show-paren-match "turquoise")
 (setq-default show-paren-delay 0))

(with-message
 "Setting up unicode"
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist '(font . "DejaVu Sans Bold 11"))
 (global-prettify-symbols-mode 1))

(with-message
 "Switch parens with brackets"
 (define-key key-translation-map "(" "[")
 (define-key key-translation-map ")" "]")
 (define-key key-translation-map "[" "(")
 (define-key key-translation-map "]" ")"))

(with-message
 "Window switch bindings"
 (with-eval-after-load 'init
   (global-set-key (kbd "<f2> <f1>") 'swap-windows)
   (global-set-key (kbd "C-M-`") 'toggle-window-split))
 (defun swap-windows ()
   "If you have 2 windows, it swaps them."
   (interactive)
   (cond ((not (= (count-windows) 2))
	  (message "You need exactly 2 windows to do this."))
	 (t
	  (let* ((w1 (car (window-list)))
		 (w2 (cadr (window-list)))
		 (b1 (window-buffer w1))
		 (b2 (window-buffer w2))
		 (s1 (window-start w1))
		 (s2 (window-start w2)))
	    (set-window-buffer w1 b2)
	    (set-window-buffer w2 b1)
	    (set-window-start w1 s2)
	    (set-window-start w2 s1)))))

 (defun toggle-window-split ()
   "When there are two windows, convert horizontal to vertical and vice versa."
   (interactive)
   (cond ((not (= (count-windows) 2))
	  (message "You need exactly 2 windows to do this."))
	 (t
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
	      (when this-win-2nd (other-window 1))))))))

(with-message
 "A few general keybindings"
 (global-set-key (kbd "M-9") 'backward-sexp)
 (global-set-key (kbd "M-0") 'forward-sexp))

(with-message
 "Setting up the order for recenter-top-bottom"
 (setq recenter-positions '(top middle bottom)))

;; Mark dired-ignored
(require 'dired)
(set-face-attribute 'dired-ignored nil
		    :strike-through t
		    :foreground "green")

;; Zoom with the mouse
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Wrap long lines
(global-visual-line-mode 1)

(provide 'mookid-naked-emacs-config)
;;; mookid-naked-emacs-config.el ends here
