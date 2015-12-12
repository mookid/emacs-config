;; -*- lexical-binding: t -*-
;;; mookid-naked-emacs-config.el --- Configuration of naked emacs

;;; Commentary:
;; Configuration of emacs that does not depend on external packages.

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'ignore-all "mookid-macros")

;; Set color theme
(load-theme 'leuven)

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

;; Auto revert
(global-auto-revert-mode 1)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
(defun adjust-columns ()
  "Adjust the window, so that the width is 80 characters."
  (interactive)
  (adjust-window-trailing-edge
   (selected-window)
   (- 80 (window-width)) t))
(global-set-key (kbd "C-M-=") 'adjust-columns)
(global-set-key (kbd "C-+") 'balance-windows)

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
 (scroll-bar-mode 1)
 (blink-cursor-mode -1))

(with-message
 "Compilation settings"
 (setq compilation-ask-about-save nil)
 (setq-default compilation-always-kill t)
 (setq-default compilation-scroll-output 'first-error)
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
 "Loading packages list"
 (require 'package)
 (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
 (package-initialize))

(with-message
 "Configuring parenthesis settings"
 (require 'paren)
 (electric-pair-mode t)
 (setq-default electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 ;; (set-face-background 'show-paren-match "deep pink")
 (setq-default show-paren-delay 0))

(with-message
 "Setting up unicode"
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
 (global-prettify-symbols-mode 1))

(ignore-all
 "Loading hl line mode"
 (global-hl-line-mode)
 (set-face-attribute 'hl-line nil :background "RoyalBlue3"))
(provide 'mookid-naked-emacs-config)
;;; mookid-naked-emacs-config.el ends here
