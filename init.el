;; -*- lexical-binding: t -*-
;;; init.el --- configuration file for emacs!

;;; Commentary:
;; My emacs config, with simple options.

;;; Code:

;; Common lisp functionalities
(with-no-warnings (require 'cl)) ; useless warning

;;; Useful macros for loading packages
(defmacro with-message (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "*** %s" ,msg) ,@body 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))
(defmacro with-title (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "[%s]" ,msg) ,@body (message "[end]") 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))
(defmacro define-and-set (name value)
  "The same effect as (setq NAME VALUE), but prevents warnings."
  `(progn (defvar ,name) (setq ,name ,value)))
(defmacro ignore-all (&rest _) "Ignore arguments, which are not evaluated." nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Naked emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set color theme
(load-theme 'tango-dark)

;; Define an alias for dabbrev-expand
(global-set-key (kbd "<tab>") 'dabbrev-expand)

;; Easy switch from one window to another
(windmove-default-keybindings)

;; Fixed size mini-window
(define-and-set resize-mini-windows nil)

;; Switch to messages buffer at startup
(define-and-set inhibit-startup-message t)
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
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespaces when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

(with-message
 "Remove gui elements"
 (tooltip-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (scroll-bar-mode 1)
 (define-and-set blink-cursor-mode nil))

(defun save-all-and-recompile ()
  "Save any modified buffer and recompile."
  (interactive)
  (save-some-buffers 1)
  (recompile))
(global-set-key (kbd "<f12>") 'save-all-and-recompile)

;; Save history between sessions
(define-and-set savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq history-length t) ; no maximum
(setq history-delete-duplicates t)
(define-and-set savehist-save-minibuffer-history t)
(define-and-set savehist-additional-variables
  '(kill-ring search-ring regexp-search-ring))

(with-message
 "Setting up selective display."
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
       (interactive)
       (g 1))
     (defun decrease-selective-display ()
       "Decrease the cap for `toogle-selective-display'.

See `toggle-selective-display' and `increase-selective-display'."
       (interactive)
       (when (> depth 0) (g -1))))))

(with-message
 "Loading packages list"
 (require 'package)
 (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
 (package-initialize))

(with-message
 "Configuring parenthesis settings"
 (require 'paren)
 (electric-pair-mode t)
 (define-and-set electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 (set-face-background 'show-paren-match "deep pink")
 (define-and-set show-paren-delay 0))

(with-message
 "Setting up unicode"
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist
              '(font . "DejaVu Sans Mono-11"))
 (global-prettify-symbols-mode 1))

(ignore-all
 "Loading hl line mode"
 (global-hl-line-mode)
 (set-face-attribute 'hl-line nil :background "RoyalBlue3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package loading and settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-title
 "Evil related packages"

 (with-message
  "Loading evil mode"
  (require 'evil)
  (evil-mode)
  (defvar evil-emacs-state-map)
  (defvar evil-motion-state-map)
  (defvar evil-insert-state-map)
  (defvar evil-visual-state-map)
  (defvar evil-normal-state-map)
  (defvar evil-replace-state-map)
  (defvar evil-operator-state-map)
  (define-and-set evil-emacs-state-cursor '("purple" box))
  (define-and-set evil-normal-state-cursor '("grey" box))
  (define-and-set evil-visual-state-cursor '("green" box))
  (define-and-set evil-insert-state-cursor '("red" bar))
  (define-and-set evil-replace-state-cursor '("deep pink" box))
  (define-and-set evil-motion-state-cursor '("gray" box)))

 (with-message
  "Loading evil visualstar"
  (require 'evil-visualstar)
  (global-evil-visualstar-mode t))

 (with-message
  "Loading evil search highlight persist"
  (require 'evil-search-highlight-persist)
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :weight 'extra-bold
                              :foreground "blue"
                              :background "yellow1"))
        '(evil-search-highlight-persist-highlight-face
          isearch
          lazy-highlight))
  (setq evil-search-highlight-string-min-len 5)
  (global-evil-search-highlight-persist t))

 (with-message
  "Loading evil numbers"
  (require 'evil-numbers)
  (cl-loop
   for (key . val) in '((<f1> . evil-numbers/inc-at-pt)
                        (<f2> . evil-numbers/dec-at-pt))
   do (define-key evil-normal-state-map
        (kbd (concat "C-M-S-" (symbol-name key))) val))))

(with-message
 "Loading powerline"
 (require 'smart-mode-line-powerline-theme)
 (define-and-set sml/no-confirm-load-theme t)
 ;; avoids a question at every startup
 (define-and-set sml/theme 'powerline)
 (sml/setup))

(with-message
 "Loading rainbow delimiters and blocks"
 (require 'rainbow-delimiters)
 (require 'rainbow-blocks)
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
 (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                     :foreground "red"
                     :inherit 'error
                     :box t)
 (let ((colors '("green" "violet" "orange red"))
       (kinds '(delimiters blocks)))
   (cl-labels ((set-bold (face color)
                         (set-face-attribute face nil
                                             :weight 'extra-bold
                                             :foreground color))
	       (mk-symb (kind lvl)
			(intern (concat "rainbow-"
					(prin1-to-string kind) "-depth-"
					(prin1-to-string lvl) "-face")))
	       (set-level (lvl color)
                          (when (< 0 lvl 10)
                            (mapc (lambda (kind)
                                    (set-bold (mk-symb kind lvl) color))
				  kinds))))
     (cl-loop
      with ncolors = (length colors)
      for lvl from 1 upto 9
      for icolor = (mod (- lvl 1) ncolors)
      do (set-level lvl (nth icolor colors))))))

(with-message
 "Loading company mode"
 (require 'company)
 (define-and-set company-idle-delay 0.5)
 (define-and-set company-tooltip-limit 5)
 (define-and-set company-minimum-prefix-length 2)
 (define-and-set company-tooltip-flip-when-above t))

(ignore-all
 "Loading paredit mode"
 (require 'paredit)
 (paredit-mode))

(with-title
 "Helm related packages"

 (with-message
  "Loading helm"
  (require 'helm)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (helm-mode))

 (with-message
  "Loading helm swoop"
  (require 'helm-swoop)
  (define-key evil-motion-state-map (kbd "\\") 'helm-swoop-from-evil-search)
  (define-key isearch-mode-map (kbd "C-\\") 'helm-swoop-from-isearch)))

(with-message
 "Loading smartparens"
 (require 'smartparens-config)
 (show-smartparens-global-mode nil)
 (define-and-set sp-autoskip-closing-pair 'always)
 (define-and-set sp-hybrid-kill-entire-symbol nil)
 (sp-use-paredit-bindings)
 (define-key evil-insert-state-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
 (global-set-key (kbd "M-[") 'sp-backward-unwrap-sexp)
 (cl-loop
  for (key . val) in '((paren   . "(")
                       (bracket . "[")
                       (brace   . "{")
                       (squote  . "'")
                       (dquote  . "\""))
  for symb = (intern (concat "wrap-with-" (prin1-to-string key) "s"))
  for kbinding = (concat "C-c " val)
  do (eval
      `(defun ,symb (&optional arg)
         "Wrap the next form (or the selection) using `sp-wrap-with-pair'."
         (interactive "P")
         (sp-wrap-with-pair ,val)))
  do (eval `(global-set-key (kbd ,kbinding) ',symb))))

(with-message
 "Setting up flycheck"
 (require 'flycheck)
 (defvar flycheck-mode-map)
 (define-key flycheck-mode-map (kbd "C-S-<next>") 'flycheck-next-error))

(with-message
 "Setting up avy"
 (require 'avy)
 (setq avy-all-windows 'all-frames)
 (mapc (lambda (map) (eval `(define-key ,map (kbd "s") 'avy-goto-word-or-subword-1)))
       '(evil-motion-state-map
         evil-visual-state-map
         evil-normal-state-map)))

(ignore-all
 "Loading ace-isearch"
 (require 'ace-isearch)
 (global-ace-isearch-mode +1)
 (define-and-set ace-isearch-function 'avy-goto-char))

(when (memq window-system '(mac ns))
  (with-message
   "Setting up path"
   (require 'exec-path-from-shell)
   (exec-path-from-shell-initialize)))

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
;;; init.el ends here
