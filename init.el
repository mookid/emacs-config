;;; init.el --- configuration file for emacs!

;;; Commentary:
;; My emacs config, with simple options.

;;; Code:

;; Common lisp functionalities
(with-no-warnings (require 'cl)) ; useless warning

;;; Useful macros for loading packages
(defmacro with-message (msg &rest body)
  `(condition-case nil
       (progn (message (format "*** %s" ,msg)) ,@body 'ok)
     (error (message (format "Error during phase called \"%s\"" ,msg)) 'fail)))
(defmacro define-and-set (name value)
  `(progn (defvar ,name) (setq ,name ,value)))
(defmacro ignore-all (&rest _) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Naked emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set color theme
(load-theme 'tango-dark)

;; Move backup files to a subdirectory of ~/.emacs.d
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
(defun adjust-columns ()
  (interactive)
  (adjust-window-trailing-edge
   (selected-window)
   (- 80 (window-width)) t))
(global-set-key (kbd "C-M-=") 'adjust-columns)
(global-set-key (kbd "C-+") 'balance-windows)

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

(with-message
 "Remove gui elements"
 (tooltip-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (scroll-bar-mode 1)
 (define-and-set blink-cursor-mode nil))

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
 (define-and-set selective-display-indent 1)
 (defun toggle-selective-display ()
   (interactive)
   (set-selective-display (unless selective-display selective-display-indent)))
 (global-set-key (kbd "<f6>") 'toggle-selective-display)
 (defun change-selective-display (offset)
   (setq selective-display-indent (+ selective-display-indent offset))
   (set-selective-display selective-display-indent))
 (defun inc-selective-display () (interactive) (change-selective-display 1))
 (defun dec-selective-display () (interactive) (change-selective-display -1))
 (global-set-key (kbd "C-<f6>") 'inc-selective-display)
 (global-set-key (kbd "S-<f6>") 'dec-selective-display))

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
(with-message
 "Loading evil mode"
 (require 'evil)
 (evil-mode)
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
 "Loading evil numbers"
 (require 'evil-numbers)
 (defvar evil-normal-state-map)
 (define-key evil-normal-state-map (kbd "C-M-S-<f1>") 'evil-numbers/inc-at-pt)
 (define-key evil-normal-state-map (kbd "C-M-S-<f2>") 'evil-numbers/dec-at-pt))

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
 (let ((colors '("green" "violet" "orange red")))
   (cl-labels ((set-bold (face color)
			 (set-face-attribute face nil
					     :weight 'extra-bold
					     :foreground color))
	       (set-level (lvl color)
			  (when (< 0 lvl 10)
			    (mapc (lambda (kind)
				    (set-bold
				     (read (concat "rainbow-"
						   kind
						   "-depth-"
						   (prin1-to-string lvl)
						   "-face"))
				     color))
				  '("delimiters" "blocks")))))
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
 (define-and-set company-tooltip-flip-when-above t)
 (global-company-mode 1))

(ignore-all
 "Loading paredit mode"
 (require 'paredit)
 (paredit-mode))

(with-message
 "Loading helm"
 (require 'helm)
 (global-set-key (kbd "M-x") 'helm-M-x)
 (global-set-key (kbd "C-x C-m") 'helm-M-x)
 (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
 (helm-mode))

(with-message
 "Loading smartparens"
 (require 'smartparens-config)
 (show-smartparens-global-mode nil)
 (define-and-set sp-autoskip-closing-pair 'always)
 (define-and-set sp-hybrid-kill-entire-symbol nil)
 (sp-use-paredit-bindings)
 (global-set-key (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
 (global-set-key (kbd "M-[") 'sp-backward-unwrap-sexp)
 (cl-loop for (key . val) in '((paren   . "(")
			       (bracket . "[")
			       (brace   . "{")
			       (squote  . "'")
			       (dquote  . "\""))
	  for fname = (concat "wrap-with-" (prin1-to-string key) "s")
	  for kbinding = (concat "C-c " val)
	  do
	  ;; definition of wrap-with-( ...
	  (eval `(defun ,(read fname) ()
		   (interactive)
		   (sp-wrap-with-pair ,val)))
	  do
	  ;; binding to C-c (
	  (eval `(global-set-key (kbd ,kbinding)
				 ,(read (concat "'" fname))))))

(with-message
 "Setting up flycheck"
 (require 'flycheck)
 (global-set-key (kbd "C-S-<next>") 'flycheck-next-error))

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el"))
   (when (file-exists-p f)
     (load f))))

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
 (global-evil-search-highlight-persist t))

(provide 'init)
;;; init.el ends here
