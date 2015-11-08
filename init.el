;; Common lisp functionalities
(require 'cl)

;; A useful macro for loading packages
(defmacro with-message (msg &rest body)
  `(condition-case nil
       (progn (message (format "*** %s" ,msg)) ,@body )
  (error (message (format "Error during phase called \"%s\"" ,msg)))))

(with-message
 "Remove gui elements"
 (tooltip-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (scroll-bar-mode 1))

;; Move backup files to a subdirectory of ~/.emacs.d
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(with-message
 "Loading packages list"
 (require 'package)
 (add-to-list 'package-archives
	      '("melpa" . "http://melpa.org/packages/"))
 (package-initialize))

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save history between sessions
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq history-length t) ; no maximum
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

(with-message
 "Loading color theme"
 (load-theme 'tango-dark))

(with-message
 "Loading evil mode"
 (require 'evil)
 (require 'evil-visualstar)
 (evil-mode)
 (setq evil-emacs-state-cursor  '("purple" box))
 (setq evil-normal-state-cursor '("yellow" box))
 (setq evil-visual-state-cursor '("green" box))
 (setq evil-insert-state-cursor '("red" bar))
 (setq evil-replace-state-cursor '("pink" box))
 (setq evil-motion-state-cursor '("gray" box)))

(with-message
 "Loading evil numbers"
 (require 'evil-numbers)
 (define-key evil-normal-state-map (kbd "C-M-S-<f1>") 'evil-numbers/inc-at-pt)
 (define-key evil-normal-state-map (kbd "C-M-S-<f2>") 'evil-numbers/dec-at-pt))

(with-message
 "Loading powerline"
 (require 'smart-mode-line-powerline-theme)
 (setq sml/no-confirm-load-theme t) ; avoids a question at every startup
 (setq sml/theme 'powerline)
 (sml/setup))

(with-message
 "Configuring parenthesis settings"
 (require 'paren)
 (electric-pair-mode t)
 (setq electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 (set-face-background 'show-paren-match "deep pink")
 (setq show-paren-delay 0))

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
   (cl-flet ((set-bold (fname cname)
		       (set-face-attribute fname nil
					   :weight 'extra-bold
					   :foreground cname)))
     (mapc (lambda (face-name)
	     (set-bold face-name (nth 0 colors)))
	   '(rainbow-delimiters-depth-1-face
	     rainbow-delimiters-depth-4-face
	     rainbow-delimiters-depth-7-face
	     rainbow-blocks-depth-1-face
	     rainbow-blocks-depth-4-face
	     rainbow-blocks-depth-7-face))
     (mapc (lambda (face-name)
	     (set-bold face-name (nth 1 colors)))
	   '(rainbow-delimiters-depth-2-face
	     rainbow-delimiters-depth-5-face
	     rainbow-delimiters-depth-8-face
	     rainbow-blocks-depth-2-face
	     rainbow-blocks-depth-5-face
	     rainbow-blocks-depth-8-face))
     (mapc (lambda (face-name)
	     (set-bold face-name (nth 2 colors)))
	   '(rainbow-delimiters-depth-3-face
	     rainbow-delimiters-depth-6-face
	     rainbow-delimiters-depth-9-face
	     rainbow-blocks-depth-3-face
	     rainbow-blocks-depth-6-face
	     rainbow-blocks-depth-9-face)))))

(with-message
 "Loading company mode"
 (require 'company)
 (setq company-idle-delay 0.5)
 (setq company-tooltip-limit 5)
 (setq company-minimum-prefix-length 2)
 (setq company-tooltip-flip-when-above t)
 (global-company-mode 1))

(with-message
 "Loading paredit mode"
 (require 'paredit)
 (paredit-mode))
