;;; init.el --- configuration file for emacs!
;; -*- lexical-binding: t -*-

;;; Commentary:

;; My emacs config, with simple options.

;;; Code:
(package-initialize)

;; Common lisp functionalities
(with-no-warnings (require 'cl)) ; useless warning

(setq load-path (cons "~/.emacs.d/lisp/" load-path))
(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")

(setq load-path (cons "~/.emacs.d/modules/" load-path))

(with-title "Naked emacs configuration" (require 'mookid-naked-emacs-config))

;; evil related packages
(init-load (require 'mookid-evil))
(init-load (require 'mookid-evil-numbers))
(init-load (require 'mookid-evil-visualstar))
(init-load (require 'mookid-evil-jumper))
(init-load (require 'mookid-highlight-persist))
;; end

(init-load (require 'mookid-powerline))
(init-load (require 'mookid-rainbow))
(init-load (require 'mookid-company))

;; helm related packages
(init-load (require 'mookid-helm))
(init-load (require 'mookid-shackle))
(init-load (require 'mookid-helm-swoop))
(init-load (require 'mookid-helm-projectile))
;; end

(init-load (require 'mookid-smartparens))
(init-load (require 'mookid-avy))
(init-load (require 'mookid-flycheck))

;; ocaml settings
(init-load (require 'mookid-tuareg))
(init-load (require 'mookid-ocp-indent))
;; (init-load (require 'mookid-merlin))
;; end

(init-load (require 'mookid-c))
(init-load (require 'mookid-highlight-column))

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
;;; init.el ends here
