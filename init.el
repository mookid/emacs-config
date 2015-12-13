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
(autoload 'ignore-all "mookid-macros")

(with-title "Naked emacs configuration" (require 'mookid-naked-emacs-config))
(with-title "Evil related packages" (require 'mookid-evil))
(with-message "Loading powerline" (require 'mookid-powerline))
(with-message "Loading rainbow parameters" (require 'mookid-rainbow))
(with-message "Loading company mode" (require 'mookid-company))

;; helm related packages
(with-message "Loading helm" (require 'mookid-helm))
(with-message "Loading shackle" (require 'mookid-shackle))
(with-message "Loading helm swoop" (require 'mookid-helm-swoop))
(with-message "Loading helm projectile" (require 'mookid-helm-projectile))
;; end

(with-message "Loading smartparens" (require 'mookid-smartparens))
(with-message "Setting up avy" (require 'mookid-avy))
(with-message "Setting up flycheck" (require 'mookid-flycheck))

;; ocaml settings
(with-message "Configuring tuareg mode" (require 'mookid-tuareg))
(with-message "Configuring ocp-indent" (require 'mookid-ocp-indent))
;; (with-message "Configuring merlin" (require 'mookid-merlin))
;; end

(with-message "C settings" (require 'mookid-c))
(with-message "Configuring highlight column" (require 'mookid-highlight-column))

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
;;; init.el ends here
