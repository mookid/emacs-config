;;; init.el --- configuration file for emacs!
;; -*- lexical-binding: t -*-

;;; Commentary:

;; My emacs config, with simple options.

;;; Code:
(package-initialize)

;; Common lisp functionalities
(require 'cl-lib)

(defvar mookid-lisp-dir "~/.emacs.d/lisp"
  "This is where are the non compiled lisp files.")
(defvar mookid-modules-dir "~/.emacs.d/modules"
  "This is where are the compiled lisp files.")

(add-to-list 'load-path mookid-lisp-dir)
(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'init-load "mookid-macros")

(add-to-list 'load-path mookid-modules-dir)

(with-title "Naked emacs configuration" (require 'mookid-naked-emacs-config))

;; evil related packages
(init-load 'mookid-evil)
(init-load 'mookid-evil-numbers)
(init-load 'mookid-evil-visualstar)
(init-load 'mookid-evil-jumper)
(init-load 'mookid-highlight-persist)
;; end

(init-load 'mookid-powerline)
(init-load 'mookid-rainbow)
(init-load 'mookid-company)

;; ivy related packages
(init-load 'mookid-ivy)
(init-load 'mookid-find-file-in-project)
;; end

;; helm related packages
;;(init-load 'mookid-helm)
(init-load 'mookid-shackle)
(init-load 'mookid-helm-swoop)
(init-load 'mookid-helm-projectile)
;; end

(init-load 'mookid-smartparens)
(init-load 'mookid-avy)
(init-load 'mookid-flycheck)

;; ocaml settings
(init-load 'mookid-tuareg)
(init-load 'mookid-ocp-indent)
;; (init-load 'mookid-merlin)
;; end

(init-load 'mookid-c)
(init-load 'mookid-highlight-column)

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
;;; init.el ends here
