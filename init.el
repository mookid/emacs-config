;;; init.el --- configuration file for emacs!
;; -*- lexical-binding: t -*-

;;; Commentary:

;; My emacs config, with simple options.

;;; Code:
;;(package-initialize)

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

(with-message
 "Loading packages list"
 (require 'package)
 (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
 (package-initialize))

(with-title "Naked emacs configuration" (require 'mookid-naked-emacs-config))

;; evil related packages
(init-load 'evil)
(init-load 'evil-numbers)
(init-load 'evil-visualstar)
(init-load 'evil-jumper)
(init-load 'highlight-persist)
;; end

(init-load 'rainbow)
(init-load 'company)

;; ivy related packages
(init-load 'ivy)
(init-load 'find-file-in-project)
;; end

(init-load 'slime)
(init-load 'expand-region)

;; helm related packages
;; (init-load 'helm)
;; (init-load 'shackle)
;; (init-load 'helm-swoop)
;; (init-load 'helm-projectile)
;; end

(init-load 'smartparens)
(init-load 'avy)
(init-load 'flycheck)

;; ocaml settings
(init-load 'tuareg)
(init-load 'ocp-indent)
;; (init-load 'merlin)
;; end

(init-load 'c)
;; (init-load 'highlight-column)

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tuareg smartparens slime rainbow-delimiters rainbow-blocks ocp-indent flycheck find-file-in-project expand-region evil-visualstar evil-search-highlight-persist evil-numbers evil-jumper counsel company avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
