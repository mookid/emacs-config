;;; init.el --- configuration file for emacs!

;;; Commentary:

;; My Emacs config, with simple options.

;;; Code:
;; The configuration directories
(defvar mookid-root-dir "~/.emacs.d"
  "The root directory of the configuration.")

(defvar mookid-lisp-dir (expand-file-name "lisp" mookid-root-dir)
  "This is where the non compiled Lisp files are.")

(defvar mookid-modules-dir (expand-file-name "modules" mookid-root-dir)
  "This is where the configuration of the Lisp files are.")
;; end

(add-to-list 'load-path mookid-lisp-dir)
(add-to-list 'load-path mookid-modules-dir)

(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'init-load "mookid-macros")

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

(init-load 'avy)
(init-load 'flycheck)

;; ocaml settings
(init-load 'tuareg)
(init-load 'ocp-indent)
;; (init-load 'merlin)
;; end

(init-load 'c)

(let ((f (expand-file-name "private.el" mookid-root-dir)))
  (when (file-exists-p f)
    (with-message "Loading private settings" (load f))))

(provide 'init)
;;; init.el ends here
