;;; mookid-init.el --- configuration file for emacs!

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'init-load "mookid-macros")

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
(init-load 'elisp-slime-nav)

;; ivy related packages
(init-load 'ivy)
(init-load 'find-file-in-project)
;; end

(init-load 'slime)
(init-load 'expand-region)

(init-load 'avy)
(init-load 'ace-window)
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

(provide 'mookid-init)
;;; mookid-init.el ends here
