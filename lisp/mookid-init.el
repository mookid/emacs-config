;;; mookid-init.el --- configuration file for emacs!

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'init-load "mookid-macros")

(with-title "Naked emacs configuration"
            (init-load 'mookid-naked-emacs-config)
            (init-load 'mookid-colors)
            (init-load 'mookid-compile)
            (init-load 'mookid-mouse)
            (init-load 'mookid-selective-display)
            (init-load 'mookid-isearch))

(init-load 'mookid-evil-nerd-commenter)
(init-load 'mookid-anzu)
(init-load 'mookid-lispy)
(init-load 'mookid-magit)
(init-load 'mookid-rainbow)
(init-load 'mookid-company)
(init-load 'mookid-elisp-slime-nav)

(with-title "Ivy configuration"
	    (init-load 'mookid-ivy)
	    (init-load 'mookid-projectile))

(init-load 'mookid-smooth-scrolling)
(init-load 'mookid-slime)
(init-load 'mookid-expand-region)

(init-load 'mookid-avy)
(init-load 'mookid-ace-window)
(init-load 'mookid-flycheck)

(with-title "OCaml configuration"
	    (init-load 'mookid-tuareg)
	    ;; (init-load 'mookid-ocp-indent)
	    ;; (init-load 'mookid-merlin)
	    )

(with-title "C configuration"
	    (init-load 'mookid-c)
	    (init-load 'mookid-clang-format))

(let ((f (expand-file-name "private.el" mookid-root-dir)))
  (when (file-exists-p f)
    (with-message "Loading private settings" (load f))))

(provide 'mookid-init)
;;; mookid-init.el ends here
