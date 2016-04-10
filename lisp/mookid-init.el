;;; mookid-init.el --- configuration file for emacs!

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'mookid-load "mookid-macros")

(with-title "Naked emacs configuration"
            (mookid-load 'mookid-naked-emacs-config)
            (mookid-load 'mookid-colors)
            (mookid-load 'mookid-dired)
            (mookid-load 'mookid-compile)
            (mookid-load 'mookid-mouse)
            (mookid-load 'mookid-selective-display)
            (mookid-load 'mookid-isearch))

(mookid-load 'mookid-evil-nerd-commenter)
(mookid-load 'mookid-anzu)
(mookid-load 'mookid-lispy)
(mookid-load 'mookid-magit)
(mookid-load 'mookid-rainbow)
(mookid-load 'mookid-company)
(mookid-load 'mookid-elisp-slime-nav)

(with-title "Ivy configuration"
	    (mookid-load 'mookid-ivy)
	    (mookid-load 'mookid-projectile))

(mookid-load 'mookid-smooth-scrolling)
(mookid-load 'mookid-slime)
(mookid-load 'mookid-expand-region)

(mookid-load 'mookid-avy)
(mookid-load 'mookid-ace-window)
(mookid-load 'mookid-flycheck)

(with-title "OCaml configuration"
	    (mookid-load 'mookid-tuareg)
	    ;; (mookid-load 'mookid-ocp-indent)
	    ;; (mookid-load 'mookid-merlin)
	    )

(with-title "C configuration"
	    (mookid-load 'mookid-c)
	    (mookid-load 'mookid-clang-format))

(let ((f (expand-file-name "private.el" mookid-root-dir)))
  (when (file-exists-p f)
    (with-message "Loading private settings" (load f))))

(provide 'mookid-init)
;;; mookid-init.el ends here
