;;; company-emacs-eclim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-emacs-eclim" "company-emacs-eclim.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-emacs-eclim.el

(autoload 'company-emacs-eclim-setup "company-emacs-eclim" "\
Convenience function that adds company-emacs-eclim to the list\n  of available company backends.\n\n(fn)" nil nil)

(autoload 'company-emacs-eclim "company-emacs-eclim" "\
`company-mode' back-end for Eclim completion\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-emacs-eclim" '("company-emacs-eclim-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-emacs-eclim-autoloads.el ends here
