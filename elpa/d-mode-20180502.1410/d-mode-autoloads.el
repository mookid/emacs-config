;;; d-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "d-mode" "d-mode.el" (0 0 0 0))
;;; Generated autoloads from d-mode.el

(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(defvar d-mode-hook nil "\
*Hook called by `d-mode'.")

(custom-autoload 'd-mode-hook "d-mode" t)

(autoload 'd-mode "d-mode" "\
Major mode for editing code written in the D Programming Language.\n\nSee http://dlang.org for more information about the D language.\n\nThe hook `c-mode-common-hook' is run with no args at mode\ninitialization, then `d-mode-hook'.\n\nKey bindings:\n\\{d-mode-map}\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "d-mode" '("doxygen-font-lock-" "d-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; d-mode-autoloads.el ends here
