;;; helpful-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helpful" "helpful.el" (0 0 0 0))
;;; Generated autoloads from helpful.el

(autoload 'helpful-function "helpful" "\
Show help for function named SYMBOL.\n\n(fn SYMBOL)" t nil)

(autoload 'helpful-command "helpful" "\
Show help for interactive function named SYMBOL.\n\n(fn SYMBOL)" t nil)

(autoload 'helpful-key "helpful" "\
Show help for interactive command bound to KEY-SEQUENCE.\n\n(fn KEY-SEQUENCE)" t nil)

(autoload 'helpful-macro "helpful" "\
Show help for macro named SYMBOL.\n\n(fn SYMBOL)" t nil)

(autoload 'helpful-callable "helpful" "\
Show help for function, macro or special form named SYMBOL.\n\nSee also `helpful-macro' and `helpful-function'.\n\n(fn SYMBOL)" t nil)

(autoload 'helpful-symbol "helpful" "\
Show help for SYMBOL, a variable, function or macro.\n\nSee also `helpful-callable' and `helpful-variable'.\n\n(fn SYMBOL)" t nil)

(autoload 'helpful-variable "helpful" "\
Show help for variable named SYMBOL.\n\n(fn SYMBOL)" t nil)

(autoload 'helpful-at-point "helpful" "\
Show help for the symbol at point.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helpful" '("helpful-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helpful-autoloads.el ends here
