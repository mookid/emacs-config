;;; rainbow-blocks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rainbow-blocks" "rainbow-blocks.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rainbow-blocks.el

(autoload 'rainbow-blocks-mode "rainbow-blocks" "\
Highlight nested parentheses, brackets, and braces according to their depth.\n\n(fn &optional ARG)" t nil)

(autoload 'rainbow-blocks-mode-enable "rainbow-blocks" "\
\n\n(fn)" nil nil)

(autoload 'rainbow-blocks-mode-disable "rainbow-blocks" "\
\n\n(fn)" nil nil)

(defvar global-rainbow-blocks-mode nil "\
Non-nil if Global Rainbow-Blocks mode is enabled.\nSee the `global-rainbow-blocks-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-rainbow-blocks-mode'.")

(custom-autoload 'global-rainbow-blocks-mode "rainbow-blocks" nil)

(autoload 'global-rainbow-blocks-mode "rainbow-blocks" "\
Toggle Rainbow-Blocks mode in all buffers.\nWith prefix ARG, enable Global Rainbow-Blocks mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nRainbow-Blocks mode is enabled in all buffers where\n`rainbow-blocks-mode-enable' would do it.\nSee `rainbow-blocks-mode' for more information on Rainbow-Blocks mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rainbow-blocks" '("rainbow-blocks-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rainbow-blocks-autoloads.el ends here
