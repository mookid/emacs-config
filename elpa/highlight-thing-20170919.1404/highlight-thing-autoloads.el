;;; highlight-thing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-thing" "highlight-thing.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from highlight-thing.el

(autoload 'highlight-thing-mode "highlight-thing" "\
Minor mode that highlights things at point\n\n(fn &optional ARG)" t nil)

(defvar global-highlight-thing-mode nil "\
Non-nil if Global Highlight-Thing mode is enabled.\nSee the `global-highlight-thing-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-highlight-thing-mode'.")

(custom-autoload 'global-highlight-thing-mode "highlight-thing" nil)

(autoload 'global-highlight-thing-mode "highlight-thing" "\
Toggle Highlight-Thing mode in all buffers.\nWith prefix ARG, enable Global Highlight-Thing mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nHighlight-Thing mode is enabled in all buffers where\n`highlight-thing-mode-maybe-activate' would do it.\nSee `highlight-thing-mode' for more information on Highlight-Thing mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-thing" '("highlight-thing-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-thing-autoloads.el ends here
