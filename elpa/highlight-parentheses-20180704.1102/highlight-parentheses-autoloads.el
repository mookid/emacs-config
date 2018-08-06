;;; highlight-parentheses-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-parentheses" "highlight-parentheses.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.\n\n(fn &optional ARG)" t nil)

(defvar global-highlight-parentheses-mode nil "\
Non-nil if Global Highlight-Parentheses mode is enabled.\nSee the `global-highlight-parentheses-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-highlight-parentheses-mode'.")

(custom-autoload 'global-highlight-parentheses-mode "highlight-parentheses" nil)

(autoload 'global-highlight-parentheses-mode "highlight-parentheses" "\
Toggle Highlight-Parentheses mode in all buffers.\nWith prefix ARG, enable Global Highlight-Parentheses mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nHighlight-Parentheses mode is enabled in all buffers where\n`(lambda nil (highlight-parentheses-mode 1))' would do it.\nSee `highlight-parentheses-mode' for more information on Highlight-Parentheses mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-parentheses" '("hl-paren-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-parentheses-autoloads.el ends here
