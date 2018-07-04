;;; yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yasnippet" "yasnippet.el" (0 0 0 0))
;;; Generated autoloads from yasnippet.el

(autoload 'yas-minor-mode "yasnippet" "\
Toggle YASnippet mode.\n\nWhen YASnippet mode is enabled, `yas-expand', normally bound to\nthe TAB key, expands snippets of code depending on the major\nmode.\n\nWith no argument, this command toggles the mode.\npositive prefix argument turns on the mode.\nNegative prefix argument turns off the mode.\n\nKey bindings:\n\\{yas-minor-mode-map}\n\n(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.\nSee the `yas-global-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet" nil)

(autoload 'yas-global-mode "yasnippet" "\
Toggle Yas minor mode in all buffers.\nWith prefix ARG, enable Yas-Global mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nYas minor mode is enabled in all buffers where\n`yas-minor-mode-on' would do it.\nSee `yas-minor-mode' for more information on Yas minor mode.\n\n(fn &optional ARG)" t nil)
(autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippet" '("yas" "help-snippet-def" "snippet-mode-map")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yasnippet-autoloads.el ends here
