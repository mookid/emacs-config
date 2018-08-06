;;; anzu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "anzu" "anzu.el" (0 0 0 0))
;;; Generated autoloads from anzu.el

(autoload 'anzu-mode "anzu" "\
minor-mode which display search information in mode-line.\n\n(fn &optional ARG)" t nil)

(defvar global-anzu-mode nil "\
Non-nil if Global Anzu mode is enabled.\nSee the `global-anzu-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-anzu-mode'.")

(custom-autoload 'global-anzu-mode "anzu" nil)

(autoload 'global-anzu-mode "anzu" "\
Toggle Anzu mode in all buffers.\nWith prefix ARG, enable Global Anzu mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nAnzu mode is enabled in all buffers where\n`anzu--turn-on' would do it.\nSee `anzu-mode' for more information on Anzu mode.\n\n(fn &optional ARG)" t nil)

(autoload 'anzu-query-replace-at-cursor "anzu" "\
Replace symbol at cursor with to-string.\n\n(fn)" t nil)

(autoload 'anzu-query-replace-at-cursor-thing "anzu" "\
Replace symbol at cursor within `anzu-replace-at-cursor-thing' area.\n\n(fn)" t nil)

(autoload 'anzu-query-replace "anzu" "\
anzu version of `query-replace'.\n\n(fn ARG)" t nil)

(autoload 'anzu-query-replace-regexp "anzu" "\
anzu version of `query-replace-regexp'.\n\n(fn ARG)" t nil)

(autoload 'anzu-replace-at-cursor-thing "anzu" "\
anzu-query-replace-at-cursor-thing without query.\n\n(fn)" t nil)

(autoload 'anzu-isearch-query-replace "anzu" "\
anzu version of `isearch-query-replace'.\n\n(fn ARG)" t nil)

(autoload 'anzu-isearch-query-replace-regexp "anzu" "\
anzu version of `isearch-query-replace-regexp'.\n\n(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "anzu" '("anzu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anzu-autoloads.el ends here
