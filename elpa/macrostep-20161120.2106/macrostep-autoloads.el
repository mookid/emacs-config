;;; macrostep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "macrostep" "macrostep.el" (0 0 0 0))
;;; Generated autoloads from macrostep.el

(autoload 'macrostep-mode "macrostep" "\
Minor mode for inline expansion of macros in Emacs Lisp source buffers.\n\n\\<macrostep-keymap>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],\nand move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].\nUse \\[macrostep-collapse-all] or collapse all visible expansions to\nquit and return to normal editing.\n\n\\{macrostep-keymap}\n\n(fn &optional ARG)" t nil)

(autoload 'macrostep-expand "macrostep" "\
Expand the macro form following point by one step.\n\nEnters `macrostep-mode' if it is not already active, making the\nbuffer temporarily read-only. If macrostep-mode is active and the\nform following point is not a macro form, search forward in the\nbuffer and expand the next macro form found, if any.\n\nWith a prefix argument, the expansion is displayed in a separate\nbuffer instead of inline in the current buffer.  Setting\n`macrostep-expand-in-separate-buffer' to non-nil swaps these two\nbehaviors.\n\n(fn &optional TOGGLE-SEPARATE-BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macrostep" '("macrostep-")))

;;;***

;;;### (autoloads nil "macrostep-c" "macrostep-c.el" (0 0 0 0))
;;; Generated autoloads from macrostep-c.el

(autoload 'macrostep-c-mode-hook "macrostep-c" "\
\n\n(fn)" nil nil)

(add-hook 'c-mode-hook #'macrostep-c-mode-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macrostep-c" '("macrostep-c-")))

;;;***

;;;### (autoloads nil nil ("macrostep-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; macrostep-autoloads.el ends here
