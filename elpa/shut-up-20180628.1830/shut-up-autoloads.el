;;; shut-up-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shut-up" "shut-up.el" (0 0 0 0))
;;; Generated autoloads from shut-up.el

(autoload 'shut-up "shut-up" "\
Evaluate BODY with silenced output.\n\nWhile BODY is evaluated, all output is redirected to a buffer,\nunless `shut-up-ignore' is non-nil.  This affects:\n\n- `message'\n- All functions using `standard-output' (e.g. `print', `princ', etc.)\n\nInside BODY, the buffer is bound to the lexical variable\n`shut-up-sink'.  Additionally provide a lexical function\n`shut-up-current-output', which returns the current contents of\n`shut-up-sink' when called with no arguments.\n\nChanges to the variable `shut-up-ignore' inside BODY does not\nhave any affect.\n\n(fn &rest BODY)" nil t)

(function-put 'shut-up 'lisp-indent-function '0)

(autoload 'shut-up-silence-emacs "shut-up" "\
Silence Emacs.\n\nChange Emacs settings to reduce the output.\n\nWARNING: This function has GLOBAL SIDE-EFFECTS.  You should only\ncall this function in `noninteractive' sessions.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shut-up" '("shut-up-" "inhibit-message")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shut-up-autoloads.el ends here
