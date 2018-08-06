;;; diminish-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "diminish" "diminish.el" (0 0 0 0))
;;; Generated autoloads from diminish.el

(autoload 'diminish "diminish" "\
Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").\n\nInteractively, enter (with completion) the name of any minor mode, followed\non the next line by what you want it diminished to (default empty string).\nThe response to neither prompt should be quoted.  However, in Lisp code,\nboth args must be quoted, the first as a symbol, the second as a string,\nas in (diminish 'jiggle-mode \" Jgl\").\n\nThe mode-line displays of minor modes usually begin with a space, so\nthe modes' names appear as separate words on the mode line.  However, if\nyou're having problems with a cramped mode line, you may choose to use single\nletters for some modes, without leading spaces.  Capitalizing them works\nbest; if you then diminish some mode to \"X\" but have abbrev-mode enabled as\nwell, you'll get a display like \"AbbrevX\".  This function prepends a space\nto TO-WHAT if it's > 1 char long & doesn't already begin with a space.\n\n(fn MODE &optional TO-WHAT)" t nil)

(autoload 'diminish-undo "diminish" "\
Restore mode-line display of diminished mode MODE to its minor-mode value.\nDo nothing if the arg is a minor mode that hasn't been diminished.\n\nInteractively, enter (with completion) the name of any diminished mode (a\nmode that was formerly a minor mode on which you invoked \\[diminish]).\nTo restore all diminished modes to minor status, answer `diminished-modes'.\nThe response to the prompt shouldn't be quoted.  However, in Lisp code,\nthe arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).\n\n(fn MODE)" t nil)

(autoload 'diminished-modes "diminish" "\
Echo all active diminished or minor modes as if they were minor.\nThe display goes in the echo area; if it's too long even for that,\nyou can see the whole thing in the *Messages* buffer.\nThis doesn't change the status of any modes; it just lets you see\nwhat diminished modes would be on the mode-line if they were still minor.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "diminish" '("diminish")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; diminish-autoloads.el ends here
