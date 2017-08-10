;;; centimacro-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "centimacro" "centimacro.el" (22923 48069 0
;;;;;;  0))
;;; Generated autoloads from centimacro.el

(autoload 'centi-assign "centimacro" "\
Read a KEY and start recording a macro for it.
Pressing KEY again stops recording and assigns the macro to KEY.
Aborts if KEY belongs to a minor mode.
Use `centi-summary' to list bound macros.
Use `centi-restore-all' to un-bind macros and restore the old key bindings.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; centimacro-autoloads.el ends here
