;;; pager-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pager" "pager.el" (23154 29898 0 0))
;;; Generated autoloads from pager.el

(autoload 'pager-page-down "pager" "\
Like scroll-up, but moves a fixed amount of lines (fixed relative the
`window-height') so that pager-page-up moves back to the same line.

\(fn)" t nil)

(autoload 'pager-page-up "pager" "\
Like scroll-down, but moves a fixed amount of lines (fixed relative the
`window-height') so that pager-page-down moves back to the same line.

\(fn)" t nil)

(autoload 'pager-row-up "pager" "\
Move point to previous line while scrolling screen down one line.
The effect is that the cursor stays in the same position on the screen.

\(fn)" t nil)

(autoload 'pager-row-down "pager" "\
Move point to next line while scrolling screen up one line.
The effect is that the cursor stays in the same position on the screen.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pager-pkg.el") (23154 29897 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pager-autoloads.el ends here
