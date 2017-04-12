;;; intero-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "intero" "intero.el" (22762 46483 0 0))
;;; Generated autoloads from intero.el

(autoload 'intero-mode "intero" "\
Minor mode for Intero

\\{intero-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'intero-mode-whitelist "intero" "\
Run intero-mode when the current project is in `intero-whitelist'.

\(fn)" t nil)

(autoload 'intero-mode-blacklist "intero" "\
Run intero-mode unless the current project is in `intero-blacklist'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; intero-autoloads.el ends here
