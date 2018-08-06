;;; composable-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "composable" "composable.el" (0 0 0 0))
;;; Generated autoloads from composable.el

(defvar composable-mode nil "\
Non-nil if Composable mode is enabled.\nSee the `composable-mode' command\nfor a description of this minor mode.")

(custom-autoload 'composable-mode "composable" nil)

(autoload 'composable-mode "composable" "\
Toggle Composable mode.\n\n(fn &optional ARG)" t nil)

(defvar composable-mark-mode nil "\
Non-nil if Composable-Mark mode is enabled.\nSee the `composable-mark-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `composable-mark-mode'.")

(custom-autoload 'composable-mark-mode "composable" nil)

(autoload 'composable-mark-mode "composable" "\
Toggle composable mark mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "composable" '("composable-")))

;;;***

;;;### (autoloads nil "composable-mark" "composable-mark.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from composable-mark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "composable-mark" '("composable-")))

;;;***

;;;### (autoloads nil nil ("composable-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; composable-autoloads.el ends here
