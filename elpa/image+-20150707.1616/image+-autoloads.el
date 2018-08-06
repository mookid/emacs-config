;;; image+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "image+" "image+.el" (0 0 0 0))
;;; Generated autoloads from image+.el

(autoload 'imagex-sticky-mode "image+" "\
To manipulate Image at point.\n\n(fn &optional ARG)" t nil)

(defvar imagex-global-sticky-mode nil "\
Non-nil if Imagex-Global-Sticky mode is enabled.\nSee the `imagex-global-sticky-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `imagex-global-sticky-mode'.")

(custom-autoload 'imagex-global-sticky-mode "image+" nil)

(autoload 'imagex-global-sticky-mode "image+" "\
Toggle Imagex-Sticky mode in all buffers.\nWith prefix ARG, enable Imagex-Global-Sticky mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nImagex-Sticky mode is enabled in all buffers where\n`imagex-sticky-mode-maybe' would do it.\nSee `imagex-sticky-mode' for more information on Imagex-Sticky mode.\n\n(fn &optional ARG)" t nil)

(defvar imagex-auto-adjust-mode nil "\
Non-nil if Imagex-Auto-Adjust mode is enabled.\nSee the `imagex-auto-adjust-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `imagex-auto-adjust-mode'.")

(custom-autoload 'imagex-auto-adjust-mode "image+" nil)

(autoload 'imagex-auto-adjust-mode "image+" "\
Adjust image to current frame automatically in `image-mode'.\n\nType \\[imagex-sticky-restore-original] to restore the original image.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "image+" '("imagex-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; image+-autoloads.el ends here
