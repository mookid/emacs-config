;;; vimish-fold-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vimish-fold" "vimish-fold.el" (0 0 0 0))
;;; Generated autoloads from vimish-fold.el

(autoload 'vimish-fold "vimish-fold" "\
Fold active region staring at BEG, ending at END.\n\n(fn BEG END)" t nil)

(autoload 'vimish-fold-unfold "vimish-fold" "\
Delete all `vimish-fold--folded' overlays at point.\n\n(fn)" t nil)

(autoload 'vimish-fold-refold "vimish-fold" "\
Refold unfolded fold at point.\n\n(fn)" t nil)

(autoload 'vimish-fold-delete "vimish-fold" "\
Delete fold at point.\n\n(fn)" t nil)

(autoload 'vimish-fold-unfold-all "vimish-fold" "\
Unfold all folds in current buffer.\n\n(fn)" t nil)

(autoload 'vimish-fold-refold-all "vimish-fold" "\
Refold all closed folds in current buffer.\n\n(fn)" t nil)

(autoload 'vimish-fold-delete-all "vimish-fold" "\
Delete all folds in current buffer.\n\n(fn)" t nil)

(autoload 'vimish-fold-toggle "vimish-fold" "\
Toggle fold at point.\n\n(fn)" t nil)

(autoload 'vimish-fold-toggle-all "vimish-fold" "\
Toggle all folds in current buffer.\n\n(fn)" t nil)

(autoload 'vimish-fold-avy "vimish-fold" "\
Fold region of text between point and line selected with avy.\n\nThis feature needs `avy' package.\n\n(fn)" t nil)

(autoload 'vimish-fold-next-fold "vimish-fold" "\
Jump to next folded region in current buffer.\n\n(fn)" t nil)

(autoload 'vimish-fold-previous-fold "vimish-fold" "\
Jump to previous folded region in current buffer.\n\n(fn)" t nil)

(autoload 'vimish-fold-mode "vimish-fold" "\
Toggle `vimish-fold-mode' minor mode.\n\nWith a prefix argument ARG, enable `vimish-fold-mode' mode if ARG\nis positive, and disable it otherwise.  If called from Lisp,\nenable the mode if ARG is omitted or NIL, and toggle it if ARG is\n`toggle'.\n\nThis minor mode sets hooks so when you `find-file' it calls\n`vimish-fold--restore-folds' and when you kill a file it calls\n`vimish-fold--save-folds'.\n\nFor globalized version of this mode see `vimish-gold-global-mode'.\n\n(fn &optional ARG)" t nil)

(defvar vimish-fold-global-mode nil "\
Non-nil if Vimish-Fold-Global mode is enabled.\nSee the `vimish-fold-global-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `vimish-fold-global-mode'.")

(custom-autoload 'vimish-fold-global-mode "vimish-fold" nil)

(autoload 'vimish-fold-global-mode "vimish-fold" "\
Toggle Vimish-Fold mode in all buffers.\nWith prefix ARG, enable Vimish-Fold-Global mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nVimish-Fold mode is enabled in all buffers where\n`vimish-fold-mode' would do it.\nSee `vimish-fold-mode' for more information on Vimish-Fold mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vimish-fold" '("vimish-fold-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vimish-fold-autoloads.el ends here
