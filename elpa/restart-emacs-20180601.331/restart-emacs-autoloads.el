;;; restart-emacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "restart-emacs" "restart-emacs.el" (0 0 0 0))
;;; Generated autoloads from restart-emacs.el

(autoload 'restart-emacs-handle-command-line-args "restart-emacs" "\
Handle the --restart-emacs-desktop command line argument.\n\nThe value of the argument is the desktop file from which the frames should be\nrestored.  IGNORED are ignored.\n\n(fn &rest IGNORED)" nil nil)

(add-to-list 'command-switch-alist '("--restart-emacs-desktop" . restart-emacs-handle-command-line-args))

(autoload 'restart-emacs "restart-emacs" "\
Restart Emacs.\n\nWhen called interactively ARGS is interpreted as follows\n\n- with a single `universal-argument' (`C-u') Emacs is restarted\n  with `--debug-init' flag\n- with two `universal-argument' (`C-u') Emacs is restarted with\n  `-Q' flag\n- with three `universal-argument' (`C-u') the user prompted for\n  the arguments\n\nWhen called non-interactively ARGS should be a list of arguments\nwith which Emacs should be restarted.\n\n(fn &optional ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "restart-emacs" '("restart-emacs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; restart-emacs-autoloads.el ends here
