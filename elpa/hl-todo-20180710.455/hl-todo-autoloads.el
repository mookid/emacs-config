;;; hl-todo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hl-todo" "hl-todo.el" (0 0 0 0))
;;; Generated autoloads from hl-todo.el

(autoload 'hl-todo-mode "hl-todo" "\
Highlight TODO and similar keywords in comments and strings.\n\n(fn &optional ARG)" t nil)

(defvar global-hl-todo-mode nil "\
Non-nil if Global Hl-Todo mode is enabled.\nSee the `global-hl-todo-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-hl-todo-mode'.")

(custom-autoload 'global-hl-todo-mode "hl-todo" nil)

(autoload 'global-hl-todo-mode "hl-todo" "\
Toggle Hl-Todo mode in all buffers.\nWith prefix ARG, enable Global Hl-Todo mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nHl-Todo mode is enabled in all buffers where\n`hl-todo--turn-on-mode-if-desired' would do it.\nSee `hl-todo-mode' for more information on Hl-Todo mode.\n\n(fn &optional ARG)" t nil)

(autoload 'hl-todo-next "hl-todo" "\
Jump to the next TODO or similar keyword.\nThe prefix argument ARG specifies how many keywords to move.\nA negative argument means move backward that many keywords.\n\n(fn ARG)" t nil)

(autoload 'hl-todo-previous "hl-todo" "\
Jump to the previous TODO or similar keyword.\nThe prefix argument ARG specifies how many keywords to move.\nA negative argument means move forward that many keywords.\n\n(fn ARG)" t nil)

(autoload 'hl-todo-occur "hl-todo" "\
Use `occur' to find all TODO or similar keywords.\nThis actually finds a superset of the highlighted keywords,\nbecause it uses a regexp instead of a more sophisticated\nmatcher.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hl-todo" '("hl-todo-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hl-todo-autoloads.el ends here
