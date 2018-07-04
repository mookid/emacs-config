;;; which-key-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "which-key" "which-key.el" (0 0 0 0))
;;; Generated autoloads from which-key.el

(defvar which-key-mode nil "\
Non-nil if Which-Key mode is enabled.\nSee the `which-key-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `which-key-mode'.")

(custom-autoload 'which-key-mode "which-key" nil)

(autoload 'which-key-mode "which-key" "\
Toggle which-key-mode.\n\n(fn &optional ARG)" t nil)

(autoload 'which-key-setup-side-window-right "which-key" "\
Apply suggested settings for side-window that opens on right.\n\n(fn)" t nil)

(autoload 'which-key-setup-side-window-right-bottom "which-key" "\
Apply suggested settings for side-window that opens on right\nif there is space and the bottom otherwise.\n\n(fn)" t nil)

(autoload 'which-key-setup-side-window-bottom "which-key" "\
Apply suggested settings for side-window that opens on\nbottom.\n\n(fn)" t nil)

(autoload 'which-key-setup-minibuffer "which-key" "\
Apply suggested settings for minibuffer.\nDo not use this setup if you use the paging commands. Instead use\n`which-key-setup-side-window-bottom', which is nearly identical\nbut more functional.\n\n(fn)" t nil)

(autoload 'which-key-add-key-based-replacements "which-key" "\
Replace the description of KEY-SEQUENCE with REPLACEMENT.\nKEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT\nmay either be a string, as in\n\n(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")\n\na cons of two strings as in\n\n(which-key-add-key-based-replacements \"C-x 8\"\n                                        '(\"unicode\" . \"Unicode keys\"))\n\nor a function that takes a (KEY . BINDING) cons and returns a\nreplacement.\n\nIn the second case, the second string is used to provide a longer\nname for the keys under a prefix.\n\nMORE allows you to specifcy additional KEY REPLACEMENT pairs.  All\nreplacements are added to\n`which-key-key-based-description-replacement-alist'.\n\n(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-add-major-mode-key-based-replacements "which-key" "\
Functions like `which-key-add-key-based-replacements'.\nThe difference is that MODE specifies the `major-mode' that must\nbe active for KEY-SEQUENCE and REPLACEMENT (MORE contains\naddition KEY-SEQUENCE REPLACEMENT pairs) to apply.\n\n(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-reload-key-sequence "which-key" "\
Simulate entering the key sequence KEY-SEQ.\nKEY-SEQ should be a list of events as produced by\n`listify-key-sequence'. If nil, KEY-SEQ defaults to\n`which-key--current-key-list'. Any prefix arguments that were\nused are reapplied to the new key sequence.\n\n(fn &optional KEY-SEQ)" nil nil)

(autoload 'which-key-show-standard-help "which-key" "\
Call the command in `which-key--prefix-help-cmd-backup'.\nUsually this is `describe-prefix-bindings'.\n\n(fn &optional _)" t nil)

(autoload 'which-key-show-next-page-no-cycle "which-key" "\
Show next page of keys unless on the last page, in which case\ncall `which-key-show-standard-help'.\n\n(fn)" t nil)

(autoload 'which-key-show-previous-page-no-cycle "which-key" "\
Show previous page of keys unless on the first page, in which\ncase do nothing.\n\n(fn)" t nil)

(autoload 'which-key-show-next-page-cycle "which-key" "\
Show the next page of keys, cycling from end to beginning\nafter last page.\n\n(fn &optional _)" t nil)

(autoload 'which-key-show-previous-page-cycle "which-key" "\
Show the previous page of keys, cycling from beginning to end\nafter first page.\n\n(fn &optional _)" t nil)

(autoload 'which-key-show-top-level "which-key" "\
Show top-level bindings.\n\n(fn &optional _)" t nil)

(autoload 'which-key-show-major-mode "which-key" "\
Show top-level bindings in the map of the current major mode.\n\nThis function will also detect evil bindings made using\n`evil-define-key' in this map. These bindings will depend on the\ncurrent evil state. \n\n(fn)" t nil)

(autoload 'which-key-undo-key "which-key" "\
Undo last keypress and force which-key update.\n\n(fn &optional _)" t nil)

(autoload 'which-key-C-h-dispatch "which-key" "\
Dispatch C-h commands by looking up key in\n`which-key-C-h-map'. This command is always accessible (from any\nprefix) if `which-key-use-C-h-commands' is non nil.\n\n(fn)" t nil)

(autoload 'which-key-show-keymap "which-key" "\
Show the top-level bindings in KEYMAP using which-key. KEYMAP\nis selected interactively from all available keymaps.\n\n(fn KEYMAP)" t nil)

(autoload 'which-key-show-full-keymap "which-key" "\
Show all bindings in KEYMAP using which-key. KEYMAP is\nselected interactively from all available keymaps.\n\n(fn KEYMAP)" t nil)

(autoload 'which-key-show-minor-mode-keymap "which-key" "\
Show the top-level bindings in KEYMAP using which-key. KEYMAP\nis selected interactively by mode in `minor-mode-map-alist'.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key" '("which-key-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; which-key-autoloads.el ends here
