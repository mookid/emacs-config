;;; evil-nerd-commenter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-nerd-commenter" "evil-nerd-commenter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-nerd-commenter.el

(autoload 'evilnc-comment-or-uncomment-paragraphs "evil-nerd-commenter" "\
Comment or uncomment NUM paragraph(s).\nA paragraph is a continuation non-empty lines.\nParagraphs are separated by empty lines.\n\n(fn &optional NUM)" t nil)

(autoload 'evilnc-comment-or-uncomment-to-the-line "evil-nerd-commenter" "\
Comment or uncomment from current line to LINENUM line.\n\n(fn &optional LINENUM)" t nil)

(autoload 'evilnc-quick-comment-or-uncomment-to-the-line "evil-nerd-commenter" "\
Comment/uncomment to line number by last digit(s) whose value is UNITS.\nFor exmaple, you can use either \\<M-53>\\[evilnc-quick-comment-or-uncomment-to-the-line] or \\<M-3>\\[evilnc-quick-comment-or-uncomment-to-the-line] to comment to the line 6453\n\n(fn &optional UNITS)" t nil)

(autoload 'evilnc-toggle-invert-comment-line-by-line "evil-nerd-commenter" "\
Please note this command may NOT work on complex evil text objects.\n\n(fn)" t nil)

(autoload 'evilnc-toggle-comment-empty-lines "evil-nerd-commenter" "\
Toggle the flag which decide wether empty line will be commented.\n\n(fn)" t nil)

(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter" "\
Comment or uncomment NUM lines.  NUM could be negative.\n\nCase 1: If no region selected, comment/uncomment on current line.\nIf NUM>1, comment/uncomment extra N-1 lines from next line.\n\nCase 2: Selected region is expanded to make it contain whole lines.\nThen we comment/uncomment the expanded region.  NUM is ignored.\n\nCase 3: If a region inside of ONE line is selected,\nwe comment/uncomment that region.\nCORRECT comment syntax will be used for C++/Java/Javascript.\n\n(fn &optional NUM)" t nil)

(autoload 'evilnc-copy-and-comment-lines "evil-nerd-commenter" "\
Copy&paste NUM lines and comment out original lines.\nNUM could be negative.\n\nCase 1: If no region selected, operate on current line.\nif NUM>1, comment/uncomment extra N-1 lines from next line\n\nCase 2: Selected region is expanded to make it contain whole lines.\nThen we operate the expanded region.  NUM is ignored.\n\n(fn &optional NUM)" t nil)

(autoload 'evilnc-comment-and-kill-ring-save "evil-nerd-commenter" "\
Comment lines save origin lines into `kill-ring'.\nNUM could be negative.\n\nCase 1: If no region selected, operate on current line.\n;; if NUM>1, comment/uncomment extra N-1 lines from next line\n\nCase 2: Selected region is expanded to make it contain whole lines.\nThen we operate the expanded region.  NUM is ignored.\n\n(fn &optional NUM)" t nil)

(autoload 'evilnc-copy-to-line "evil-nerd-commenter" "\
Copy from current line to LINENUM line.  For non-evil user only.\n\n(fn &optional LINENUM)" t nil)

(autoload 'evilnc-kill-to-line "evil-nerd-commenter" "\
Kill from the current line to the LINENUM line.  For non-evil user only.\n\n(fn &optional LINENUM)" t nil)

(autoload 'evilnc-version "evil-nerd-commenter" "\
The version number.\n\n(fn)" t nil)

(autoload 'evilnc-default-hotkeys "evil-nerd-commenter" "\
Setup the key bindings of evil-nerd-comment.\nIf NO-EVIL-KEYBINDINGS is t, we don't define keybindings in EVIL.\n\n(fn &optional NO-EVIL-KEYBINDINGS)" t nil)

(autoload 'evilnc-imenu-create-index-function "evil-nerd-commenter" "\
Imenu function find comments.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-nerd-commenter" '("evilnc-")))

;;;***

;;;### (autoloads nil "evil-nerd-commenter-operator" "evil-nerd-commenter-operator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-nerd-commenter-operator.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-nerd-commenter-operator" '("evilnc-")))

;;;***

;;;### (autoloads nil "evil-nerd-commenter-sdk" "evil-nerd-commenter-sdk.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-nerd-commenter-sdk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-nerd-commenter-sdk" '("evilnc-")))

;;;***

;;;### (autoloads nil nil ("evil-nerd-commenter-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-nerd-commenter-autoloads.el ends here
