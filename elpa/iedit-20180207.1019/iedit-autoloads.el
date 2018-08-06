;;; iedit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "iedit" "iedit.el" (0 0 0 0))
;;; Generated autoloads from iedit.el

(autoload 'iedit-mode "iedit" "\
Toggle Iedit mode.\nThis command behaves differently, depending on the mark, point,\nprefix argument and variable `iedit-transient-mark-sensitive'.\n\nIf Iedit mode is off, turn Iedit mode on.\n\nWhen Iedit mode is turned on, all the occurrences of the current\nregion in the buffer (possibly narrowed) or a region are\nhighlighted.  If one occurrence is modified, the change are\npropagated to all other occurrences simultaneously.\n\nIf region is not active, `iedit-default-occurrence' is called to\nget an occurrence candidate, according to the thing at point.  It\nmight be url, email address, markup tag or current symbol(or\nword).\n\nIn the above two situations, with digit prefix argument 0, only\noccurrences in current function are matched.  This is good for\nrenaming refactoring in programming.\n\nYou can also switch to Iedit mode from isearch mode directly. The\ncurrent search string is used as occurrence.  All occurrences of\nthe current search string are highlighted.\n\nWith an universal prefix argument, the occurrence when Iedit mode\nis turned off last time in current buffer is used as occurrence.\nThis is intended to recover last Iedit mode which is turned off.\nIf region active, Iedit mode is limited within the current\nregion.\n\nWith repeated universal prefix argument, the occurrence when\nIedit mode is turned off last time (might be in other buffer) is\nused as occurrence.  If region active, Iedit mode is limited\nwithin the current region.\n\nWith digital prefix argument 1, Iedit mode is limited on the\ncurrent symbol or the active region, which means just one\ninstance is highlighted.  This behavior serves as a start point\nof incremental selection work flow.\n\nIf Iedit mode is on and region is active, Iedit mode is\nrestricted in the region, e.g. the occurrences outside of the\nregion is excluded.\n\nIf Iedit mode is on and region is active, with an universal\nprefix argument, Iedit mode is restricted outside of the region,\ne.g. the occurrences in the region is excluded.\n\nTurn off Iedit mode in other situations.\n\nCommands:\n\\{iedit-mode-keymap}\nKeymap used within overlays:\n\\{iedit-mode-occurrence-keymap}\n\n(fn &optional ARG)" t nil)

(autoload 'iedit-mode-toggle-on-function "iedit" "\
Toggle Iedit mode on current function.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "iedit" '("iedit-")))

;;;***

;;;### (autoloads nil "iedit-lib" "iedit-lib.el" (0 0 0 0))
;;; Generated autoloads from iedit-lib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "iedit-lib" '("iedit-")))

;;;***

;;;### (autoloads nil "iedit-rect" "iedit-rect.el" (0 0 0 0))
;;; Generated autoloads from iedit-rect.el

(autoload 'iedit-rectangle-mode "iedit-rect" "\
Toggle Iedit-rect mode.\n\nWhen Iedit-rect mode is on, a rectangle is started with visible\nrectangle highlighting.  Rectangle editing support is based on\nIedit mechanism.\n\nCommands:\n\\{iedit-rect-keymap}\n\n(fn &optional BEG END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "iedit-rect" '("iedit-rect")))

;;;***

;;;### (autoloads nil nil ("iedit-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; iedit-autoloads.el ends here
