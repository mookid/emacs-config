;;; multiple-cursors-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mc-cycle-cursors" "mc-cycle-cursors.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from mc-cycle-cursors.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-cycle-cursors" '("mc/")))

;;;***

;;;### (autoloads nil "mc-edit-lines" "mc-edit-lines.el" (0 0 0 0))
;;; Generated autoloads from mc-edit-lines.el

(autoload 'mc/edit-lines "mc-edit-lines" "\
Add one cursor to each line of the active region.\nStarts from mark and moves in straight down or up towards the\nline point is on.\n\nWhat is done with lines which are not long enough is governed by\n`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used\nto override this.  If ARG is a symbol (when called from Lisp),\nthat symbol is used instead of `mc/edit-lines-empty-lines'.\nOtherwise, if ARG negative, short lines will be ignored.  Any\nother non-nil value will cause short lines to be padded.\n\n(fn &optional ARG)" t nil)

(autoload 'mc/edit-ends-of-lines "mc-edit-lines" "\
Add one cursor to the end of each line in the active region.\n\n(fn)" t nil)

(autoload 'mc/edit-beginnings-of-lines "mc-edit-lines" "\
Add one cursor to the beginning of each line in the active region.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-edit-lines" '("mc/edit-lines-empty-lines")))

;;;***

;;;### (autoloads nil "mc-hide-unmatched-lines-mode" "mc-hide-unmatched-lines-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from mc-hide-unmatched-lines-mode.el

(autoload 'mc-hide-unmatched-lines-mode "mc-hide-unmatched-lines-mode" "\
Minor mode when enabled hides all lines where no cursors (and\nalso hum/lines-to-expand below and above) To make use of this\nmode press \"C-'\" while multiple-cursor-mode is active. You can\nstill edit lines while you are in mc-hide-unmatched-lines\nmode. To leave this mode press <return> or \"C-g\"\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-hide-unmatched-lines-mode" '("hum/")))

;;;***

;;;### (autoloads nil "mc-mark-more" "mc-mark-more.el" (0 0 0 0))
;;; Generated autoloads from mc-mark-more.el

(autoload 'mc/mark-next-like-this "mc-mark-more" "\
Find and mark the next part of the buffer matching the currently active region\nIf no region is active add a cursor on the next line\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-next-like-this-word "mc-mark-more" "\
Find and mark the next part of the buffer matching the currently active region\nIf no region is active, mark the word at the point and find the next match\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-next-word-like-this "mc-mark-more" "\
Find and mark the next word of the buffer matching the currently active region\nThe matching region must be a whole word to be a match\nIf no region is active, mark the symbol at the point and find the next match\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-next-symbol-like-this "mc-mark-more" "\
Find and mark the next symbol of the buffer matching the currently active region\nThe matching region must be a whole symbol to be a match\nIf no region is active, mark the symbol at the point and find the next match\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-previous-like-this "mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region\nIf no region is active add a cursor on the previous line\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-previous-like-this-word "mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region\nIf no region is active, mark the word at the point and find the previous match\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark previous.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-previous-word-like-this "mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region\nThe matching region must be a whole word to be a match\nIf no region is active add a cursor on the previous line\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-previous-symbol-like-this "mc-mark-more" "\
Find and mark the previous part of the buffer matching the currently active region\nThe matching region must be a whole symbol to be a match\nIf no region is active add a cursor on the previous line\nWith negative ARG, delete the last one instead.\nWith zero ARG, skip the last one and mark next.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-next-lines "mc-mark-more" "\
\n\n(fn ARG)" t nil)

(autoload 'mc/mark-previous-lines "mc-mark-more" "\
\n\n(fn ARG)" t nil)

(autoload 'mc/unmark-next-like-this "mc-mark-more" "\
Deselect next part of the buffer matching the currently active region.\n\n(fn)" t nil)

(autoload 'mc/unmark-previous-like-this "mc-mark-more" "\
Deselect prev part of the buffer matching the currently active region.\n\n(fn)" t nil)

(autoload 'mc/skip-to-next-like-this "mc-mark-more" "\
Skip the current one and select the next part of the buffer matching the currently active region.\n\n(fn)" t nil)

(autoload 'mc/skip-to-previous-like-this "mc-mark-more" "\
Skip the current one and select the prev part of the buffer matching the currently active region.\n\n(fn)" t nil)

(autoload 'mc/mark-all-like-this "mc-mark-more" "\
Find and mark all the parts of the buffer matching the currently active region\n\n(fn)" t nil)

(autoload 'mc/mark-all-words-like-this "mc-mark-more" "\
\n\n(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this "mc-mark-more" "\
\n\n(fn)" t nil)

(autoload 'mc/mark-all-in-region "mc-mark-more" "\
Find and mark all the parts in the region matching the given search\n\n(fn BEG END &optional SEARCH)" t nil)

(autoload 'mc/mark-all-in-region-regexp "mc-mark-more" "\
Find and mark all the parts in the region matching the given regexp.\n\n(fn BEG END)" t nil)

(autoload 'mc/mark-more-like-this-extended "mc-mark-more" "\
Like mark-more-like-this, but then lets you adjust with arrows key.\nThe adjustments work like this:\n\n   <up>    Mark previous like this and set direction to 'up\n   <down>  Mark next like this and set direction to 'down\n\nIf direction is 'up:\n\n   <left>  Skip past the cursor furthest up\n   <right> Remove the cursor furthest up\n\nIf direction is 'down:\n\n   <left>  Remove the cursor furthest down\n   <right> Skip past the cursor furthest down\n\nThe bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'.\n\n(fn)" t nil)

(autoload 'mc/mark-all-like-this-dwim "mc-mark-more" "\
Tries to guess what you want to mark all of.\nCan be pressed multiple times to increase selection.\n\nWith prefix, it behaves the same as original `mc/mark-all-like-this'\n\n(fn ARG)" t nil)

(autoload 'mc/mark-all-dwim "mc-mark-more" "\
Tries even harder to guess what you want to mark all of.\n\nIf the region is active and spans multiple lines, it will behave\nas if `mc/mark-all-in-region'. With the prefix ARG, it will call\n`mc/edit-lines' instead.\n\nIf the region is inactive or on a single line, it will behave like\n`mc/mark-all-like-this-dwim'.\n\n(fn ARG)" t nil)

(autoload 'mc/mark-all-like-this-in-defun "mc-mark-more" "\
Mark all like this in defun.\n\n(fn)" t nil)

(autoload 'mc/mark-all-words-like-this-in-defun "mc-mark-more" "\
Mark all words like this in defun.\n\n(fn)" t nil)

(autoload 'mc/mark-all-symbols-like-this-in-defun "mc-mark-more" "\
Mark all symbols like this in defun.\n\n(fn)" t nil)

(autoload 'mc/toggle-cursor-on-click "mc-mark-more" "\
Add a cursor where you click, or remove a fake cursor that is\nalready there.\n\n(fn EVENT)" t nil)

(defalias 'mc/add-cursor-on-click 'mc/toggle-cursor-on-click)

(autoload 'mc/mark-sgml-tag-pair "mc-mark-more" "\
Mark the tag we're in and its pair for renaming.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-mark-more" '("mc/" "mc--")))

;;;***

;;;### (autoloads nil "mc-mark-pop" "mc-mark-pop.el" (0 0 0 0))
;;; Generated autoloads from mc-mark-pop.el

(autoload 'mc/mark-pop "mc-mark-pop" "\
Add a cursor at the current point, pop off mark ring and jump\nto the popped mark.\n\n(fn)" t nil)

;;;***

;;;### (autoloads nil "mc-separate-operations" "mc-separate-operations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from mc-separate-operations.el

(autoload 'mc/insert-numbers "mc-separate-operations" "\
Insert increasing numbers for each cursor, starting at\n`mc/insert-numbers-default' or ARG.\n\n(fn ARG)" t nil)

(autoload 'mc/insert-letters "mc-separate-operations" "\
Insert increasing letters for each cursor, starting at 0 or ARG.\n     Where letter[0]=a letter[2]=c letter[26]=aa\n\n(fn ARG)" t nil)

(autoload 'mc/reverse-regions "mc-separate-operations" "\
\n\n(fn)" t nil)

(autoload 'mc/sort-regions "mc-separate-operations" "\
\n\n(fn)" t nil)

(autoload 'mc/vertical-align "mc-separate-operations" "\
Aligns all cursors vertically with a given CHARACTER to the one with the\nhighest colum number (the rightest).\nMight not behave as intended if more than one cursors are on the same line.\n\n(fn CHARACTER)" t nil)

(autoload 'mc/vertical-align-with-space "mc-separate-operations" "\
Aligns all cursors with whitespace like `mc/vertical-align' does\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-separate-operations" '("mc/insert-numbers-default" "mc--")))

;;;***

;;;### (autoloads nil "multiple-cursors-core" "multiple-cursors-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from multiple-cursors-core.el

(autoload 'multiple-cursors-mode "multiple-cursors-core" "\
Mode while multiple cursors are active.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multiple-cursors-core" '("multiple-cursors-mode" "unsupported-cmd" "deactivate-cursor-after-undo" "activate-cursor-for-undo")))

;;;***

;;;### (autoloads nil "rectangular-region-mode" "rectangular-region-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rectangular-region-mode.el

(autoload 'set-rectangular-region-anchor "rectangular-region-mode" "\
Anchors the rectangular region at point.\n\nThink of this one as `set-mark' except you're marking a rectangular region. It is\nan exceedingly quick way of adding multiple cursors to multiple lines.\n\n(fn)" t nil)

(autoload 'rectangular-region-mode "rectangular-region-mode" "\
A mode for creating a rectangular region to edit\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rectangular-region-mode" '("rectangular-region-mode" "rrm/")))

;;;***

;;;### (autoloads nil nil ("multiple-cursors-pkg.el" "multiple-cursors.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multiple-cursors-autoloads.el ends here
