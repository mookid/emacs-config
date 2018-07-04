;;; emmet-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emmet-mode" "emmet-mode.el" (0 0 0 0))
;;; Generated autoloads from emmet-mode.el

(autoload 'emmet-expand-line "emmet-mode" "\
Replace the current line's emmet expression with the corresponding expansion.\nIf prefix ARG is given or region is visible call `emmet-preview' to start an\ninteractive preview.\n\nOtherwise expand line directly.\n\nFor more information see `emmet-mode'.\n\n(fn ARG)" t nil)

(autoload 'emmet-mode "emmet-mode" "\
Minor mode for writing HTML and CSS markup.\nWith emmet for HTML and CSS you can write a line like\n\n  ul#name>li.item*2\n\nand have it expanded to\n\n  <ul id=\"name\">\n    <li class=\"item\"></li>\n    <li class=\"item\"></li>\n  </ul>\n\nThis minor mode defines keys for quick access:\n\n\\{emmet-mode-keymap}\n\nHome page URL `http://www.emacswiki.org/emacs/Emmet'.\n\nSee also `emmet-expand-line'.\n\n(fn &optional ARG)" t nil)

(autoload 'emmet-expand-yas "emmet-mode" "\
\n\n(fn)" t nil)

(autoload 'emmet-preview "emmet-mode" "\
Expand emmet between BEG and END interactively.\nThis will show a preview of the expanded emmet code and you can\naccept it or skip it.\n\n(fn BEG END)" t nil)

(autoload 'emmet-wrap-with-markup "emmet-mode" "\
Wrap region with markup.\n\n(fn WRAP-WITH)" t nil)

(autoload 'emmet-next-edit-point "emmet-mode" "\
\n\n(fn COUNT)" t nil)

(autoload 'emmet-prev-edit-point "emmet-mode" "\
\n\n(fn COUNT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emmet-mode" '("emmet-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emmet-mode-autoloads.el ends here
