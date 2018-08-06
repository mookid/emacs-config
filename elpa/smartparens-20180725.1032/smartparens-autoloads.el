;;; smartparens-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smartparens" "smartparens.el" (0 0 0 0))
;;; Generated autoloads from smartparens.el

(autoload 'sp-cheat-sheet "smartparens" "\
Generate a cheat sheet of all the smartparens interactive functions.\n\nWithout a prefix argument, print only the short documentation and examples.\n\nWith non-nil prefix argument ARG, show the full documentation for each function.\n\nYou can follow the links to the function or variable help page.\nTo get back to the full list, use \\[help-go-back].\n\nYou can use `beginning-of-defun' and `end-of-defun' to jump to\nthe previous/next entry.\n\nExamples are fontified using the `font-lock-string-face' for\nbetter orientation.\n\n(fn &optional ARG)" t nil)

(defvar smartparens-mode-map (make-sparse-keymap) "\
Keymap used for `smartparens-mode'.")

(autoload 'sp-use-paredit-bindings "smartparens" "\
Initiate `smartparens-mode-map' with `sp-paredit-bindings'.\n\n(fn)" t nil)

(autoload 'sp-use-smartparens-bindings "smartparens" "\
Initiate `smartparens-mode-map' with `sp-smartparens-bindings'.\n\n(fn)" t nil)

(autoload 'smartparens-mode "smartparens" "\
Toggle smartparens mode.\n\nYou can enable pre-set bindings by customizing\n`sp-base-key-bindings' variable.  The current content of\n`smartparens-mode-map' is:\n\n \\{smartparens-mode-map}\n\n(fn &optional ARG)" t nil)

(autoload 'smartparens-strict-mode "smartparens" "\
Toggle the strict smartparens mode.\n\nWhen strict mode is active, `delete-char', `kill-word' and their\nbackward variants will skip over the pair delimiters in order to\nkeep the structure always valid (the same way as `paredit-mode'\ndoes).  This is accomplished by remapping them to\n`sp-delete-char' and `sp-kill-word'.  There is also function\n`sp-kill-symbol' that deletes symbols instead of words, otherwise\nworking exactly the same (it is not bound to any key by default).\n\nWhen strict mode is active, this is indicated with \"/s\"\nafter the smartparens indicator in the mode list.\n\n(fn &optional ARG)" t nil)

(defvar smartparens-global-strict-mode nil "\
Non-nil if Smartparens-Global-Strict mode is enabled.\nSee the `smartparens-global-strict-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `smartparens-global-strict-mode'.")

(custom-autoload 'smartparens-global-strict-mode "smartparens" nil)

(autoload 'smartparens-global-strict-mode "smartparens" "\
Toggle Smartparens-Strict mode in all buffers.\nWith prefix ARG, enable Smartparens-Global-Strict mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nSmartparens-Strict mode is enabled in all buffers where\n`turn-on-smartparens-strict-mode' would do it.\nSee `smartparens-strict-mode' for more information on Smartparens-Strict mode.\n\n(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-strict-mode "smartparens" "\
Turn on `smartparens-strict-mode'.\n\n(fn)" t nil)

(autoload 'turn-off-smartparens-strict-mode "smartparens" "\
Turn off `smartparens-strict-mode'.\n\n(fn)" t nil)

(defvar smartparens-global-mode nil "\
Non-nil if Smartparens-Global mode is enabled.\nSee the `smartparens-global-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `smartparens-global-mode'.")

(custom-autoload 'smartparens-global-mode "smartparens" nil)

(autoload 'smartparens-global-mode "smartparens" "\
Toggle Smartparens mode in all buffers.\nWith prefix ARG, enable Smartparens-Global mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nSmartparens mode is enabled in all buffers where\n`turn-on-smartparens-mode' would do it.\nSee `smartparens-mode' for more information on Smartparens mode.\n\n(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-mode "smartparens" "\
Turn on `smartparens-mode'.\n\nThis function is used to turn on `smartparens-global-mode'.\n\nBy default `smartparens-global-mode' ignores buffers with\n`mode-class' set to special, but only if they are also not comint\nbuffers.\n\nAdditionally, buffers on `sp-ignore-modes-list' are ignored.\n\nYou can still turn on smartparens in these mode manually (or\nin mode's startup-hook etc.) by calling `smartparens-mode'.\n\n(fn)" t nil)

(autoload 'turn-off-smartparens-mode "smartparens" "\
Turn off `smartparens-mode'.\n\n(fn)" t nil)

(autoload 'show-smartparens-mode "smartparens" "\
Toggle visualization of matching pairs.  When enabled, any\nmatching pair is highlighted after `sp-show-pair-delay' seconds\nof Emacs idle time if the point is immediately in front or after\na pair.  This mode works similarly to `show-paren-mode', but\nsupport custom pairs.\n\n(fn &optional ARG)" t nil)

(defvar show-smartparens-global-mode nil "\
Non-nil if Show-Smartparens-Global mode is enabled.\nSee the `show-smartparens-global-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `show-smartparens-global-mode'.")

(custom-autoload 'show-smartparens-global-mode "smartparens" nil)

(autoload 'show-smartparens-global-mode "smartparens" "\
Toggle Show-Smartparens mode in all buffers.\nWith prefix ARG, enable Show-Smartparens-Global mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nShow-Smartparens mode is enabled in all buffers where\n`turn-on-show-smartparens-mode' would do it.\nSee `show-smartparens-mode' for more information on Show-Smartparens mode.\n\n(fn &optional ARG)" t nil)

(autoload 'turn-on-show-smartparens-mode "smartparens" "\
Turn on `show-smartparens-mode'.\n\n(fn)" t nil)

(autoload 'turn-off-show-smartparens-mode "smartparens" "\
Turn off `show-smartparens-mode'.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens" '("smartparens-" "sp-")))

;;;***

;;;### (autoloads nil "smartparens-clojure" "smartparens-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-clojure" '("sp-clojure-prefix")))

;;;***

;;;### (autoloads nil "smartparens-config" "smartparens-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-config" '("sp-lisp-invalid-hyperlink-p")))

;;;***

;;;### (autoloads nil "smartparens-crystal" "smartparens-crystal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-crystal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-crystal" '("sp-crystal-")))

;;;***

;;;### (autoloads nil "smartparens-elixir" "smartparens-elixir.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-elixir.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-elixir" '("sp-elixir-")))

;;;***

;;;### (autoloads nil "smartparens-ess" "smartparens-ess.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-ess.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ess" '("sp-ess-")))

;;;***

;;;### (autoloads nil "smartparens-haskell" "smartparens-haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-haskell" '("sp-")))

;;;***

;;;### (autoloads nil "smartparens-html" "smartparens-html.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-html" '("sp-html-")))

;;;***

;;;### (autoloads nil "smartparens-latex" "smartparens-latex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-latex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-latex" '("sp-latex-")))

;;;***

;;;### (autoloads nil "smartparens-lua" "smartparens-lua.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-lua.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-lua" '("sp-lua-post-keyword-insert")))

;;;***

;;;### (autoloads nil "smartparens-markdown" "smartparens-markdown.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-markdown.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-markdown" '("sp-")))

;;;***

;;;### (autoloads nil "smartparens-org" "smartparens-org.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from smartparens-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-org" '("sp--org-skip-asterisk")))

;;;***

;;;### (autoloads nil "smartparens-python" "smartparens-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-python" '("sp-python-fix-tripple-quotes")))

;;;***

;;;### (autoloads nil "smartparens-ruby" "smartparens-ruby.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-ruby.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ruby" '("sp-")))

;;;***

;;;### (autoloads nil "smartparens-rust" "smartparens-rust.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-rust.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rust" '("sp-")))

;;;***

;;;### (autoloads nil "smartparens-scala" "smartparens-scala.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smartparens-scala.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-scala" '("sp-scala-wrap-with-indented-newlines")))

;;;***

;;;### (autoloads nil "smartparens-text" "smartparens-text.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from smartparens-text.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-text" '("sp-text-mode-")))

;;;***

;;;### (autoloads nil nil ("smartparens-javascript.el" "smartparens-ml.el"
;;;;;;  "smartparens-pkg.el" "smartparens-racket.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smartparens-autoloads.el ends here
