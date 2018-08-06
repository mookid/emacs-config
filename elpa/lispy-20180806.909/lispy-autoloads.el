;;; lispy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "le-clojure" "le-clojure.el" (0 0 0 0))
;;; Generated autoloads from le-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "le-clojure" '("lispy-")))

;;;***

;;;### (autoloads nil "le-hy" "le-hy.el" (0 0 0 0))
;;; Generated autoloads from le-hy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "le-hy" '("lispy--")))

;;;***

;;;### (autoloads nil "le-julia" "le-julia.el" (0 0 0 0))
;;; Generated autoloads from le-julia.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "le-julia" '("lispy-")))

;;;***

;;;### (autoloads nil "le-lisp" "le-lisp.el" (0 0 0 0))
;;; Generated autoloads from le-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "le-lisp" '("lispy-")))

;;;***

;;;### (autoloads nil "le-python" "le-python.el" (0 0 0 0))
;;; Generated autoloads from le-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "le-python" '("lispy-")))

;;;***

;;;### (autoloads nil "le-scheme" "le-scheme.el" (0 0 0 0))
;;; Generated autoloads from le-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "le-scheme" '("lispy-")))

;;;***

;;;### (autoloads nil "lispy" "lispy.el" (0 0 0 0))
;;; Generated autoloads from lispy.el

(autoload 'lispy-mode "lispy" "\
Minor mode for navigating and editing LISP dialects.\n\nWhen `lispy-mode' is on, most unprefixed keys,\ni.e. [a-zA-Z+-./<>], conditionally call commands instead of\nself-inserting. The condition (called special further on) is one\nof:\n\n- the point is before \"(\"\n- the point is after \")\"\n- the region is active\n\nFor instance, when special, \"j\" moves down one sexp, otherwise\nit inserts itself.\n\nWhen special, [0-9] call `digit-argument'.\n\nWhen `lispy-mode' is on, \"[\" and \"]\" move forward and\nbackward through lists, which is useful to move into special.\n\n\\{lispy-mode-map}\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lispy" '("lh-knight" "lispy-" "mc/cmds-to-run-" "ac-trigger-commands" "unsupported-mode-error" "hydra-lispy-x")))

;;;***

;;;### (autoloads nil "lispy-inline" "lispy-inline.el" (0 0 0 0))
;;; Generated autoloads from lispy-inline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lispy-inline" '("lispy-")))

;;;***

;;;### (autoloads nil "lispy-tags" "lispy-tags.el" (0 0 0 0))
;;; Generated autoloads from lispy-tags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lispy-tags" '("lispy-")))

;;;***

;;;### (autoloads nil nil ("elpa.el" "lispy-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lispy-autoloads.el ends here
