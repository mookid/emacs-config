;;; tuareg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ocamldebug" "ocamldebug.el" (0 0 0 0))
;;; Generated autoloads from ocamldebug.el

(autoload 'ocamldebug "ocamldebug" "\
Run ocamldebug on program FILE in buffer *ocamldebug-FILE*.\nThe directory containing FILE becomes the initial working directory\nand source-file directory for ocamldebug.  If you wish to change this, use\nthe ocamldebug commands `cd DIR' and `directory'.\n\n(fn PGM-PATH)" t nil)

(defalias 'camldebug 'ocamldebug)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ocamldebug" '("ocamldebug-" "def-ocamldebug")))

;;;***

;;;### (autoloads nil "tuareg" "tuareg.el" (0 0 0 0))
;;; Generated autoloads from tuareg.el
(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
               ".annot" ".cmt" ".cmti"))
 (add-to-list 'completion-ignored-extensions ext))

(autoload 'tuareg-mode "tuareg" "\
Major mode for editing OCaml code.\n\nDedicated to Emacs and XEmacs, version 21 and higher.  Provides\nautomatic indentation and compilation interface.  Performs font/color\nhighlighting using Font-Lock.  It is designed for OCaml but handles\nCaml Light as well.\n\nThe Font-Lock minor-mode is used according to your customization\noptions.\n\nYou have better byte-compile tuareg.el.\n\nFor customization purposes, you should use `tuareg-mode-hook'\n(run for every file) or `tuareg-load-hook' (run once) and not patch\nthe mode itself.  You should add to your configuration file something like:\n  (add-hook 'tuareg-mode-hook\n            (lambda ()\n               ... ; your customization code\n            ))\nFor example you can change the indentation of some keywords, the\n`electric' flags, Font-Lock colors... Every customizable variable is\ndocumented, use `C-h-v' or look at the mode's source code.\n\n`dot-emacs.el' is a sample customization file for standard changes.\nYou can append it to your `.emacs' or use it as a tutorial.\n\n`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable\nFILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.\n\nA Tuareg Interactive Mode to evaluate expressions in a REPL (aka toplevel) is\nincluded.  Type `M-x tuareg-run-ocaml' or simply `M-x run-ocaml' or see\nspecial-keys below.\n\nShort cuts for the Tuareg mode:\n\\{tuareg-mode-map}\n\nShort cuts for interactions with the REPL:\n\\{tuareg-interactive-mode-map}\n\n(fn)" t nil)

(autoload 'tuareg-run-ocaml "tuareg" "\
Run an OCaml REPL process.  I/O via buffer `*OCaml*'.\n\n(fn)" t nil)

(defalias 'run-ocaml 'tuareg-run-ocaml)

(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))

(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg" '("tuareg-")))

;;;***

;;;### (autoloads nil "tuareg-dune" "tuareg-dune.el" (0 0 0 0))
;;; Generated autoloads from tuareg-dune.el

(autoload 'tuareg-dune-mode "tuareg-dune" "\
Major mode to edit dune files.\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)jbuild\\(?:\\.inc\\)?\\'" . tuareg-dune-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg-dune" '("tuareg-dune-" "verbose-tuareg-dune-smie-rules")))

;;;***

;;;### (autoloads nil "tuareg-menhir" "tuareg-menhir.el" (0 0 0 0))
;;; Generated autoloads from tuareg-menhir.el

(add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-menhir-mode))

(autoload 'tuareg-menhir-mode "tuareg-menhir" "\
Major mode to edit Menhir (and Ocamlyacc) files.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg-menhir" '("tuareg-menhir-")))

;;;***

;;;### (autoloads nil "tuareg-opam" "tuareg-opam.el" (0 0 0 0))
;;; Generated autoloads from tuareg-opam.el

(autoload 'tuareg-opam-mode "tuareg-opam" "\
Major mode to edit opam files.\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("[./]opam_?\\'" . tuareg-opam-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg-opam" '("tuareg-opam-" "verbose-tuareg-opam-smie-rules")))

;;;***

;;;### (autoloads nil "tuareg-site-file" "tuareg-site-file.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from tuareg-site-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg-site-file" '("run-ocaml" "camldebug")))

;;;***

;;;### (autoloads nil nil ("dot-emacs.el" "tuareg-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tuareg-autoloads.el ends here
