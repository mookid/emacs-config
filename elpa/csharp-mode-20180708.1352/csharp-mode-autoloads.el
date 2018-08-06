;;; csharp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "csharp-mode" "csharp-mode.el" (0 0 0 0))
;;; Generated autoloads from csharp-mode.el

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(autoload 'csharp-mode "csharp-mode" "\
Major mode for editing C# code.\n\nThe mode provides fontification and indent for C# syntax, as well\nas some other handy features.\n\nAt mode startup, there are two interesting hooks that run:\n`prog-mode-hook' is run with no args, then `csharp-mode-hook' is run after\nthat, also with no args.\n\nTo run your own logic after csharp-mode starts, do this:\n\n  (defun my-csharp-mode-fn ()\n    \"my function that runs when csharp-mode is initialized for a buffer.\"\n    (turn-on-font-lock)\n    (turn-on-auto-revert-mode) ;; helpful when also using Visual Studio\n    (setq indent-tabs-mode nil) ;; tabs are evil\n    ....your own code here...\n  )\n  (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)\n\n\nThe function above is just a suggestion.\n\n\nImenu Integration\n===============================\n\nCheck the menubar for menu entries for Imenu; it is labelled\n\"Index\".\n\nThe Imenu index gets computed when the file is .cs first opened and loaded.\nThis may take a moment or two.  If you don't like this delay and don't\nuse Imenu, you can turn this off with the variable `csharp-want-imenu'.\n\n\n\nKey bindings:\n\\{csharp-mode-map}\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "csharp-mode" '("c-looking-at-inexpr-block" "csharp-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; csharp-mode-autoloads.el ends here
