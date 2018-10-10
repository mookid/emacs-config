(deftheme my-color)

(custom-theme-set-faces
 'my-color
 ;; TODO: company?
 '(compilation-mode-line-fail ((t (:weight bold :inherit error))))
 '(completions-common-part ((t (:foreground "#DDDDDD"))))
 '(completions-first-difference ((t (:background "green" :bold t))))
 '(cursor ((t (:background "black"))))
 '(default ((t (:background "#FFFFEA" :foreground "black"))))
 '(dired-ignored ((t (:foreground "gray60"))))
 '(error ((t (:foreground "red2" :bold t))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#999999"))))
 '(font-lock-string-face ((t (:bold t))))
 '(font-lock-builtin-face ((t (:foreground nil))))
 '(font-lock-constant-face ((t (:foreground nil))))
 '(font-lock-doc-face ((t (:foreground "purple4" :bold t))))
 '(font-lock-function-name-face ((t (:foreground nil))))
 '(font-lock-keyword-face ((t (:foreground nil))))
 '(font-lock-preprocessor-face ((t (:foreground nil))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground nil))))
 '(font-lock-regexp-grouping-construct ((t (:foreground nil))))
 '(font-lock-type-face ((t (:foreground nil))))
 '(font-lock-variable-name-face ((t (:foreground nil))))
 '(font-lock-warning-face ((t (:foreground nil))))
 '(fringe ((t (:background "dark green"))))
 '(isearch ((t (:background "orange"))))
 '(lazy-highlight ((t (:background "yellow"))))
 '(highlight ((t (:background "light green"))))
 '(hl-line ((t (:background "#EAFFFF"))))
 '(match ((t (:background "#EAFFFF"))))
 '(mode-line ((t (:background "#EAFFFF" :foreground "black"))))
 '(mode-line-buffer-id ((t (:inherit mode-line :bold t))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "grey50"))))
 '(region ((t :background "#EEEE9E")))
 '(whitespace-empty ((t (:foreground "#E0E0E0"))))
 '(whitespace-hspace ((t (:foreground "#E0E0E0"))))
 '(whitespace-line ((t (:foreground nil :background nil))))
 '(whitespace-newline ((t (:foreground "#E0E0E0"))))
 '(whitespace-indentation ((t (:foreground "#E0E0E0"))))
 '(whitespace-space ((t (:foreground "#E0E0E0"))))
 '(whitespace-space-after-tab ((t (:foreground "#E0E0E0"))))
 '(whitespace-space-before-tab ((t (:foreground "#E0E0E0"))))
 '(whitespace-tab ((t (:foreground "#E0E0E0"))))
 '(whitespace-trailing ((t (:foreground "red2")))))

(provide-theme 'my-color)
;;; my-color-theme.el ends here
