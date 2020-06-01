(deftheme my-color)

(custom-theme-set-faces
 'my-color
 '(compilation-mode-line-fail ((t (:weight bold :inherit error))))
 '(completions-common-part ((t (:foreground "#DDDDDD"))))
 '(completions-first-difference ((t (:background "green" :bold t))))
 '(cursor ((t (:background "black"))))
 '(default ((t (:background "#FAFAFA" :foreground "#000"))))
 '(dired-ignored ((t (:foreground "gray60"))))
 '(error ((t (:foreground "red2" :bold t))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#66F"))))
 '(font-lock-string-face ((t (:foreground "#080"))))
 '(font-lock-builtin-face ((t (:foreground nil))))
 '(font-lock-constant-face ((t (:foreground nil))))
 '(font-lock-doc-face ((t (:foreground "#008"))))
 '(font-lock-function-name-face ((t (:foreground nil))))
 '(font-lock-keyword-face ((t (:foreground nil))))
 '(font-lock-preprocessor-face ((t (:foreground nil))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground nil))))
 '(font-lock-regexp-grouping-construct ((t (:foreground nil))))
 '(font-lock-type-face ((t (:foreground nil))))
 '(font-lock-variable-name-face ((t (:foreground nil))))
 '(font-lock-warning-face ((t (:foreground nil))))
 '(isearch ((t (:background "#FED4A4"))))
 '(lazy-highlight ((t (:background "#FBFEA4"))))
 '(highlight ((t (:background "#B4FFB4"))))
 '(hl-line ((t (:background "#EAFFFF" :extend t))))
 '(match ((t (:background "#FFEAFF"))))
 '(mode-line-buffer-id ((t (:foreground "blue4" :background nil :bold t))))
 '(region ((t :background "#DFE2E7" :extend t)))
 '(sh-heredoc ((t :background nil :foreground "dark blue")))
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
