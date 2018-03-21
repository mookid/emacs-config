(deftheme my-color)

(custom-theme-set-faces
 'my-color
 `(default ((t :foreground "#ffffff" :background "#2d3743")))
 `(highlight ((t :background "#2e2d43")))
 `(region ((t :background "#43392d")))
 '(error ((t :foreground "#ff0000")))
 '(diff-added ((t :foreground "#00ff00")))
 '(diff-removed ((t :foreground "#ff0000")))
 '(diff-refine-added ((t :foreground "#00ff00" :background "#004f00")))
 '(diff-refine-removed ((t :foreground "#ff0000" :background "#4f0000"))))

(provide-theme 'my-color)
;;; my-color-theme.el ends here
