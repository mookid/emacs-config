;;; mookid-naked-emacs-config.el --- Configuration of isearch

;;; Commentary:

;;; Code:
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(add-hook 'isearch-mode-hook
          (lambda ()
            (set (make-local-variable 'isearch-mode-line-face-remap-cookie)
                 (face-remap-add-relative
                  'mode-line '((:foreground "black" :background "orange")
			       mode-line)))))

(add-hook 'isearch-mode-end-hook
          (lambda ()
            (face-remap-remove-relative isearch-mode-line-face-remap-cookie)))
(provide 'mookid-isearch)
;;; mookid-isearch.el ends here
