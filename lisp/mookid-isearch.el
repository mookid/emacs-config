;;; mookid-naked-emacs-config.el --- Configuration of isearch

;;; Commentary:

;;; Code:
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(provide 'mookid-isearch)
;;; mookid-isearch.el ends here
