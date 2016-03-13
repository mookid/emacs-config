;;; mookid-naked-emacs-config.el --- Configuration of isearch

;;; Commentary:

;;; Code:
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map "\M-<" 'isearch-beginning-of-buffer)
(define-key isearch-mode-map "\M->" 'isearch-end-of-buffer)

(defun isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

(provide 'mookid-isearch)
;;; mookid-isearch.el ends here
