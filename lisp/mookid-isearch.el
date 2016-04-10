;;; mookid-naked-emacs-config.el --- Configuration of isearch

;;; Commentary:

;;; Code:
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)

(define-key isearch-mode-map "\M-<" 'mookid-isearch-beginning-of-buffer)
(define-key isearch-mode-map "\M->" 'mookid-isearch-end-of-buffer)

(defun mookid-isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun mookid-isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

;; Wrap without failing
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

(provide 'mookid-isearch)
;;; mookid-isearch.el ends here
