;;; mookid-naked-emacs-config.el --- Configuration of isearch

;;; Commentary:

;;; Code:
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map (kbd "C-<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)

(define-key isearch-mode-map (kbd "M-<") 'mookid-isearch-beginning-of-buffer)
(define-key isearch-mode-map (kbd "M->") 'mookid-isearch-end-of-buffer)
(define-key global-map (kbd "C-M-s") 'mookid-isearch-region)

(define-key isearch-mode-map (kbd "<S-return>") 'mookid-isearch-exit-leave-hl)

(defun mookid-isearch-exit-leave-hl ()
  "Exit search and leave extra match highlighting."
  (interactive)
  (let ((lazy-highlight-cleanup nil))
    (when isearch-lazy-highlight
      (isearch-lazy-highlight-new-loop (point-min) (point-max)))
    (isearch-exit)))

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

;; Exit isearch at the beginning of the matching string
(add-hook 'isearch-mode-end-hook #'mookid-isearch-exit-beginning)
(defun mookid-isearch-exit-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defun mookid-isearch-region (beg end)
  "Send selection between BEG and END to isearch."
  (interactive "r")
  (deactivate-mark)
  (kill-ring-save beg end)
  (isearch-mode t nil nil nil)
  (isearch-yank-pop))

(defun mookid-occur-rename-buffer ()
  "Used to uniquify the occur buffer names."
  (occur-rename-buffer t))
(add-hook 'occur-hook #'mookid-occur-rename-buffer)

(provide 'mookid-isearch)
;;; mookid-isearch.el ends here
