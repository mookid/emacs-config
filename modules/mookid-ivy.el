;;; mookid-ivy --- Configuration for ivy

;;; Commentary:

;;; Code:
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(require 'counsel)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-x") 'counsel-M-x)
(define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)

(defvar evil-normal-state-map)
(define-key evil-normal-state-map (kbd "C-b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "RET") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "<C-return>") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC") 'counsel-M-x)

(require 'imenu)
(defun ivy-imenu-get-candidates-from (alist  &optional prefix)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                     (ivy-imenu-get-candidates-from
                      (cl-loop for (e . v) in (cdr elm) collect
                               (cons e (if (integerp v) (copy-marker v) v)))
                      (concat prefix (if prefix ".") (car elm)))
                   (and (cdr elm) ; bug in imenu, should not be needed.
                        (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                        (list (cons (concat prefix (if prefix ".") (car elm))
                                    (copy-marker (cdr elm))))))))

(provide 'mookid-ivy)
;;; mookid-ivy.el ends here
