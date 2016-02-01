;; -*- lexical-binding: t -*-
;;; mookid-lispy.el --- Configuration for lispy

;;; Commentary:

;;; Code:
(require 'lispy)

(lispy-mode 1)

;; undef `;' binding for ocaml
(define-key lispy-mode-map (kbd "C-;") 'lispy-comment)
(define-key lispy-mode-map (kbd ";") nil)

(defun lispy-on () "Turn lispy mode on." (lispy-mode 1))
(add-hook 'lisp-mode-hook 'lispy-on)
(add-hook 'lisp-interaction-mode-hook 'lispy-on)
(add-hook 'tuareg-mode-hook 'lispy-on)

(provide 'mookid-lispy)
;;; mookid-lispy.el ends here
