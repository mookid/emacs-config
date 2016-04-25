;;; mookid-caml --- Configuration for caml mode

;;; Commentary:

;;; Code:
(require 'caml)
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . caml-mode))

(defvar caml-mode-map)
(defvar mookid-ocaml-stars "(***************************************************************************)"
  "A separator for OCaml code.")
(defun mookid-caml-insert-stars ()
  "Insert a line with stars."
  (interactive)
  (newline)
  (insert mookid-ocaml-stars)
  (newline))

(defun mookid-caml-setup ()
  "Keybindings for caml mode."
  (define-key caml-mode-map (kbd "C-c =") 'mookid-caml-insert-stars))

(add-hook 'caml-mode-hook 'mookid-caml-setup)
(require 'rainbow-blocks)
(add-hook 'caml-mode-hook 'rainbow-blocks-mode)

(with-eval-after-load 'caml
  (mapc (lambda (face)
          (when (string-prefix-p "caml-types" (face-name face))
            (set-face-attribute face nil
                                :background "deep pink"
                                :foreground "white")))
        (face-list)))

(provide 'mookid-caml)
;;; mookid-caml.el ends here
