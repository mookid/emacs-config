;;; mookid-tuareg --- Configuration for tuareg

;;; Commentary:

;;; Code:
(autoload 'tuareg-mode "tuareg")
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

(defvar tuareg-mode-map)
(defvar mookid-ocaml-stars "(***************************************************************************)"
  "A separator for OCaml code.")
(defun mookid-tuareg-insert-stars ()
  "Insert a line with stars."
  (interactive)
  (newline)
  (insert mookid-ocaml-stars)
  (newline))

(defun mookid-tuareg-setup ()
  "Keybindings for tuareg mode."
  (define-key tuareg-mode-map (kbd "C-'") 'tuareg-eval-region)
  (define-key tuareg-mode-map (kbd "C-c =") 'mookid-tuareg-insert-stars))

(add-hook 'tuareg-mode-hook 'mookid-tuareg-setup)

(with-eval-after-load 'caml
  (mapc (lambda (face)
	  (when (string-prefix-p "caml-types" (face-name face))
	    (set-face-attribute face nil
				:background "deep pink"
				:foreground "white")))
	(face-list)))

(provide 'mookid-tuareg)
;;; mookid-tuareg.el ends here
