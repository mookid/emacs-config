;;; mookid-tuareg --- Configuration for tuareg

;;; Commentary:

;; With prettify-symbols-mode on, there is by default some ugly prettifiers.
;; I remove them.  Customizations is possible by editing the list in the
;; remove-some-prettifiers function.

;;; Code:
(autoload 'tuareg-mode "tuareg")
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

(defvar tuareg-mode-map)
(defvar ocaml-stars "(***************************************************************************)"
  "A separator for OCaml code.")
(defun tuareg-insert-stars ()
  "Insert a line with stars."
  (interactive)
  (newline)
  (insert ocaml-stars)
  (newline))

(defun define-tuareg-bindings ()
  "Keybindings for tuareg mode."
  (define-key tuareg-mode-map (kbd "C-'") 'tuareg-eval-region)
  (define-key tuareg-mode-map (kbd "C-c =") 'tuareg-insert-stars))

(add-hook 'tuareg-mode-hook 'define-tuareg-bindings)
(with-eval-after-load 'evil
  (setq-default evil-shift-width 2))

(provide 'mookid-tuareg)
;;; mookid-tuareg.el ends here
