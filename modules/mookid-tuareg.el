;;; mookid-tuareg --- Configuration for tuareg

;;; Commentary:

;; With prettify-symbols-mode on, there is by default some ugly prettifiers.
;; I remove them.  Customizations is possible by editing the list in the
;; remove-some-prettifiers function.

;;; Code:
(autoload 'tuareg-mode "tuareg")
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

(defun remove-some-prettifiers ()
  "Remove undesirable prettifiers."
  (dolist (str '("||" "&&" "not"))
    (assq-delete-all str prettify-symbols-alist)))
(add-hook 'tuareg-mode-hook 'remove-some-prettifiers)

(defvar tuareg-mode-map)
(defvar ocaml-stars "(***************************************************************************)"
  "A separator for OCaml code.")
(defun tuareg-insert-stars ()
  "Insert a line with stars."
  (interactive)
  (newline)
  (insert ocaml-stars)
  (newline))

(with-eval-after-load 'tuareg
  (set-face-attribute 'tuareg-font-lock-module-face nil
                      :foreground "pink"))

(defun define-tuareg-bindings ()
  "Keybindings for tuareg mode."
  (define-key tuareg-mode-map (kbd "C-'") 'tuareg-eval-region)
  (define-key tuareg-mode-map (kbd "C-c =") 'tuareg-insert-stars))
(add-hook 'tuareg-mode-hook 'remove-some-prettifiers)
(add-hook 'tuareg-mode-hook 'define-tuareg-bindings)
(with-eval-after-load 'evil
  (setq-default evil-shift-width 2))

(defun verticalize-sequence (pos1 pos2)
  "Transform a one-line sequence literal to a multi-line."
  (interactive "r")
  (if (use-region-p)
      (save-restriction
	(narrow-to-region pos1 pos2)
	(goto-char (point-min))
	(forward-char)
	(insert "\n")
	(indent-according-to-mode)
	(while (< 2 (- (point-max) (point)))
	  (forward-sexp)
	  (if (equal "," (thing-at-point 'char))
	      (progn
		(forward-char)
		(insert "\n")
		(indent-according-to-mode)
		)))
	;; add a newline before the closing delimiter
	(goto-char  (point-max))
	(backward-char)
	(insert "\n")
	)
    (message "You need to highlight a sequence.")))

(provide 'mookid-tuareg)
;;; mookid-tuareg.el ends here
