;;; mookid-tuareg --- Configuration for tuareg

;;; Commentary:

;; With prettify-symbols-mode on, there is by default some ugly prettifiers.
;; I remove them. Customizations is possible by editing the list in the
;; remove-some-prettifiers function.

;;; Code:
(autoload 'tuareg-mode "tuareg")
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

(defun remove-some-prettifiers ()
  "Remove undesirable prettifiers."
  (dolist (str '("||" "&&" "not"))
    (assq-delete-all str prettify-symbols-alist)))
(add-hook 'tuareg-mode-hook 'remove-some-prettifiers)

(defun insert-stars ()
  "Insert a line with stars."
  (interactive)
  (insert (concat "(*************************************"
		  "**************************************)")))

(defun tuareg-evil-bindings ()
  "Bindings for tuareg mode depending on evil mode."
  (global-set-key (kbd "C-'") 'tuareg-eval-region)
  (global-set-key (kbd "C-+") 'insert-stars))
(defun define-tuareg-bindings ()
  "Keybindings for tuareg mode."
  (add-hook 'evil-mode-hook 'tuareg-evil-bindings))
(add-hook 'tuareg-mode-hook 'remove-some-prettifiers)


(provide 'mookid-tuareg)
;;; mookid-tuareg.el ends here
