;;; mookid-ocaml --- Configuration for ocaml

;;; Commentary:

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'ignore-all "mookid-macros")

(with-message
 "Configure tuareg mode"
 (autoload 'tuareg-mode "tuareg")
 (add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
 (defun remove-some-prettifiers ()
   (dolist (str '("||" "&&" "not"))
     (assq-delete-all str prettify-symbols-alist)))
 (add-hook 'tuareg-mode 'remove-some-prettifiers))

(with-message
 "Configure ocp indent"
 (defvar tuareg-mode-map)
 (defun ocp-indent-setup ()
   (require 'ocp-indent)
   (define-key tuareg-mode-map (kbd "C-=") 'ocp-indent-buffer))
 (add-hook 'tuareg-mode-hook 'ocp-indent-setup))

(ignore-all
 "Configuring merlin"
 (defun merlin-load () (require 'merlin) (merlin-mode))
 (add-hook 'tuareg-mode-hook 'merlin-load t)
 (defun merlin-setup ()
   (setq-default merlin-use-auto-complete-mode 'easy)
   (defvar company-backends)
   (with-eval-after-load 'company
     (add-to-list 'company-backends 'merlin-company-backend)))
 (add-hook 'merlin-mode-hook 'merlin-setup))

(provide 'mookid-ocaml)
;;; mookid-ocaml.el ends here
