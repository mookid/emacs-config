;;; mookid-ocaml --- Configuration for ocaml

;;; Commentary:

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'ignore-all "mookid-macros")

(with-message
 "Configure tuareg mode"
 (require 'tuareg)
 (add-to-list auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode)))

(with-message
 "Configure ocp indent"
 (require 'ocp-indent)
 (defvar tuareg-mode-map)
 (add-hook 'tuareg-mode-hook
           (lambda ()
             (define-key tuareg-mode-map (kbd "C-=") 'ocp-indent-buffer))))

(ignore-all
 "Configuring merlin"
 (require 'merlin)
 (add-hook 'tuareg-mode-hook 'merlin-mode t)
 (setq-default merlin-use-auto-complete-mode 'easy)
 (defvar company-backends)
 (with-eval-after-load 'company
   (add-to-list 'company-backends 'merlin-company-backend))
 (add-hook 'merlin-mode-hook 'company-mode))

(provide 'mookid-ocaml)
;;; mookid-ocaml.el ends here
