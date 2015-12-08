;;; mookid-ocaml --- Configuration for ocaml

;;; Commentary:

;;; Code:
(with-message
 "Configure tuareg mode"
 (require 'tuareg)
 (setq auto-mode-alist
       (cons '("\\.ml[ily]?$" . tuareg-mode) auto-mode-alist)))

(with-message
 "Configure ocp indent"
 (require 'ocp-indent)
 (define-key tuareg-mode-map (kbd "C-=") 'ocp-indent-buffer))

(ignore-all
 "Configuring merlin"
 (require 'merlin)
 (add-hook 'tuareg-mode-hook 'merlin-mode t)
 (setq merlin-use-auto-complete-mode 'easy)
 (with-eval-after-load 'company
   (add-to-list 'company-backends
                'merlin-company-backend))
 (add-hook 'merlin-mode-hook 'company-mode))

(provide 'mookid-ocaml)
;;; mookid-ocaml.el ends here
