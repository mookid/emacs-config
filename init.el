;;; init.el --- configuration file for emacs!
;; -*- lexical-binding: t -*-

;;; Commentary:
;; My emacs config, with simple options.

;;; Code:
(package-initialize)

;; Common lisp functionalities
(with-no-warnings (require 'cl)) ; useless warning

(setq load-path (cons "~/.emacs.d/lisp/" load-path))

(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'ignore-all "mookid-macros")

(with-title "Naked emacs configuration" (require 'mookid-naked-emacs-config))
(with-title "Evil related packages" (require 'mookid-evil))
(with-message "Loading powerline" (require 'mookid-powerline))
(with-message "Loading rainbow parameters" (require 'mookid-rainbow))
(with-message "Loading company mode" (require 'mookid-company))
(ignore-all "Loading paredit mode" (require 'mookid-paredit))
(with-title "Helm related packages" (require 'mookid-helm))
(with-message "Loading smartparens" (require 'mookid-smartparens))
(with-message "Setting up avy" (require 'mookid-avy))
(with-message "Setting up flycheck" (require 'mookid-flycheck))

(ignore-all
 "Loading ace-isearch"
 (require 'ace-isearch)
 (global-ace-isearch-mode +1)
 (setq-default ace-isearch-function 'avy-goto-char))

(with-title
 "OCaml config"

 (with-message
  "Configure tuareg mode"
  (require 'tuareg)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode))
                auto-mode-alist)))

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
  (add-hook 'merlin-mode-hook 'company-mode)))

(with-message
 "C settings"
 (require 'cc-mode)
 (setq-default c-default-style "linux" c-basic-offset 8)
 (define-key c-mode-base-map (kbd "C-c C-c") 'compile))

(with-message
 "Configuring highlight column"
 (require 'highlight-indentation)
 (set-face-background 'highlight-indentation-face "light steel blue")
 (set-face-background 'highlight-indentation-current-column-face "sienna4"))

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
;;; init.el ends here
