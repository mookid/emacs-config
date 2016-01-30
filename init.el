;;; init.el --- configuration file for emacs!

;;; Commentary:

;; My Emacs config, with simple options.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; The configuration directories
(defvar mookid-root-dir "~/.emacs.d"
  "The root directory of the configuration.")

(defvar mookid-lisp-dir (expand-file-name "lisp" mookid-root-dir)
  "This is where the non compiled Lisp files are.")

(defvar mookid-modules-dir (expand-file-name "modules" mookid-root-dir)
  "This is where the configuration of the Lisp files are.")

(add-to-list 'load-path mookid-lisp-dir)
(add-to-list 'load-path mookid-modules-dir)

(let ((gc-cons-threshold most-positive-fixnum)) (require 'mookid-init))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lispy tuareg smooth-scrolling slime rainbow-delimiters rainbow-blocks ocp-indent gnuplot-mode flycheck find-file-in-project expand-region evil-visualstar evil-search-highlight-persist evil-numbers evil-jumper evil-anzu elisp-slime-nav d-mode counsel company clang-format ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
