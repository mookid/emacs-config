;;; init.el --- configuration file for emacs!

;;; Commentary:

;; My Emacs config, with simple options.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'benchmark-init)

;; The configuration directories
(defvar mookid-root-dir "~/.emacs.d"
  "The root directory of the configuration.")

(let ((gc-cons-threshold most-positive-fixnum))
  (load-file (expand-file-name "mookid-init.el" mookid-root-dir)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rust-mode whole-line-or-region use-package tuareg spacemacs-theme smooth-scrolling smooth-scroll smart-mode-line slime ruby-block ranger rainbow-mode rainbow-delimiters rainbow-blocks projectile powerline pos-tip plan9-theme ocp-indent magit loccur lispy image+ icicles hungry-delete haskell-mode gnuplot-mode fullframe flycheck find-file-in-project expand-region evil-nerd-commenter elisp-slime-nav d-mode counsel company clang-format centered-cursor-mode caml benchmark-init base16-theme anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
