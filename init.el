;;; init.el --- configuration file for emacs!

;;; Commentary:

;; My Emacs config, with simple options.

;;; Code:
(let ((gc-cons-threshold most-positive-fixnum)
      (debug-on-error t))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (when (version< emacs-version "27.0")
    (package-initialize))

  (add-to-list 'load-path "~/.emacs.d/benchmark-init-el")
  (require 'benchmark-init)
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

  ;; The configuration directories
  (defvar custom-file)
  (defvar my-main-init-file)
  (defvar savehist-file)
  (setq custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory))
  (setq my-main-init-file (expand-file-name "my-init.el" user-emacs-directory))
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (load-file my-main-init-file)
  (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
    (when (file-exists-p private-file)
      (condition-case nil
          (load private-file)
        (error (message "Error during loading of private settings"))))))

(provide 'init)
;;; init.el ends here
