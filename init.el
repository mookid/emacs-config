;;; init.el --- configuration file for emacs!

;;; Commentary:

;; My Emacs config, with simple options.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'benchmark-init)

;; The configuration directories
(defvar custom-file)
(defvar savehist-file)
(defvar yas-snippet-dirs)
(setq custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory))
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

(setq debug-on-error t)
(let ((gc-cons-threshold most-positive-fixnum))
  (load-file (expand-file-name "my-init.el" user-emacs-directory))
  (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
    (when (file-exists-p private-file)
      (condition-case nil
          (load private-file)
        (error (message "Error during loading of private settings"))))))
(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
