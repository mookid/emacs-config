;;; init.el --- configuration file for emacs!

;;; Commentary:

;; My Emacs config, with simple options.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'benchmark-init)

;; The configuration directories
(defvar my-root-dir "~/.emacs.d"
  "The root directory of the configuration.")

(defvar custom-file)
(defvar savehist-file)
(setq custom-file (expand-file-name ".emacs-custom.el" my-root-dir))
(setq savehist-file (expand-file-name "savehist" my-root-dir))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" my-root-dir))))

(let ((gc-cons-threshold most-positive-fixnum))
  (load-file (expand-file-name "my-init.el" my-root-dir))
  (let ((private-file (expand-file-name "private.el" my-root-dir)))
    (when (file-exists-p private-file)
      (condition-case nil
          (load private-file)
        (error (message "Error during loading of private settings"))))))

(provide 'init)
;;; init.el ends here
