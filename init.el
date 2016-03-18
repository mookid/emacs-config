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
