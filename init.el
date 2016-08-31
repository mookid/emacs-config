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

(let ((gc-cons-threshold most-positive-fixnum))
  (load-file (expand-file-name "my-init.el" my-root-dir)))

(provide 'init)
;;; init.el ends here
