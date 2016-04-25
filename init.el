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

(let ((gc-cons-threshold most-positive-fixnum))
  (load-file (expand-file-name "mookid-init.el" mookid-root-dir)))

(provide 'init)
;;; init.el ends here
