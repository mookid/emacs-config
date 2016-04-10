;;; mookid-lispy.el --- Configuration for lispy -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'lispy)

(defun lispy-on () "Turn lispy mode on." (lispy-mode 1))

(lispy-set-key-theme '(special))
(add-hook 'prog-mode-hook 'lispy-on)

(provide 'mookid-lispy)
;;; mookid-lispy.el ends here
