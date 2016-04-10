;;; mookid-lispy.el --- Configuration for lispy -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'lispy)

(lispy-set-key-theme '(special))
(add-hook 'prog-mode-hook 'lispy-mode)

(provide 'mookid-lispy)
;;; mookid-lispy.el ends here
