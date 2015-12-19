;;; mookid-find-file-in-project --- Configuration for find-file-in-project

;;; Commentary:
;; To load when using ivy.

;;; Code:
(require 'find-file-in-project)
(global-set-key (kbd "<f5>") 'find-file-in-project)
(global-set-key (kbd "C-<f5>") 'find-file-in-project-by-selected)

(provide 'mookid-find-file-in-project)
;;; mookid-find-file-in-project.el ends here
