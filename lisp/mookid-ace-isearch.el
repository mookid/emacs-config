;;; mookid-ace-isearch --- Configuration for ace-isearch

;;; Commentary:

;;; Code:
(require 'ace-isearch)

(global-ace-isearch-mode +1)
(setq-default ace-isearch-function 'avy-goto-char)

(provide 'mookid-ace-isearch)
;;; mookid-ace-isearch.el ends here
