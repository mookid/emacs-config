;;; mookid-highlight-column --- Configuration for highlight column

;;; Commentary:

;;; Code:
(require 'highlight-indentation)

(set-face-background 'highlight-indentation-current-column-face
                     "light steel blue")
(add-hook 'prog-mode-hook
          'highlight-indentation-current-column-mode)

(provide 'mookid-highlight-column)
;;; mookid-highlight-column.el ends here
