;;; mookid-popwin.el --- Configuration for popwin

;;; Commentary:

;;; Code:
(require 'popwin)
(popwin-mode 1)
(remove '*vc-diff* popwin:special-display-config)
(provide 'mookid-popwin)
;;; mookid-popwin.el ends here
