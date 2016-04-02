;;; mookid-popwin.el --- Configuration for popwin

;;; Commentary:

;;; Code:
(require 'popwin)
(popwin-mode 1)

(setq popwin:popup-window-position 'right)
(setq popwin:adjust-other-windows nil)
(setq popwin:popup-window-width 60)

(provide 'mookid-popwin)
;;; mookid-popwin.el ends here
