;;; mookid-powerline --- Powerline configuration
;;; Commentary:
;;; Code:
(require 'smart-mode-line-powerline-theme)

(setq-default sml/no-confirm-load-theme t) ; avoids a question at every startup
(setq-default sml/theme 'powerline)
(sml/setup)

(provide 'mookid-powerline)
;;; mookid-powerline.el ends here
