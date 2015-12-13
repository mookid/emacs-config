;;; mookid-company --- Configuration for company mode

;;; Commentary:

;;; Code:
(require 'company)
(setq-default company-idle-delay 0.5)
(setq-default company-tooltip-limit 5)
(setq-default company-minimum-prefix-length 2)
(setq-default company-tooltip-flip-when-above t)

(provide 'mookid-company)
;;; mookid-company.el ends here
