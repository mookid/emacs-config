;;; mookid-evil-surround.el --- Configuration for evil-surround

;;; Commentary:

;;; Code:
(require 'evil-surround)

(global-evil-surround-mode 1)
(setq-default evil-surround-pairs-alist
	      ((40 "( " . " )")
	       (41 "(" . ")")
	       (123 "{ " . " }")
	       (125 "{" . "}")
	       (91 "[ " . " ]")
	       (93 "[" . "]")
	       (35 "#{" . "}")
	       (98 "(" . ")")
	       (66 "{" . "}")
	       (62 "<" . ">")
	       (116 . evil-surround-read-tag)
	       (60 . evil-surround-read-tag)
	       (102 . evil-surround-function))


(provide 'mookid-evil-surround)
;;; mookid-evil-surround.el ends here
