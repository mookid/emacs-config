;;; mookid-highlight-persist.el --- Config of evil-search-highlight-persist

;;; Commentary:

;;; Code:
(with-eval-after-load 'evil
  (require 'evil-search-highlight-persist)
  (mapc (lambda (face)
	  (set-face-attribute face nil
			      :weight 'extra-bold
			      :foreground "White"
			      :background "RosyBrown"))
	'(evil-search-highlight-persist-highlight-face
	  isearch
	  lazy-highlight))
  (global-evil-search-highlight-persist t))

(provide 'mookid-highlight-persist)
;;; mookid-highlight-persist.el ends here
