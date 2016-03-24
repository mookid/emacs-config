;;; mookid-evil-highlight-persist.el --- Config of evil-search-highlight-persist

;;; Commentary:

;;; Code:
(with-eval-after-load 'evil
  (require 'evil-search-highlight-persist)

  (face-rem)
  (setq evil-search-highlight-persist-highlight-face nil)

  (global-evil-search-highlight-persist t)
  (add-hook 'isearch-mode-end-hook
	    'evil-search-highlight-persist-remo
