;;; mookid-mouse.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of mouse actions.

;;; Code:
(require 'mouse-copy)
(global-set-key (kbd "<C-down-mouse-1>") 'mouse-drag-secondary-pasting)
(global-set-key (kbd "<C-S-down-mouse-1>") 'mouse-drag-secondary-moving)
(global-set-key (kbd "<S-mouse-1>") 'mouse-set-mark)

(setq mouse-drag-copy-region t)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(provide 'mookid-mouse)
;;; mookid-mouse.el ends here
