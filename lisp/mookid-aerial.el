;;; mookid-aerial.el --- Custom theme

;;; Commentary:
;; Based on https://github.com/rougier/aerial-emacs/blob/master/.emacs

;;; Code:
;; -----------------------------------------------------------------------------
;; Aerial Emacs
;; Author: Nicolas P. Rougier
;; Licence: GNU GPL v3
;; -----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Headerline
;; ----------------------------------------------------------------------------
(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(setq-default header-line-format (list
   " "
   'mode-line-mule-info
   'mode-line-modified
   "  "
   'mode-line-buffer-identification
   'mode-line-modes
   " -- "
   `(vc-mode vc-mode)

   ;; File read-only
   '(:eval (if buffer-read-only
               (list (mode-line-fill 'nil 13)
                     (propertize " [read-only] " 'face 'header-line-grey))))

   ;; File modified
   '(:eval (if (buffer-modified-p)
               (list (mode-line-fill 'nil 12)
                     (propertize " [modified] " 'face 'header-line-red))
             (list (mode-line-fill 'nil 9)
                   (propertize "%4l:%3c " 'face 'header-line))))
   ))
(setq-default mode-line-format "")


;; ----------------------------------------------------------------------------
;; Colors
;; -----------------------------------------------------------------------------
(setq frame-background-mode 'light)
(set-foreground-color "#000000")
(set-background-color "#ffffff")

(set-face-attribute 'bold nil
                    :weight 'regular)

(make-face 'header-line-grey)
(set-face-attribute 'header-line-grey nil
                    :weight 'medium
                    :foreground "#ffffff"
                    :background "#999999"
                    :box '(:line-width 1 :color "#999999"))
(make-face 'header-line-red)
(set-face-attribute 'header-line-red nil
                    :weight 'medium
                    :foreground "white"
                    :background "#dd7777"
                    :box '(:line-width 1 :color "#dd7777"))

(set-face-attribute 'mode-line nil
                    :height 10
                    :background "#999"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :height 10
                    :background "#999"
                    :box nil)
(set-face-attribute 'header-line nil
                    :inherit nil
                    :foreground "white"
                    :background "#000000"
                    :box '(:line-width 3 :color "#000000"))

(set-face-attribute 'fringe  nil
                    :inherit nil
                    :background "#ffffff")
(set-face-attribute 'show-paren-match nil
                    :bold t
                    :background "#dddddd")

(set-face-attribute 'highlight nil
                    :background "#f5f5ff")
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#8c878f")
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#3b5bb5")
(set-face-attribute 'font-lock-string-face nil
                    :foreground "#0078ff")
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#3b5bb5")
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "black")
(set-face-attribute 'font-lock-type-face nil
                    :foreground "#3b5bb5")
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground "#ff7800")
(set-face-attribute 'region nil
                    :stipple nil
                    :background "#ffffcc")
(set-face-attribute 'isearch nil
                    :foreground "#000000"
                    :background "#ffff00")
(set-face-attribute 'lazy-highlight nil
                    :background "paleturquoise")
