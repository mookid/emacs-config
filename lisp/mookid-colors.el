;;; mookid-colors.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of Emacs colors.

;;; Code:

(defun mookid-remap-attribute (attribute color &optional only-default eql-pred)
  "Change every mapping of the face ATTRIBUTE to COLOR.

When ONLY-DEFAULT is
* 'rebind: change only values associated to the default face value
* 'all:    change every face

Equality test is done with EQL-PRED.

See `set-face-attribute' for legal ATTRIBUTE values."
  (let ((default-val (face-attribute 'default attribute))
        (eql-pred (or eql-pred #'string=))
        (only-default (or only-default 'rebind)))
    (mapc (lambda (face)
            (when (or (eql 'all only-default)
                      (funcall eql-pred
                               (face-attribute face attribute)
                               default-val))
              (set-face-attribute face nil attribute color)))
          (face-list))))

(defun mookid-faces-fix (&optional frame)
  "Remove undesired faces properties."
  (interactive)
  (dolist (face (face-list))
    (when (face-bold-p face frame)
      (set-face-attribute face nil
                          :weight 'normal
                          :underline t)
      (set-face-bold face nil frame)
      (set-face-underline face t frame))))

(add-to-list 'custom-define-hook #'mookid-faces-fix)

(defun mookid-colors-wombat ()
  "Settings for the wombat color theme."
  (interactive)
  (load-theme 'wombat)
  (defvar *default-light-color* nil "Foreground for most faces.")
  (mookid-remap-attribute :foreground "cornsilk1")

  (set-face-attribute 'error nil :foreground "deep pink")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "white")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "white")
  (set-face-attribute 'font-lock-type-face nil :foreground "white")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "white")
  (set-face-attribute 'font-lock-string-face nil :foreground "pale green")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "lavender")
  (set-face-attribute 'font-lock-constant-face nil :foreground "light sky blue"))

(defun mookid-colors-plan9 ()
  "Settings for the plan9 theme."
  (interactive)
  (load-theme 'plan9 t)
  (set-foreground-color "DarkGrey")
  (mookid-remap-attribute :weight 'light 'all))

(defun mookid-colors-leuven ()
  "Settings for the leuven color theme."
  (interactive)
  (load-theme 'leuven t)
  (mookid-remap-attribute :background "#FFFFE8")
  (mookid-remap-attribute :weight 'light 'all))

(defun mookid-colors-spacemacs (light-p)
  "Settings for the spacemacs themes. VERSION is 'light or 'dark."
  (interactive "r")
  (let ((theme (if light-p
                   'spacemacs-light
                 'spacemacs-dark)))
    (load-theme theme t))
  (global-hl-line-mode 1))

(defun jurta-colors-dark ()
  "Set colors suitable for working in the darkness without electricity."
  (interactive)
  (setq frame-background-mode 'dark)
  (set-background-color "black")
  (set-foreground-color "DarkGrey")
  (set-face-background 'region "DimGray")
  (set-face-background 'fringe (face-background 'default))
  (set-face-foreground 'fringe (face-foreground 'default)))

(defun mookid-colors-nocolor ()
  "Settings for a custom colorless theme."
  (interactive)
  (setq frame-background-mode 'dark)
  (set-background-color "grey10")
  (set-foreground-color "dark grey")
  (mapc (lambda (face)
          (when (string-prefix-p "font-lock" (face-name face))
            (set-face-attribute face nil
                                :foreground nil)))
        (face-list))
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "dark slate gray"
                      :background nil)
  (set-face-attribute 'font-lock-doc-face nil
                      :foreground "tomato"
                      :background nil)
  (set-face-attribute 'region nil
                        :foreground "cornsilk1"
                        :background "dim gray")
  (set-face-attribute 'error nil
                        :foreground "red"
                        :background nil)
  (set-face-attribute 'highlight nil
                        :foreground "cornsilk1"
                        :background "dim gray"))

(mookid-colors-nocolor)

(provide 'mookid-colors)
;;; mookid-colors.el ends here
