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
  (mookid-remap-attribute :weight 'light 'all))

(defun mookid-colors-leuven ()
  "Settings for the leuven color theme."
  (interactive)
  (load-theme 'leuven t)
  (mookid-remap-attribute :background "#FFFFE8")
  (mookid-remap-attribute :weight 'light 'all))

(defun mookid-colors-spacemacs (version)
  "Settings for the spacemacs themes. VERSION is 'light or 'dark."
  (interactive)
  (let ((theme (cond ((eql version 'light)
                      'spacemacs-light)
                     ((eql version 'dark)
                      'spacemacs-dark)
                     (t
                      (error "usage: `version' should be 'light or 'dark")))))
    (load-theme theme t))
  (hl-line-mode 1))

(mookid-colors-spacemacs 'light)

(provide 'mookid-colors)
;;; mookid-colors.el ends here
