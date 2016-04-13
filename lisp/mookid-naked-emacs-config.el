;;; mookid-naked-emacs-config.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of Emacs that does not depend on external packages.

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'mookid-root-dir "mookid-values")

;; Remove silly message
(defun display-startup-echo-area-message () "Inhibit welcome message." ())

;; Start with a blank scratch buffer
(setq initial-scratch-message nil)

(define-key global-map (kbd "M-r") 'raise-sexp)

(defun mookid-last-2 (list)
  "Remove all the elements of LIST except the last two."
  (let ((lst list))
    (while (cl-caddr lst)
      (setq lst (cdr lst)))
    lst))

(require 'subr-x)
(defun mookid-shorten-path (path)
  "Shortens the string representing a PATH for the modeline."
  (let ((r (string-join (cons "..." (mookid-last-2 (split-string path "/"))) "/")))
    (if (< (length r) (length path)) r path)))

(with-message
 "Define zap-up-to-char"
 (autoload 'zap-up-to-char "misc")
 (define-key global-map (kbd "M-z") 'zap-up-to-char))

(with-message
 "Set mode line format"
 (make-face 'mode-line-folder-face)
 (make-face 'mode-line-filename-face)
 (set-face-attribute 'mode-line-filename-face nil :weight 'bold)
 (setq-default mode-line-format
               (list
                "  "
                mode-line-position
                '(:propertize
                  (:eval (when buffer-file-name
                           (mookid-shorten-path default-directory)))
                  face mode-line-folder-face)
                '(:propertize "%b" face mode-line-filename-face)
                "%n  "
                mode-line-modes
                mode-line-misc-info
                "%-")))

;; Disable the bell
(setq ring-bell-function 'ignore)

;; Default behaviour for newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; No welcome message
(setq inhibit-startup-message t)

;; Move backup files to a subdirectory of the root directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" mookid-root-dir))))

;; Stop auto save
(setq auto-save-default nil)

;; Auto revert
(global-auto-revert-mode 1)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; No tabs
(setq indent-tabs-mode nil)
(defun mookid-untabify-all ()
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook #'mookid-untabify-all)

;; Delete trailing whitespaces when saving a file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable region narrowing
(put 'narrow-to-region 'disabled nil)

;; Save all buffers when focus is lost
(defun mookid-save-all-buffers () "Save all buffers." (save-some-buffers t))
(add-hook 'focus-out-hook 'mookid-save-all-buffers)

(require 'face-remap)
;; Use proportional fonts
(define-globalized-minor-mode
  mookid-prop-fonts-mode
  buffer-face-mode
  (lambda () (variable-pitch-mode 1)))

(diminish 'buffer-face-mode)

(defun mookid-prop-fonts-mode-off ()
  "Turn it off."
  (variable-pitch-mode -1))

(mookid-prop-fonts-mode 1)

;; Disable them for specific modes
(add-hook 'dired-mode-hook 'mookid-prop-fonts-mode-off)
(add-hook 'ibuffer-mode-hook 'mookid-prop-fonts-mode-off)
(add-hook 'package-menu-mode-hook 'mookid-prop-fonts-mode-off)
(add-hook 'help-mode-hook 'mookid-prop-fonts-mode-off)

(with-message
 "Remove gui elements"
 (and (fboundp 'fringe-mode) (fringe-mode -1))
 (and (fboundp 'tooltip-mode) (tooltip-mode -1))
 (and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
 (and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
 (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
 (and (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
 (setq pop-up-windows nil))


;; Save history between sessions
(setq-default savehist-file
              (expand-file-name "savehist" mookid-root-dir))
(savehist-mode t)
(setq history-length 16384)
(setq history-delete-duplicates t)
(setq-default savehist-save-minibuffer-history t)
(setq-default savehist-additional-variables
              '(kill-ring search-ring regexp-search-ring))

(with-message
 "Configuring parenthesis settings"
 (electric-pair-mode t)
 (setq-default electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 (set-face-background 'show-paren-match "turquoise")
 (setq-default show-paren-delay 0))

(with-message
 "Setting up fonts"
 (defvar mookid-default-font nil "The font used almost everywhere.")
 (setq mookid-default-font "Source Code Pro Light")
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist `(font . ,mookid-default-font))
 (global-prettify-symbols-mode 1))

(with-message
 "Customize proportional font"
 (set-face-attribute 'variable-pitch nil
                     :family "DejaVu Sans"))

;; Keyboard translations
(with-message
 "Keyboard translations"
 (keyboard-translate ?\( ?\[)
 (keyboard-translate ?\[ ?\()
 (keyboard-translate ?\) ?\])
 (keyboard-translate ?\] ?\))
 (keyboard-translate ?\C-h ?\C-p)
 (keyboard-translate ?\C-p ?\C-h))

(with-message
 "Window switch bindings"
 ;; unbind all keybindings starting with f2
 (cl-dolist (keystr '("<f2> 2" "<f2> b" "<f2> s"))
   (global-unset-key (kbd keystr)))

 (with-eval-after-load 'init
   (define-key global-map (kbd "<f2> <f2>") 'mookid-toggle-window-split))

 (defun mookid-toggle-window-split ()
   "When there are two windows, convert horizontal to vertical and vice versa."
   (interactive)
   (or (= (count-windows) 2)
       (error "You need exactly 2 windows to do this"))
   (let* ((this-win-buffer (window-buffer))
          (next-win-buffer (window-buffer (next-window)))
          (this-win-edges (window-edges (selected-window)))
          (next-win-edges (window-edges (next-window)))
          (this-win-2nd (not (and (<= (car this-win-edges)
                                      (car next-win-edges))
                                  (<= (cadr this-win-edges)
                                      (cadr next-win-edges)))))
          (splitter
           (if (= (car this-win-edges)
                  (car next-win-edges))
               'split-window-horizontally
             'split-window-vertically)))
     (delete-other-windows)
     (let ((first-win (selected-window)))
       (funcall splitter)
       (when this-win-2nd (other-window 1))
       (set-window-buffer (selected-window) this-win-buffer)
       (set-window-buffer (next-window) next-win-buffer)
       (select-window first-win)
       (when this-win-2nd (other-window 1))))))

(with-message
 "Setting up the order for recenter-top-bottom"
 (setq recenter-positions '(top middle bottom)))

;; Mark dired-ignored
(with-eval-after-load 'dired
  (set-face-attribute 'dired-ignored nil
                      :strike-through t
                      :foreground "green"))

;; Wrap long lines
(global-visual-line-mode 1)
(require 'diminish)
(diminish 'visual-line-mode)

;; Use ibuffer
(require 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(require 'fullframe)
(fullframe ibuffer ibuffer-quit)

;; Use fullframe the packages
(fullframe list-packages quit-window)

(winner-mode 1)

;; Run Cygwin shell
(setq-default explicit-shell-file-name "C:/bin/bash")

(defun mookid-previous-buffer ()
  "Not the current buffer but the buffer before."
  (other-buffer (current-buffer) 1))

(defun mookid-insert-buffer-name ()
  "Insert the previous buffer name.  Useful for compilation."
  (interactive)
  (insert (buffer-name (mookid-previous-buffer))))
(define-key global-map (kbd "C-c C-v") 'mookid-insert-buffer-name)

(defun mookid-insert-buffer-path (arg)
  (interactive "P")
  "Insert the previous buffer path.

With a prefix argument, insert `file:' before."
  (interactive)
  (insert (concat (if arg "file:" "")
                  (buffer-file-name (mookid-previous-buffer)))))

;; from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html:
(defun mookid-narrow-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first.  Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ;; ((derived-mode-p 'org-mode)
        ;;  ;; `org-edit-src-code' is not a real narrowing
        ;;  ;; command. Remove this first conditional if you
        ;;  ;; don't want it.
        ;;  (cond ((ignore-errors (org-edit-src-code))
        ;;         (delete-other-windows))
        ;;        ((ignore-errors (org-narrow-to-block) t))
        ;;        (t (org-narrow-to-subtree))))
        ;; ((derived-mode-p 'latex-mode)
        ;;  (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(define-key global-map (kbd "C-x n n") 'mookid-narrow-dwim)

(defun mookid-join-line (beg end)
  "If the range BEG-END is active, group it on one line.
Otherwise, join the current line with the following."
  (interactive "r")
  (cond ((null mark-active)
         (delete-indentation 1))
        (t (if mark-active
               (let ((beg (region-beginning))
                     (end (copy-marker (region-end))))
                 (goto-char beg)
                 (while (< (point) end)
                   (join-line 1)))))))
(define-key global-map (kbd "M-j") 'mookid-join-line)

(advice-add 'transpose-chars :before #'forward-char)
(advice-add 'transpose-chars :after #'backward-char)

(advice-add 'transpose-lines :before #'forward-line)
(advice-add 'transpose-lines :after #'backward-line)

(advice-add 'transpose-sexps :before #'forward-sexp)
(advice-add 'transpose-sexps :after #'backward-sexp)

;; Swap both keybindings
(define-key global-map (kbd "C-t") #'transpose-lines)
(define-key ctl-x-map (kbd "C-t") #'transpose-chars)

(provide 'mookid-naked-emacs-config)
;;; mookid-naked-emacs-config.el ends here
