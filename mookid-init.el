;;; mookid-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(require 'cl-lib)


;;; Macros

(defmacro with-title (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "[%s]" ,msg) ,@body (message "[end]") 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defmacro with-message (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "*** %s" ,msg) ,@body 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defmacro mookid-ignore (&rest _body)
  "Ignore the arguments.  Use it to enable a part of the file."
  nil)


;;; Basic configuration

(defvar mookid-root-dir "~/.emacs.d" "The root directory of the configuration.")

(defun display-startup-echo-area-message () "Inhibit welcome message." ())

(setq initial-scratch-message nil)

(define-key global-map (kbd "M-r") 'raise-sexp)

(defun mookid-last-2 (list)
  "Remove all the elements of LIST except the last two."
  (let ((lst list))
    (while (cl-caddr lst)
      (setq lst (cdr lst)))
    lst))

(advice-add 'move-beginning-of-line :before #'back-to-indentation)

(require 'subr-x)
(defun mookid-shorten-path (path)
  "Shortens the string representing a PATH for the modeline."
  (let ((r (string-join (cons "..." (mookid-last-2 (split-string path "/"))) "/")))
    (if (< (length r) (length path)) r path)))

(autoload 'zap-up-to-char "misc")
(define-key global-map (kbd "M-z") 'zap-up-to-char)

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

;; Enable originally disabled functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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
  "Untabify the current buffer, except if it is a Makefile.

\(BROKEN)."
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
(add-hook 'focus-out-hook #'mookid-save-all-buffers)

(require 'face-remap)
;; Use proportional fonts
(define-globalized-minor-mode
  mookid-prop-fonts-mode
  buffer-face-mode
  #'mookid-prop-fonts-mode-on)

(with-eval-after-load 'diminish
  (diminish 'buffer-face-mode))

(defun mookid-prop-fonts-mode-off ()
  "Turn `mookid-prop-fonts-mode' off."
  (variable-pitch-mode -1))

(defun mookid-prop-fonts-mode-on ()
  "Turn `mookid-prop-fonts-mode' on."
  (variable-pitch-mode +1))

;; Disable them for specific modes
(add-hook 'dired-mode-hook 'mookid-prop-fonts-mode-off)
(add-hook 'ibuffer-mode-hook 'mookid-prop-fonts-mode-off)
(add-hook 'package-menu-mode-hook 'mookid-prop-fonts-mode-off)
(add-hook 'help-mode-hook 'mookid-prop-fonts-mode-off)

(with-message
 "Remove gui elements"
 (and (fboundp 'fringe-mode) (fringe-mode -1))
 (and (fboundp 'tooltip-mode) (tooltip-mode +1))
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
 (setq mookid-default-font "Source Code Pro Light 10")
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist `(font . ,mookid-default-font))
 (global-prettify-symbols-mode 1))

(with-message
 "Customize proportional font"
 (set-face-attribute 'variable-pitch nil
                     :family "DejaVu Sans"))

(with-message
 "Keyboard translations"
 (keyboard-translate ?\( ?\[)
 (keyboard-translate ?\[ ?\()
 (keyboard-translate ?\) ?\])
 (keyboard-translate ?\] ?\)))

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

;; Pop mark
(setq set-mark-command-repeat-pop t)

;; Mark dired-ignored
(with-eval-after-load 'dired
  (set-face-attribute 'dired-ignored nil
                      :strike-through t
                      :foreground "green"))

;; Wrap long lines
(global-visual-line-mode 1)
(define-key visual-line-mode-map [remap kill-line] nil)
(define-key visual-line-mode-map [remap move-beginning-of-line] nil)
(define-key visual-line-mode-map [remap move-end-of-line] nil)
(with-eval-after-load 'diminish
  (diminish 'visual-line-mode))

;; Use ibuffer
(require 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load 'fullframe
  (fullframe ibuffer ibuffer-quit))

;; Use fullframe
(with-eval-after-load 'fullframe
  (fullframe list-packages quit-window))

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
  "Insert the previous buffer path.

With a prefix argument ARG, insert `file:' before."
  (interactive "P")
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
  "If the range BEG END is active, group it on one line.
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

(advice-add 'transpose-lines :before #'forward-line)
(advice-add 'transpose-sexps :before #'forward-sexp)

;; Swap keybindings
(define-key global-map (kbd "C-t") 'transpose-lines)
(define-key global-map (kbd "C-x C-t") 'transpose-chars)

;; Stop exiting with the keyboard
(global-unset-key (kbd "C-x C-c"))

(define-key global-map (kbd "M-g a") 'align-regexp)

(define-key global-map (kbd "<M-left>") 'previous-buffer)
(define-key global-map (kbd "<M-right>") 'next-buffer)

;; Display page delimiter as a horizontal line
(aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))

(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "C-M-v") 'yank-pop)

(define-key global-map (kbd "M-DEL") 'kill-whole-line)
(advice-add 'kill-whole-line :before #'append-next-kill)


;;; Colors

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
  "Remove undesired faces properties in FRAME."
  (interactive)
  (dolist (face (face-list))
    (when (face-bold-p face frame)
      (set-face-attribute face nil
                          :weight 'normal
                          :underline t)
      (set-face-bold face nil frame)
      (set-face-underline face t frame))))

;; (add-to-list 'custom-define-hook #'mookid-faces-fix)

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
  (mookid-remap-attribute :background "#fafdf7")
  (mookid-remap-attribute :weight 'light 'all))

(defun mookid-colors-spacemacs (light)
  "Settings for the spacemacs themes, LIGHT and dark versions."
  (interactive "r")
  (load-theme (if light 'spacemacs-light 'spacemacs-dark) t)
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
  (global-hl-line-mode 1)
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

(mookid-colors-leuven)

;;; Dired
(require 'dired)
(require 'dired-x)

(define-key dired-mode-map (kbd "M-<left>") 'dired-jump)
(define-key dired-mode-map (kbd "M-<right>") 'dired-find-file)

(define-key global-map (kbd "C-S-<left>") 'dired-jump)


;;; Compilation
(setq compilation-ask-about-save nil)
(setq-default compilation-always-kill t)
(setq-default compilation-scroll-output 'first-error)

;; disable it for grep mode:
(defun mookid-disable-jump-to-error ()
  (kill-local-variable 'compilation-auto-jump-to-next))
(add-hook 'grep-mode-hook 'mookid-disable-jump-to-error)
(define-key global-map (kbd "<f12>") 'recompile)
(define-key global-map (kbd "C-<prior>") 'previous-error)
(define-key global-map (kbd "C-<next>") 'next-error)


;;; Mouse

(require 'mouse)
(global-unset-key (kbd "<S-down-mouse-1>"))
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-mark)
(setq mouse-drag-copy-region t)
(setq mouse-yank-at-point t)

(setq mouse-autoselect-window t)

(require 'mouse-copy)
(define-key global-map (kbd "<C-down-mouse-1>") 'mouse-drag-secondary-pasting)
(define-key global-map (kbd "<C-S-down-mouse-1>") 'mouse-drag-secondary-moving)

(setq mouse-drag-copy-region t)
(define-key global-map (kbd "<C-wheel-up>") 'text-scale-increase)
(define-key global-map (kbd "<C-wheel-down>") 'text-scale-decrease)

(define-key global-map (kbd "<mode-line> <down-mouse-1>") 'enlarge-window)

(global-unset-key (kbd "<mode-line> <mouse-2>"))
(define-key global-map (kbd "<mode-line> <down-mouse-2>") 'delete-other-windows-vertically)

(require 'mouse-drag)
(defun mookid-mouse-drag-throw (start-event)
  "Similar to `mouse-drag-throw' but only vertically.

To test this function, evaluate:
    (define-key global-map [down-mouse-2] \\='mookid-mouse-drag-throw)"
  (interactive "e")
  (save-selected-window
    (let* ((start-posn (event-start start-event))
           (start-window (posn-window start-posn))
           (start-row (cdr (posn-col-row start-posn)))
           (start-col (car (posn-col-row start-posn)))
           event end row scroll-delta
           have-scrolled
           col
           (scroll-col-delta 0))
      (select-window start-window)
      (track-mouse
        ;; Don't change the mouse pointer shape while we drag.
        (setq track-mouse 'dragging)
        (while (progn
                 (setq event (read-event)
                       end (event-end event)
                       row (cdr (posn-col-row end))
                       col (car (posn-col-row end)))
                 (or (mouse-movement-p event)
                     (eq (car-safe event) 'switch-frame)))
          (when (eq start-window (posn-window end))
            (setq scroll-delta (mouse-drag-scroll-delta (- start-row row))))

          (if (or (/= 0 scroll-delta)
                  (/= 0 scroll-col-delta))
              (progn
                (setq have-scrolled t)
                (mouse-drag-safe-scroll scroll-delta scroll-col-delta)
                (mouse-drag-repeatedly-safe-scroll scroll-delta scroll-col-delta))))) ;xxx
      ;; If it was a click and not a drag, prepare to pass the event on.
      ;; Is there a more correct way to reconstruct the event?
      (if (and (not have-scrolled)
               (mouse-drag-events-are-point-events-p start-posn end))
          (push (cons (event-basic-type start-event) (cdr start-event))
                unread-command-events)))))


;;; Selective display
(let ((depth 1))
  (define-key global-map (kbd "<f6>") 'mookid-selective-display-toggle)
  (define-key global-map (kbd "C-<f6>") 'mookid-selective-display-increase)
  (define-key global-map (kbd "S-<f6>") 'mookid-selective-display-decrease)

  (defun mookid-selective-display-toggle ()
    "Hide lines starting with a lot of spaces.

See `mookid-selective-display-increase' to increase the number of spaces.
See `mookid-selective-display-decrease' to decrease it."
    (interactive)
    (set-selective-display (unless selective-display depth)))
  (cl-flet ((g (offset)
               (setq depth (+ depth offset))
               (set-selective-display depth)))
    (defun mookid-selective-display-increase ()
      "Increase the cap for `toogle-selective-display'.

See `mookid-selective-display-toggle' and `mookid-selective-display-decrease'."
      (interactive)
      (when (< depth 20) (g 1)))

    (defun mookid-selective-display-decrease ()
      "Decrease the cap for `toogle-selective-display'.

See `mookid-selective-display-toggle' and `mookid-selective-display-increase'."
      (interactive)
      (when (> depth 1) (g -1)))))


;;; Isearch
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map (kbd "C-<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)

(define-key isearch-mode-map (kbd "M-<") 'mookid-isearch-beginning-of-buffer)
(define-key isearch-mode-map (kbd "M->") 'mookid-isearch-end-of-buffer)
(define-key global-map (kbd "C-M-s") 'mookid-isearch-region)

(define-key isearch-mode-map (kbd "<S-return>") 'mookid-isearch-exit-leave-hl)

(defun mookid-isearch-exit-leave-hl ()
  "Exit search and leave extra match highlighting."
  (interactive)
  (let ((lazy-highlight-cleanup nil))
    (when isearch-lazy-highlight
      (isearch-lazy-highlight-new-loop (point-min) (point-max)))
    (isearch-exit)))

(defun mookid-isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun mookid-isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

(defadvice isearch-repeat (after isearch-no-fail activate)
  "Wrap isearch without failing."
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

;; Exit isearch at the beginning of the matching string
(add-hook 'isearch-mode-end-hook #'mookid-isearch-exit-beginning)
(defun mookid-isearch-exit-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defun mookid-isearch-region (beg end)
  "Send selection between BEG and END to isearch."
  (interactive "r")
  (deactivate-mark)
  (kill-ring-save beg end)
  (isearch-mode t nil nil nil)
  (isearch-yank-pop))

(defun mookid-occur-rename-buffer ()
  "Used to uniquify the occur buffer names."
  (occur-rename-buffer t))
(add-hook 'occur-hook #'mookid-occur-rename-buffer)


;;; Melpa packages

(require 'fullframe)
(require 'diminish)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(with-message
 "Nerd commenter"
 (require 'evil-nerd-commenter)
 (define-key global-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
 (define-key global-map (kbd "C-c c") 'evilnc-copy-and-comment-lines))

(with-message
 "Anzu"
 (require 'anzu)
 (diminish 'anzu-mode)
 (global-anzu-mode +1)
 (global-set-key (kbd "M-%") 'anzu-query-replace-regexp))

(with-message
 "Lispy"
 (require 'lispy)
 (lispy-set-key-theme '(special))
 (add-hook 'prog-mode-hook 'lispy-mode))

(with-message
 "Magit"
 (autoload 'magit-status "magit")
 (autoload 'magit-mode-quit-window "magit")
 (global-set-key (kbd "<f7>") 'magit-status)
 (fullframe magit-status magit-mode-quit-window))

(with-message
 "Rainbow modes"
 (require 'rainbow-delimiters)
 (require 'rainbow-blocks)
 (autoload 'mookid-default-font "mookid-naked-emacs-config")
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
 (defun mookid-rainbow-delimiters-disable ()
   "Disable rainbow-delimiters mode."
   (rainbow-delimiters-mode -1))

 (add-hook 'shell-mode-hook #'mookid-rainbow-disable)
 (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                     :foreground "red"
                     :inherit 'error
                     :box t)
 (let ((colors '("green3" "orange" "pale violet red"))
       (kinds '(blocks)))
   (cl-labels ((set-bold (face color)
                         (set-face-attribute face nil
                                             :foreground color))
               (symb (kind lvl)
                     (intern (format "rainbow-%S-depth-%S-face" kind lvl)))
               (set-level (lvl color)
                          (when (< 0 lvl 10)
                            (mapc (lambda (kind)
                                    (set-bold (symb kind lvl) color))
                                  kinds))))
     (cl-loop
      with ncolors = (length colors)
      for lvl from 1 upto 9
      for icolor = (mod (- lvl 1) ncolors)
      do (set-level lvl (nth icolor colors)))))
 (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "green3")
 )

(with-message
 "Company"
 (require 'company)
 (setq-default company-idle-delay 0.5)
 (setq-default company-tooltip-limit 5)
 (setq-default company-minimum-prefix-length 2)
 (setq-default company-tooltip-flip-when-above t))

(with-message
 "Elisp-slime-nav"
 (require 'elisp-slime-nav)
 (diminish 'elisp-slime-nav-mode)

 (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
 (add-hook 'lisp-interaction-mode 'elisp-slime-nav-mode)

 (define-key global-map (kbd "C-\"") 'elisp-slime-nav-find-elisp-thing-at-point))

(with-title
 "Ivy configuration"
 (require 'ivy)
 (diminish 'ivy-mode)

 (ivy-mode 1)
 (setq-default ivy-use-virtual-buffers t)

 (require 'counsel)
 (defvar ivy-minibuffer-map)
 (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
 (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)

 (define-key global-map (kbd "C-M-y") 'counsel-yank-pop)

 (define-key global-map (kbd "M-x") 'counsel-M-x)
 (define-key global-map (kbd "C-x <return>") 'counsel-M-x)
 (define-key global-map (kbd "M-m") 'counsel-M-x)

 (define-key global-map (kbd "M-s p") 'swiper)
 (define-key isearch-mode-map (kbd "M-s p") 'swiper-from-isearch)

 (define-key global-map (kbd "<M-return>") 'ivy-switch-buffer)
 (define-key global-map (kbd "<C-return>") 'counsel-find-file)

 (defvar grep-mode-map)
 (add-hook 'grep-setup-hook
           (lambda () (define-key grep-mode-map (kbd "RET") 'ivy-switch-buffer)))

 (require 'projectile)

 (projectile-global-mode)
 (setq-default projectile-indexing-method 'native)
 (setq-default projectile-enable-caching t)
 (setq projectile-mode-line '(:eval (concat " <" (projectile-project-name) ">")))
 (defun mookid-projectile (p)
   "My projectile command.

If P is non nil, call `projectile-find-file' else call `projectile-switch-project'."
   (interactive "P")
   (if p (projectile-switch-project) (projectile-find-file)))
 (global-set-key (kbd "<C-S-return>") 'mookid-projectile)

 (setq-default projectile-completion-system 'ivy))

(with-message
 "Slime"
 (defvar slime-mode-map nil)
 (require 'slime-autoloads)
 (setq-default inferior-lisp-program "sbcl")
 (add-hook 'comint-mode-hook 'rainbow-delimiters-mode)
 (setq-default common-lisp-hyperspec-root "file:///Hyperspec/")
 (with-eval-after-load 'slime
   (define-key slime-mode-map (kbd "C-c h") 'hyperspec-lookup)))

(with-message
 "Expand region"
 (require 'expand-region)
 (global-set-key (kbd "M-`") 'er/expand-region))

(with-message
 "Avy"
 (require 'avy)
 (setq-default avy-all-windows 'all-frames)
 (define-key global-map (kbd "C-:") 'avy-goto-word-or-subword-1))

(with-message
 "Ace window"
 (require 'ace-window)
 (defun mookid-other-window ()
   "Forwards to `other-window'."
   (interactive)
   (other-window 1))

 (define-key global-map (kbd "M-o") 'ace-window)
 (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
 (setq-default aw-dispatch-always t)
 (defvar aw-dispatch-alist)
 (add-to-list 'aw-dispatch-alist '(?v mookid-split-window-right))
 (add-to-list 'aw-dispatch-alist '(?p mookid-other-window))
 (add-to-list 'aw-dispatch-alist '(?b mookid-split-window-below))
 (add-to-list 'aw-dispatch-alist '(?c mookid-delete-window))

 ;; Auto balance windows:
 (defun mookid-split-window-right ()
   "Forwards to `split-window-below' and rebalances."
   (split-window-right))

 (defun mookid-split-window-below ()
   "Forwards to `split-window-below' and rebalances."
   (split-window-below))

 (defun mookid-delete-window ()
   "Forwards to `delete-window' and rebalances."
   (delete-window))

 (define-key global-map (kbd "C-x 0") 'mookid-delete-window)
 (define-key global-map (kbd "C-x 2") 'mookid-split-window-below)
 (define-key global-map (kbd "C-x 3") 'mookid-split-window-right)
 (mapc (lambda (fun) (advice-add fun :after #'balance-windows))
       '(mookid-split-window-right
         mookid-split-window-below
         mookid-delete-window)))

(with-message
 "Flycheck"
 (with-eval-after-load 'flycheck
   (defvar flycheck-mode-map)
   (define-key flycheck-mode-map (kbd "C-S-<next>") 'flycheck-next-error)))

(with-title
 "OCaml configuration"
 (defvar mookid-ocaml-stars "(***************************************************************************)"
   "A separator for OCaml code.")

 (defun mookid-ocaml-insert-stars ()
   "Insert a line with stars."
   (interactive)
   (newline)
   (insert mookid-ocaml-stars)
   (newline))

 (with-message
  "Tuareg"
  (autoload 'tuareg-mode "tuareg")
  (add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

  (defvar tuareg-mode-map)
  (defun mookid-tuareg-setup ()
    "Keybindings for tuareg mode."
    (define-key tuareg-mode-map (kbd "C-'") 'tuareg-eval-region)
    (define-key tuareg-mode-map (kbd "C-c =") 'mookid-ocaml-insert-stars))

  (add-hook 'tuareg-mode-hook 'mookid-tuareg-setup)

  (with-eval-after-load 'caml
    (mapc (lambda (face)
            (when (string-prefix-p "caml-types" (face-name face))
              (set-face-attribute face nil
                                  :background "deep pink"
                                  :foreground "white")))
          (face-list)))

  (defun mookid-ocp-indent-tuareg-setup ()
    (interactive)
    "My setup for ocp-indent."
    (require 'ocp-indent)
    (define-key tuareg-mode-map (kbd "C-=") 'ocp-indent-buffer))
  (add-hook 'tuareg-mode-hook 'mookid-ocp-indent-tuareg-setup))

 (mookid-ignore
  "Caml"
  (require 'caml)
  (add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . caml-mode))

  (defvar caml-mode-map)
  (defun mookid-caml-setup ()
    "Keybindings for caml mode."
    (define-key caml-mode-map (kbd "C-c =") 'mookid-ocaml-insert-stars))

  (add-hook 'caml-mode-hook 'mookid-caml-setup)
  (require 'rainbow-blocks)
  (add-hook 'caml-mode-hook 'rainbow-blocks-mode)
  (defun mookid-ocp-indent-caml-setup ()
    (interactive)
    "My setup for ocp-indent."
    (require 'ocp-indent)
    (define-key caml-mode-map (kbd "C-=") 'ocp-indent-buffer))
  (add-hook 'caml-mode-hook 'mookid-ocp-indent-caml-setup))

 (mookid-ignore
  "Merlin"
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (defun merlin-setup ()
    "My setup for merlin."
    (setq-default merlin-use-auto-complete-mode 'easy)
    (defvar company-backends)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend)))
  (add-hook 'merlin-mode-hook 'merlin-setup)))

(with-title
 "C configuration"
 (defvar c-mode-base-map)
 (defvar c-indentation 8 "The indentation for C code.")
 (defvar c-stars "/*****************************************************************************/"
   "A separator for C code.")
 (defun mookid-c-insert-stars ()
   "Insert the value of `c-stars'."
   (interactive)
   (insert c-stars))
 (require 'find-file)
 (require 'compile)
 (defun mookid-c-setup ()
   "My setup for C."
   (setq-default c-default-style "linux" c-basic-offset c-indentation)
   (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
   (define-key c-mode-base-map (kbd "C-c C-a") 'ff-find-other-file)
   (define-key c-mode-base-map (kbd "C-c =") 'mookid-c-insert-stars)
   (setq-default indent-tabs-mode nil))
 (add-hook 'c-initialization-hook 'mookid-c-setup)
 (require 'clang-format))

(with-message
 "Images"
 (require 'image+)
 (define-key image-mode-map (kbd "+") 'imagex-sticky-zoom-in)
 (define-key image-mode-map (kbd "-") 'imagex-sticky-zoom-out))

(let ((f (expand-file-name "private.el" mookid-root-dir)))
  (when (file-exists-p f)
    (with-message "Loading private settings" (load f))))

(provide 'mookid-init)
;;; mookid-init.el ends here
