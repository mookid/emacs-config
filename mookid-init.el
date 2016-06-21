;;; mookid-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(require 'cl-lib)


;;; Macros
(defmacro mookid-with-message (msg &rest body)
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

(add-to-list 'completion-styles 'partial-completion)

(setq minibuffer-depth-indicate-mode t)

(define-key global-map (kbd "C-c C-M-<up>") 'raise-sexp)

(defun mookid-last-2 (list)
  "Remove all the elements of LIST except the last two."
  (let ((lst list))
    (while (cl-caddr lst)
      (setq lst (cdr lst)))
    lst))

(defun mookid-move-beginning-of-line (arg)
  "Redefinition of `move-beginning-of-line'."
  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'mookid-move-beginning-of-line)

(define-key global-map (kbd "M-p") 'backward-paragraph)
(define-key global-map (kbd "M-n") 'forward-paragraph)

(require 'subr-x)
(defun mookid-shorten-path (path)
  "Shortens the string representing a PATH for the modeline."
  (let ((r (string-join (cons "..." (mookid-last-2 (split-string path "/"))) "/")))
    (if (< (length r) (length path)) r path)))

(mookid-ignore
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

(mookid-with-message
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
(mapc (lambda (item) (add-to-list 'savehist-additional-variables item))
      '(kill-ring
        search-ring
        regexp-search-ring
        compile-command))

(mookid-with-message
 "Configuring parenthesis settings"
 (electric-pair-mode t)
 (setq-default electric-pair-pairs '((?\{ . ?\})))
 (show-paren-mode t)
 (set-face-background 'show-paren-match "turquoise")
 (setq-default show-paren-delay 0))

(mookid-with-message
 "Setting up frame size"
 ;; (add-to-list 'default-frame-alist '(height . 55))
 (add-to-list 'default-frame-alist '(width . 80)))

(mookid-with-message
 "Setting up fonts"
 (defvar mookid-default-font nil "The font used almost everywhere.")
 (setq mookid-default-font "Consolas")
 (set-default-coding-systems 'utf-8)
 (add-to-list 'default-frame-alist `(font . ,mookid-default-font))
 (global-prettify-symbols-mode 1))

(mookid-with-message
 "Customize proportional font"
 (set-face-attribute 'variable-pitch nil
                     :family "DejaVu Sans"))

(define-key global-map (kbd "C-c h") 'help-command)

(defun mookid-upcase-char (arg)
  "Applies `upcase-region' to the following ARG characters."
  (interactive "P")
  (let ((arg (or arg 1)))
    (upcase-region
     (point)
     (+ (point) arg))))

(defun mookid-downcase-char (arg)
  "Applies `downcase-region' to the following ARG characters."
  (interactive "P")
  (let ((arg (or arg 1)))
    (downcase-region
     (point)
     (+ (point) arg))))

(define-key global-map (kbd "C-c u") 'mookid-upcase-char)
(define-key global-map (kbd "C-c l") 'mookid-downcase-char)

(mookid-with-message
 "Keyboard translations"
 (define-key key-translation-map (kbd "C-h") (kbd "C-p"))
 (define-key key-translation-map (kbd "M-h") (kbd "M-p")))

(mookid-with-message
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

(mookid-with-message
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

;; Stop exiting with the keyboard
(global-unset-key (kbd "C-x C-c"))

(define-key global-map (kbd "M-=") 'align-regexp)

(define-key global-map (kbd "<M-left>") 'previous-buffer)
(define-key global-map (kbd "<M-right>") 'next-buffer)

;; Display page delimiter as a horizontal line
(aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))

(define-key global-map (kbd "M-DEL") 'kill-whole-line)
(advice-add 'kill-whole-line :before #'append-next-kill)


;;; Colors

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

(load-theme 'plan9 t)

(cl-dolist (face '(font-lock-comment-face
                   font-lock-comment-delimiter-face))
  (set-face-attribute face nil :foreground "grey" :slant 'italic))

;; (add-to-list 'default-frame-alist '(background-color . "#fdf6e3"))

;; (add-to-list 'default-frame-alist '(background-color . "azure1"))
(add-to-list 'default-frame-alist '(foreground-color . "#586e75"))
(add-to-list 'default-frame-alist '(cursor-color . "red"))
;; (set-face-attribute 'region nil :background "#ABDFFA")

;; (mookid-colors-leuven)

;;; Dired
(autoload 'dired-find-file "dired")
(defvar dired-mode-map)
(autoload 'dired-jump "dired-x")

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-<up>") 'dired-jump)
  (define-key dired-mode-map (kbd "M-<down>") 'dired-find-file))

(define-key global-map (kbd "M-<up>") 'dired-jump)


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

;; (define-key global-map (kbd "<mode-line> <down-mouse-1>") 'enlarge-window)

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


;;; Windows

(mookid-with-message
 "Windows commands"
 (defun mookid-split-window-right ()
   "Forwards to `split-window-below' and rebalances."
   (split-window-right))

 (defun mookid-split-window-below ()
   "Forwards to `split-window-below' and rebalances."
   (split-window-below))

 (defun mookid-delete-window ()
   "Forwards to `delete-window' and rebalances."
   (delete-window))

 (mapc (lambda (fun) (advice-add fun :after #'balance-windows))
       '(mookid-split-window-right
         mookid-split-window-below
         mookid-delete-window))

 (define-key global-map (kbd "C-x 0") 'mookid-delete-window)
 (define-key global-map (kbd "C-x 2") 'mookid-split-window-below)
 (define-key global-map (kbd "C-x 3") 'mookid-split-window-right))


;;; melpa packages

(require 'fullframe)
(require 'diminish)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; Use package
(setq use-package-verbose t)
(require 'use-package)

(use-package evil-nerd-commenter
  :defer t
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c c". evilnc-copy-and-comment-lines)))

(use-package anzu
  :init (global-anzu-mode 1)
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace-regexp)))

(use-package lispy
  :defer t
  :init (add-hook 'prog-mode-hook 'lispy-mode)
  :config
  (progn
    (lispy-set-key-theme '(special))))

(use-package magit
  :defer t
  :bind (("<f7>" . magit-status))
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)))

(use-package rainbow-delimiters
  :config
  (progn
    (autoload 'mookid-default-font "mookid-naked-emacs-config")
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (defun mookid-rainbow-delimiters-disable ()
      "Disable rainbow-delimiters mode."
      (rainbow-delimiters-mode -1))

    (add-hook 'shell-mode-hook #'mookid-rainbow-delimiters-disable)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground "red"
                        :inherit 'error
                        :box t)))

(use-package rainbow-blocks
  :defer t
  :config
  (progn
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
         do (set-level lvl (nth icolor colors)))))))

(use-package company
  :defer t
  :config
  (progn
    (setq-default company-idle-delay 0.5)
    (setq-default company-tooltip-limit 5)
    (setq-default company-minimum-prefix-length 2)
    (setq-default company-tooltip-flip-when-above t)))

(use-package elisp-slime-nav
  :defer t
  :diminish elisp-slime-nav-mode
  :bind (("C-\"" . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (add-hook 'lisp-interaction-mode 'elisp-slime-nav-mode)))

(use-package ivy
  :defer t
  :diminish ivy-mode
  :bind
  (("M-m" . counsel-M-x)
   ("M-s p" . swiper)
   ("<M-return>" . ivy-switch-buffer)
   ("<C-return>". counsel-find-file)
   :map isearch-mode-map
   ("M-s p" . swiper-from-isearch)
   :map ivy-minibuffer-map
   ("<right>" . ivy-alt-done))
  :config
  (setq-default ivy-use-virtual-buffers t))

(use-package projectile
  :defer t
  :bind (("<C-S-return>" . mookid-projectile))
  :config
  (progn
    (setq-default projectile-indexing-method 'alien)
    (setq-default projectile-enable-caching t)
    (setq projectile-mode-line
          '(:eval (concat " <" (projectile-project-name) ">")))
    (defun mookid-projectile (p)
      "My projectile command.

If P is non nil, call `projectile-find-file' else call `projectile-switch-project'."
      (interactive "P")
      (if p (projectile-switch-project) (projectile-find-file)))
    (setq-default projectile-completion-system 'ivy)
    (projectile-global-mode)))

(use-package counsel
  :defer t
  :after ivy
  :init
  (progn
    (add-hook 'grep-setup-hook
              (lambda () (define-key grep-mode-map (kbd "RET")
                      'ivy-switch-buffer)))))

(use-package slime
  :defer t
  :bind (("C-c h" . hyperspec-lookup))
  :init
  (progn
    (require 'slime-autoloads)
    (setq-default inferior-lisp-program "sbcl")
    (add-hook 'comint-mode-hook 'rainbow-delimiters-mode)
    (setq-default common-lisp-hyperspec-root "file:///Hyperspec/")))

(use-package expand-region
  :defer t
  :bind (("C-M-SPC" . er/expand-region))
  :config
  (setq expand-region-contract-fast-key "z"
        expand-region-reset-fast-key "x"
        expand-region-fast-keys-enabled t))

(use-package avy
  :defer t
  :bind (("C-:" . avy-goto-word-or-subword-1))
  :config
  (progn
    (setq-default avy-all-windows 'all-frames)))

(use-package ace-window
  :defer t
  :bind
  (("M-o" . ace-window))
  :config
  (progn
    (defun mookid-other-window ()
      "Forwards to `other-window'."
      (interactive)
      (other-window 1))
    (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq-default aw-dispatch-always t)
    (defvar aw-dispatch-alist)
    (add-to-list 'aw-dispatch-alist '(?v mookid-split-window-right))
    (add-to-list 'aw-dispatch-alist '(?o mookid-other-window))
    (add-to-list 'aw-dispatch-alist '(?b mookid-split-window-below))
    (add-to-list 'aw-dispatch-alist '(?c mookid-delete-window))))

(use-package flycheck
  :defer t
  :bind (:map flycheck-mode-map ("C-S-<next>" . flycheck-next-error)))

(mookid-with-message
 "OCaml configuration"


 (use-package tuareg
   :defer t
   :mode "\\.ml[ily]?$"
   :config
   (progn
     (mapc (lambda (face)
             (set-face-attribute face nil
                                 :foreground 'unspecified
                                 :weight 'unspecified
                                 :inherit 'font-lock-keyword-face))
           '(tuareg-font-lock-governing-face
             tuareg-font-lock-module-face))
     (set-face-attribute tuareg-font-lock-module-face nil
                                 :weight 'bold)))


 (use-package ocp-indent
   :defer t
   :after tuareg
   :bind (:map tuareg-mode-map ("C-=" . ocp-indent-buffer)))

 (use-package caml
   :defer t
   :after tuareg
   :init
   (progn
     (mapc (lambda (face)
             (when (string-prefix-p "caml-types" (face-name face))
               (set-face-attribute face nil
                                   :background "deep pink"
                                   :foreground "white")))
           (face-list))))

 (use-package merlin
   :disabled t
   :defer t
   :after tuareg
   :init
   (progn
     (setq-default merlin-use-auto-complete-mode 'easy)
     (defvar company-backends)
     (with-eval-after-load 'company
       (add-to-list 'company-backends 'merlin-company-backend)))))

(mookid-with-message
 "C configuration"
 (defvar c-mode-base-map)
 (defvar c-indentation 8 "The indentation for C code.")
 (defvar c-stars "/*****************************************************************************/"
   "A separator for C code.")

 (defun mookid-c-insert-stars ()
   "Insert the value of `c-stars'."
   (interactive)
   (insert c-stars))

 (defun mookid-c-setup ()
   "My setup for C."
   (setq-default c-default-style "linux" c-basic-offset c-indentation)
   (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
   (define-key c-mode-base-map (kbd "C-c C-a") 'ff-find-other-file)
   (define-key c-mode-base-map (kbd "C-c =") 'mookid-c-insert-stars)
   (setq-default indent-tabs-mode nil))

 (add-hook 'c-initialization-hook 'mookid-c-setup)

 (use-package find-file :defer t)
 (use-package compile :defer t)
 (use-package clang-format :defer t))

(mookid-with-message
 "Images"
 (with-eval-after-load "image-mode"
   (require 'image+)
   (defvar image-mode-map)
   (define-key image-mode-map (kbd "+") 'imagex-sticky-zoom-in)
   (define-key image-mode-map (kbd "-") 'imagex-sticky-zoom-out)))

(let ((f (expand-file-name "private.el" mookid-root-dir)))
  (when (file-exists-p f)
    (mookid-with-message "Loading private settings" (load f))))

(use-package smart-mode-line
  :config (progn (setq sml/no-confirm-load-theme t)
                 (setq powerline-arrow-shape 'curve)
                 (sml/setup)))

(use-package powerline
  :config
  :disabled t
  (setq powerline-display-buffer-size nil)
  (setq powerline-display-mule-info nil)
  (setq powerline-display-hud nil)
  (mapc (lambda (face) (set-face-attribute face nil :foreground "grey"))
        '(powerline-active1
          powerline-active2
          powerline-inactive1
          powerline-inactive2))
  (powerline-default-theme))

(use-package org
  :defer t
  :config
  (progn
    ;; allow for export=>beamer by placing
    ;; #+LaTeX_CLASS: beamer in org files
    (unless (boundp 'org-export-latex-classes)
      (setq org-export-latex-classes nil))
    (add-to-list 'org-export-latex-classes
                 ;; beamer class, for presentations
                 '("beamer"
                   "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

                   ("\\section{%s}" . "\\section*{%s}")

                   ("\\begin{frame}[fragile]\\frametitle{%s}"
                    "\\end{frame}"
                    "\\begin{frame}[fragile]\\frametitle{%s}"
                    "\\end{frame}")))

    ;; letter class, for formal letters

    (add-to-list 'org-export-latex-classes

                 '("letter"
                   "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(provide 'mookid-init)
;;; mookid-init.el ends here
