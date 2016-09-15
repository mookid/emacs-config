;;; my-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(require 'cl-lib)


;;; Macros
(defmacro my-ignore (&rest _body)
  "Ignore the arguments.  Use it to disable a part of the file."
  nil)

(defmacro my-every-frame (&rest body)
  "Apply BODY to every new frame."
  `(progn
     (add-hook 'after-make-frame-functions
               (lambda (frame)
                 (with-selected-frame frame
                   ,@body)))
     ,@body))

(defmacro my-goto-buffer (buffer-name &optional key)
  "Defines a command to jump to the buffer designated by BUFFER-NAME.

Binds the command to KEY if supplied."
  (let* ((buffer-name-str (symbol-name buffer-name))
         (command-name (intern (concat "my-goto-" buffer-name-str))))
    `(progn
       (defun ,command-name ()
         ,(concat "Goto buffer `" buffer-name-str "'.")
         (interactive)
         (pop-to-buffer ,buffer-name-str))
       (when ,key
         (define-key global-map (kbd ,key) ',command-name)))))


;;; Basic configuration
(defun display-startup-echo-area-message () "Inhibit welcome message." ())

(setq initial-scratch-message nil)

(add-to-list 'completion-styles 'partial-completion)

(setq minibuffer-depth-indicate-mode t)

(defun my-last-2 (list)
  "Remove all the elements of LIST except the last two."
  (let ((lst list))
    (while (cl-caddr lst)
      (setq lst (cdr lst)))
    lst))

(defun my-move-beginning-of-line (_arg)
  "Redefinition of `move-beginning-of-line'."
  (interactive "^p")
  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(global-set-key [remap move-beginning-of-line]
                'my-move-beginning-of-line)


(defun my-scroll-up (arg)
  "Forward to `scroll-up'."
  (interactive "P")
  (let ((arg (or arg 1)))
    (scroll-up arg)))

(defun my-scroll-down (arg)
  "Forward to `scroll-down'."
  (interactive "P")
  (let ((arg (or arg 1)))
    (scroll-down arg)))

(defun my-kill-buffer ()
  "Kill buffer without confirmation."
  (interactive)
  (kill-buffer nil))

;; Keybindings
(define-key global-map (kbd "C-c C-v") 'my-insert-buffer-name)
(define-key global-map (kbd "C-c k") 'delete-frame)
(define-key global-map (kbd "C-c n") 'make-frame)
(define-key global-map (kbd "M-n") 'my-scroll-up)
(define-key global-map (kbd "M-p") 'my-scroll-down)
(define-key global-map (kbd "C-c C-M-<up>") 'raise-sexp)
(define-key global-map (kbd "C-c .") 'repeat)
(define-key global-map (kbd "M-=") 'align-regexp)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "C-x g") 'move-to-column)
(define-key global-map (kbd "C-x p") 'proced)
(define-key global-map (kbd "C-S-o") 'other-window)
(define-key global-map (kbd "M-S-<up>") 'split-window-below)
(define-key global-map (kbd "M-S-<down>") 'delete-other-windows-vertically)
(define-key global-map (kbd "M-S-<left>") 'delete-other-windows)
(define-key global-map (kbd "M-S-<right>") 'split-window-horizontally)
(define-key global-map (kbd "C-x k") 'my-kill-buffer)
(define-key global-map (kbd "C-S-SPC") 'rectangle-mark-mode)

(require 'subr-x)
(defun my-shorten-path (path)
  "Shortens the string representing a PATH for the modeline."
  (let ((r (string-join (cons "..." (my-last-2 (split-string path "/"))) "/")))
    (if (< (length r) (length path)) r path)))

;; Set mode line format
(my-ignore
 (make-face 'mode-line-folder-face)
 (make-face 'mode-line-filename-face)
 (set-face-attribute 'mode-line-filename-face nil :weight 'bold)
 (defvar mode-line-format)
 (setq mode-line-format
       (list "  "
             mode-line-position
             '(:propertize
               (:eval (when buffer-file-name
                        (my-shorten-path default-directory)))
               face mode-line-folder-face)
             '(:propertize "%b" face mode-line-filename-face)
             "%n  "
             mode-line-modes
             mode-line-misc-info
             "%-")))

;; Jump to grep buffer
(my-goto-buffer *grep* "<f10>")

;; Disable the bell
(setq ring-bell-function 'ignore)

;; Enable originally disabled functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Default behaviour for newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; No welcome message
(setq inhibit-startup-message t)

;; Stop auto save
(setq auto-save-default nil)

;; Auto revert
(global-auto-revert-mode 1)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; No tabs
(setq indent-tabs-mode nil)
(defun my-untabify-all ()
  "Untabify the current buffer, except if it is a Makefile.

\(BROKEN)."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook #'my-untabify-all)

;; Delete trailing whitespaces when saving a file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save all buffers when focus is lost
(defun my-save-all-buffers ()
  "Save all buffers."
  (save-some-buffers t))
(add-hook 'focus-out-hook #'my-save-all-buffers)
(add-hook 'shell-mode-hook #'my-save-all-buffers)

;; VC
(define-key global-map (kbd "<f7>") 'vc-diff)
(define-key global-map (kbd "C-<f7>") 'vc-root-diff)

;; Reduce echo delay
(setq echo-keystrokes 0.3)

;; Remove gui elements
(progn
  (and (fboundp 'fringe-mode) (fringe-mode -1))
  (and (fboundp 'tooltip-mode) (tooltip-mode +1))
  (and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (and (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
  (setq use-dialog-box nil)
  (setq pop-up-windows nil))

;; Save history between sessions
(defvar savehist-save-minibuffer-history)
(savehist-mode t)
(setq history-length 16384)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(defvar savehist-additional-variables)
(mapc (lambda (item) (add-to-list 'savehist-additional-variables item))
      '(kill-ring
        search-ring
        regexp-search-ring
        compile-command))

;; Ediff frame setup
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; TAB completion
(define-key global-map (kbd "TAB") 'completion-at-point)

;; Configuring parenthesis settings
(progn
  (defvar electric-pair-pairs)
  (defvar show-paren-delay)
  (electric-pair-mode t)
  (add-to-list 'electric-pair-pairs '(?\{ . ?\}))
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq show-paren-delay 0)
  (show-paren-mode t)
  (defadvice show-paren-function
      (after show-matching-paren-offscreen activate)
    "Show the matching line in the echo area.

Has no effect if the character before point is not of the syntax
class ')'."
    (interactive)
    (let* ((cb (char-before (point)))
           (matching-text (and cb
                               (char-equal (char-syntax cb) ?\) )
                               (blink-matching-open))))
      (when matching-text (message matching-text))))
  (set-face-background 'show-paren-match "turquoise"))

(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 80))

;; Setting up fonts"
(progn
  (defvar my-default-font nil "The font used almost everywhere.")
  (setq my-default-font "Consolas")
  (set-default-coding-systems 'utf-8)
  (add-to-list 'default-frame-alist `(font . ,my-default-font))
  (global-prettify-symbols-mode 1))

;; Customize proportional font"
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")

;; Upcase / downcase commands
(define-key global-map (kbd "C-S-u") 'upcase-region)
(define-key global-map (kbd "C-S-l") 'downcase-region)

(defun my-kill-line-backward ()
  "The same as `kill-line', but backward (and reindent)."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))
(define-key global-map (kbd "C-S-k") 'my-kill-line-backward)

;; Setting up the order for recenter-top-bottom"
(setq recenter-positions '(top middle bottom))

;; Pop mark
(setq set-mark-command-repeat-pop t)

;; Wrap long lines
(progn
  (global-visual-line-mode 1)
  (setcdr visual-line-mode-map nil)
  (with-eval-after-load 'diminish
    (diminish 'visual-line-mode)))

;; Use ibuffer
(progn
  (require 'ibuffer)
  (define-key global-map (kbd "C-x C-b") 'ibuffer)
  (with-eval-after-load 'fullframe
    (fullframe ibuffer ibuffer-quit)))

;; Use fullframe
(with-eval-after-load 'fullframe
  (fullframe list-packages quit-window))

;; Shell
(progn
  (let* ((cygwin-root "c:")
         (cygwin-bin (expand-file-name "bin" cygwin-root)))
    (when (and (eq 'windows-nt system-type)
               (file-readable-p cygwin-root))
      (setq exec-path (cons cygwin-bin exec-path))
      (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
      (setq shell-file-name "bash")
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

  (add-hook 'shell-mode 'dirtrack-mode)
  (define-key global-map (kbd "<f1>") 'shell))

(defun my-previous-buffer ()
  "Not the current buffer but the buffer before."
  (other-buffer (current-buffer) 1))

(defun my-insert-buffer-name ()
  "Insert the previous buffer name.  Useful for compilation."
  (interactive)
  (insert (buffer-name (my-previous-buffer))))

(defun my-insert-buffer-path (arg)
  "Insert the previous buffer path.

With a prefix argument ARG, insert `file:' before."
  (interactive "P")
  (insert (concat (if arg "file:" "")
                  (buffer-file-name (my-previous-buffer)))))

;; from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html:
(defun my-narrow-dwim (p)
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
(define-key global-map (kbd "C-x n n") 'my-narrow-dwim)

(defun my-join-line (beg end)
  "If the range BEG END is active, group it on one line.
Otherwise, join the current line with the following."
  (interactive "r")
  (ignore beg end)
  (cond ((null mark-active)
         (delete-indentation 1))
        (mark-active
         (let ((beg (region-beginning))
               (end (copy-marker (region-end))))
           (goto-char beg)
           (while (< (point) end)
             (join-line 1))))))
(define-key global-map (kbd "M-j") 'my-join-line)

;; Don't kill by accident
(setq confirm-kill-emacs 'y-or-n-p)

;; Display page delimiter as a horizontal line
;; (aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))

(define-key global-map (kbd "M-DEL") 'kill-whole-line)
(advice-add 'kill-whole-line :before #'append-next-kill)


;;; Colors
(defvar my-default-cursor-color "The cursor color by default.")
(defun my-color-config ()
  (cond
   (window-system
    (set-face-attribute 'font-lock-comment-face nil :background "lavender")
    (set-face-attribute 'font-lock-comment-face nil :foreground "steel blue")
    (set-background-color "azure1")
    (set-foreground-color "slate gray")
    (setq my-default-cursor-color "red")
    (set-cursor-color my-default-cursor-color)
    (set-face-attribute 'region nil :background "#ABDFFA")
    (set-face-attribute 'secondary-selection nil :background "#DDFFDD"))
   (t nil)))

(my-every-frame (my-color-config))


;;; Dired
(progn
  (autoload 'dired-find-file "dired")
  (defvar dired-mode-map)
  (autoload 'dired-jump "dired-x")
  (with-eval-after-load 'dired
    (setq dired-listing-switches "-alh")
    (set-face-attribute 'dired-ignored nil
                        :strike-through t
                        :foreground "green")
    (define-key dired-mode-map (kbd "M-<up>") 'dired-jump)
    (define-key dired-mode-map (kbd "M-<down>") 'dired-find-file))
  (define-key global-map (kbd "M-<up>") 'dired-jump))


;;; Find file at point
(require 'ffap)
(ffap-bindings)


;;; Server
(defadvice server-visit-files (before parse-numbers-in-lines
                                      (files proc &optional nowait) activate)
  "Looks for petterns file:line or file:line:position when starting server."
  (ad-set-arg
   0
   (mapcar (lambda (fn)
             (let ((name (car fn)))
               (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                   (cons
                    (match-string 1 name)
                    (cons (string-to-number (match-string 2 name))
                          (string-to-number (or (match-string 3 name) "")))
                    )
                 fn)))
           files)))




;;; Compilation
(defvar compilation-always-kill)
(defvar compilation-scroll-output)

(defun my-disable-jump-to-error ()
  "Disable `compilation-auto-jump-to-next' local variable."
  (kill-local-variable 'compilation-auto-jump-to-next))

(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
(setq compilation-scroll-output 'first-error)

(add-hook 'grep-mode-hook 'my-disable-jump-to-error)
(my-goto-buffer *compilation* "<f5>")
(define-key global-map (kbd "<f12>") 'recompile)
(define-key global-map (kbd "C-<end>") 'recompile)
(define-key global-map (kbd "C-<prior>") 'previous-error)
(define-key global-map (kbd "C-<next>") 'next-error)


;;; Mouse

(require 'mouse)
(global-unset-key (kbd "<S-down-mouse-1>"))
(define-key global-map (kbd "<S-mouse-1>") 'my-acme-search)

(defun my-acme-search (click)
  "Move mouse to the next occurence of the symbol at point and highlight it."
  (interactive "e")
  (let ((sym (if (region-active-p)
                 (buffer-substring (mark) (point))
               (mouse-set-point click)
               (thing-at-point 'filename))))
    (if (file-readable-p sym)
        (special-display-popup-frame (find-file-noselect sym nil nil nil))
      (or (my-acme-search-forward sym)
          (let ((saved-point (point)))
            (message "Wrapped search")
            (goto-char (point-min))
            (or (my-acme-search-forward sym)
                (goto-char saved-point)))))
    ;;Redisplay the screen if we search off the bottom of the window.
    (unless (posn-at-point)
      (universal-argument)
      (recenter))
    (my-move-mouse-to-point)))

(defun my-move-mouse-to-point ()
  "Move the mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
         (window-coords (window-inside-edges))
         (x (+ (car coords) (car window-coords) -1))
         (y (+ (cdr coords) (cadr window-coords)
               (if header-line-format -1 0))))
    (set-mouse-position (selected-frame) x y)))

(defun my-acme-search-forward (sym)
  "Search forward from point for SYM and highlight it.

If there is no match, returns NIL."
  (when (search-forward sym nil t)
    (my-acme-highlight-search sym)
    t))

(defun my-acme-highlight-search (sym)
  "Set the region to the current search result. Assume point is
at the end of the result."
  (set-mark (point))
  (search-backward sym nil t)
  (exchange-point-and-mark))

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


;;; Recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 150)
(define-key global-map (kbd "C-x C-h") 'recentf-open-files)


;;; Selective display
(let ((depth 1))
  (define-key global-map (kbd "<f6>") 'my-selective-display-toggle)
  (define-key global-map (kbd "C-<f6>") 'my-selective-display-increase)
  (define-key global-map (kbd "S-<f6>") 'my-selective-display-decrease)

  (defun my-selective-display-toggle ()
    "Hide lines starting with a lot of spaces.

See `my-selective-display-increase' to increase the number of spaces.
See `my-selective-display-decrease' to decrease it."
    (interactive)
    (set-selective-display (unless selective-display depth)))
  (cl-flet ((g (offset)
               (setq depth (+ depth offset))
               (set-selective-display depth)))
    (defun my-selective-display-increase ()
      "Increase the cap for `toogle-selective-display'.

See `my-selective-display-toggle' and `my-selective-display-decrease'."
      (interactive)
      (when (< depth 20) (g 1)))

    (defun my-selective-display-decrease ()
      "Decrease the cap for `toogle-selective-display'.

See `my-selective-display-toggle' and `my-selective-display-increase'."
      (interactive)
      (when (> depth 1) (g -1)))))


;;; Isearch
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "<right>") 'isearch-yank-word-or-char)

(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)

(define-key isearch-mode-map (kbd "TAB") 'isearch-complete)
(define-key minibuffer-local-isearch-map (kbd "TAB") 'isearch-complete-edit)

(define-key isearch-mode-map (kbd "M-<") 'my-isearch-beginning-of-buffer)
(define-key isearch-mode-map (kbd "M->") 'my-isearch-end-of-buffer)
(define-key global-map (kbd "C-M-s") 'my-isearch-region)

(define-key isearch-mode-map (kbd "<return>") 'my-isearch-exit-leave-hl)

(with-eval-after-load 'diminish
  (diminish 'isearch-mode))

(add-hook 'isearch-mode-hook 'my-notify-grep)
(defun my-notify-grep ()
  (set-cursor-color "yellow"))
(add-hook 'isearch-mode-end-hook 'my-notify-grep-exit)
(defun my-notify-grep-exit ()
  (set-cursor-color my-default-cursor-color))

(defun my-isearch-exit-leave-hl ()
  "Exit search and leave extra match highlighting."
  (interactive)
  (let ((lazy-highlight-cleanup nil))
    (when isearch-lazy-highlight
      (isearch-lazy-highlight-new-loop (point-min) (point-max)))
    (isearch-exit)))

(defun my-isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun my-isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

;; Exit isearch at the beginning of the matching string
(add-hook 'isearch-mode-end-hook #'my-isearch-exit-beginning)
(defun my-isearch-exit-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defun my-isearch-region (beg end)
  "Send selection between BEG and END to isearch."
  (interactive "r")
  (deactivate-mark)
  (kill-ring-save beg end)
  (isearch-mode t nil nil nil)
  (isearch-yank-pop))

(defun my-isearch-forward (regexp-p)
  "Forward to `isearch-forward-regexp' with fancy `whitespace-regexp'.

REGEXP-P is used as in the vanilla Emacs api."
  (interactive "P")
  (let ((isearch-regexp-lax-whitespace t)
        (search-whitespace-regexp ".*?"))
    (isearch-forward-regexp regexp-p)))

;; (defun my-occur-rename-buffer ()
;;   "Used to uniquify the occur buffer names."
;;   (occur-rename-buffer t))
;; (add-hook 'occur-hook #'my-occur-rename-buffer)


;;; Windows
(define-key global-map (kbd "<M-left>") 'previous-buffer)
(define-key global-map (kbd "<M-right>") 'next-buffer)

(setq tags-add-tables nil)

(defun my-mouse-drag-throw (start-event)
  "Similar to `mouse-drag-throw' but only vertically.

Throw the page according to a mouse drag triggering START-EVENT.

To test this function, evaluate: (define-key global-map [down-mouse-2] \\='my-mouse-drag-throw)
and use mouse2."
  (interactive "e")
  (require 'mouse-drag)
  (save-selected-window
    (let* ((start-posn (event-start start-event))
           (start-window (posn-window start-posn))
           (start-row (cdr (posn-col-row start-posn)))
           event end row scroll-delta
           have-scrolled
           (scroll-col-delta 0))
      (select-window start-window)
      (track-mouse
        ;; Don't change the mouse pointer shape while we drag.
        (setq track-mouse 'dragging)
        (while (progn
                 (setq event (read-event)
                       end (event-end event)
                       row (cdr (posn-col-row end)))
                 (or (mouse-movement-p event)
                     (eq (car-safe event) 'switch-frame)))
          (when (eq start-window (posn-window end))
            (setq scroll-delta (mouse-drag-scroll-delta (- start-row row))))

          (if (or (/= 0 scroll-delta)
                  (/= 0 scroll-col-delta))
              (progn
                (setq have-scrolled t)
                (mouse-drag-safe-scroll scroll-delta scroll-col-delta)
                (mouse-drag-repeatedly-safe-scroll scroll-delta
                                                   scroll-col-delta)))))
      ;; If it was a click and not a drag, prepare to pass the event on.
      ;; Is there a more correct way to reconstruct the event?
      (if (and (not have-scrolled)
               (mouse-drag-events-are-point-events-p start-posn end))
          (push (cons (event-basic-type start-event) (cdr start-event))
                unread-command-events)))))

(winner-mode 1)
(define-key global-map (kbd "C-M-S-<left>") 'winner-undo)
(define-key global-map (kbd "C-M-S-<right>") 'winner-redo)
(define-key global-map (kbd "C-M-S-<up>") 'balance-windows)

(windmove-default-keybindings)

;; unbind all keybindings starting with f2
(mapc (lambda (keystr) (global-unset-key (kbd keystr)))
      '("<f2> 2" "<f2> b" "<f2> s"))

(with-eval-after-load 'init
  (define-key global-map (kbd "<f2> <f2>") 'my-toggle-window-split))

(defun my-toggle-window-split ()
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
      (when this-win-2nd (other-window 1)))))


;;; melpa packages

(require 'fullframe)
(require 'diminish)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; Use package
(defvar use-package-verbose)
(setq use-package-verbose t)
(require 'use-package)

(use-package eshell
  :defer t
  :disabled t
  :bind ("<f5>" . eshell))

(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c c". evilnc-copy-and-comment-lines)))

(use-package anzu
  :init (global-anzu-mode 1)
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace-regexp)))

(use-package lispy
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  :config
  (progn
    (lispy-set-key-theme '(special))))

(use-package magit
  :defer t
  :disabled t
  :bind (("<f7>" . magit-status))
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)))

(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (defun my-rainbow-delimiters-disable ()
      "Disable rainbow-delimiters mode."
      (rainbow-delimiters-mode -1))

    (add-hook 'shell-mode-hook #'my-rainbow-delimiters-disable)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground "red"
                        :inherit 'error
                        :box t)))

(use-package loccur
  :bind
  (("M-s o" . loccur-current)
   ("M-s i" . loccur)
   ("M-s p" . loccur-previous-match)))

(use-package company
  :defer t
  :config
  (progn
    (defvar company-idle-delay)
    (defvar company-tooltip-limit)
    (defvar company-minimum-prefix-length)
    (defvar company-tooltip-flip-when-above)
    (setq company-idle-delay 0.)
    (setq company-tooltip-limit 5)
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-flip-when-above t)))

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
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-M-y". counsel-yank-pop)
   :map isearch-mode-map
   ("M-s p" . swiper-from-isearch)
   :map ivy-minibuffer-map
   ("<right>" . ivy-alt-done))
  :config
  (progn
    (defvar ivy-use-virtual-buffers)
    (setq ivy-use-virtual-buffers t)))

(use-package projectile
  :bind (("<C-S-return>" . my-projectile))
  :diminish projectile-mode
  :init
  (with-eval-after-load 'key-chord
      (key-chord-define-global "pf" 'projectile-find-file)
      (key-chord-define-global "pp" 'projectile-switch-project))
  :config
  (progn
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)
    (defun my-projectile (p)
      "My projectile command.

If P is non nil, call `projectile-find-file' else call `projectile-switch-project'."
      (interactive "P")
      (if p (projectile-switch-project) (projectile-find-file)))
    (setq projectile-completion-system 'ivy)
    (projectile-global-mode)))

(use-package slime
  :defer t
  :bind (("C-c y" . hyperspec-lookup))
  :init
  (progn
    (defvar common-lisp-hyperspec-root)
    (defvar inferior-lisp-program)
    (require 'slime-autoloads)
    (setq inferior-lisp-program "sbcl")
    (add-hook 'comint-mode-hook 'rainbow-delimiters-mode)
    (setq common-lisp-hyperspec-root "file:///Hyperspec/")))

(use-package expand-region
  :defer t
  :bind (("C-M-SPC" . er/expand-region))
  :config
  (setq expand-region-contract-fast-key "z"
        expand-region-reset-fast-key "x"
        expand-region-fast-keys-enabled t))

(use-package avy
  :defer t
  :bind (("C-z" . avy-goto-word-or-subword-1))
  :init (setq avy-all-windows 'all-frames))

(use-package ace-window
  :defer t
  :bind
  (("C-'" . ace-window))
  :config
  (progn
    (defun my-other-window ()
      "Forwards to `other-window'."
      (interactive)
      (other-window 1))
    (defvar aw-keys)
    (defvar aw-dispatch-always)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-dispatch-always t)
    (defvar aw-dispatch-alist)
    (add-to-list 'aw-dispatch-alist '(?v split-window-right))
    (add-to-list 'aw-dispatch-alist '(?b split-window-below))
    (add-to-list 'aw-dispatch-alist '(?c delete-other-windows-vertically))))

(use-package flycheck
  :defer t
  :bind (:map flycheck-mode-map ("C-S-<next>" . flycheck-next-error)))

;; OCaml configuration
(progn
  (use-package tuareg
    :defer t
    :mode "\\.m[lf][ily]?$"
    :config
    (progn
      ;; (add-to-list 'auto-mode-alist
      ;;              '("\\.m[lf][ily]?$" . tuareg-mode))
      (define-key tuareg-mode-map (kbd "C-c .") nil)
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
                                    :background "yellow"
                                    :foreground "black")))
            (face-list))))

  (use-package merlin
    :disabled t
    :defer t
    :after tuareg
    :init
    (progn
      (defvar merlin-use-auto-complete-mode)
      (setq merlin-use-auto-complete-mode 'easy)
      (defvar company-backends)
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'merlin-company-backend)))))

;; C configuration
(progn
  (defun my-c-setup ()
    "My setup for C."
    (defvar c-default-style)
    (defvar indent-tabs-mode)
    (setq c-default-style "linux")
    (setq indent-tabs-mode nil)
    (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
    (define-key c-mode-base-map (kbd "C-c C-a") 'ff-find-other-file))
  (add-hook 'c-initialization-hook #'my-c-setup))

;; Images
(with-eval-after-load "image-mode"
  (require 'image+)
  (defvar image-mode-map)
  (define-key image-mode-map (kbd "+") 'imagex-sticky-zoom-in)
  (define-key image-mode-map (kbd "-") 'imagex-sticky-zoom-out))

(use-package smart-mode-line
  :init (setq sml/no-confirm-load-theme t)
  :config (sml/setup))

(use-package powerline
  :disabled t
  :config
  (setq powerline-display-buffer-size nil)
  (setq powerline-display-mule-info nil)
  (setq powerline-display-hud nil)
  (mapc (lambda (face) (set-face-attribute face nil :foreground "grey"))
        '(powerline-active1
          powerline-active2
          powerline-inactive1
          powerline-inactive2))
  (powerline-default-theme))

(use-package yasnippet
  :defer t
  :init (setq yas-snippet-dirs `(,(expand-file-name "snippets" my-root-dir)))
  :config (yas-reload-all))

(use-package org
  :defer t
  :config
  (progn
    ;; allow for export=>beamer by placing
    ;; #+LaTeX_CLASS: beamer in org files
    (defvar org-export-latex-classes)
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

(use-package key-chord
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "jg" 'abort-recursive-edit)
    (key-chord-define-global "jk" 'execute-extended-command)
    (key-chord-define-global "fj" 'find-file-at-point)
    (key-chord-define-global "fb" 'switch-to-buffer)
    (with-eval-after-load 'recentf
      (key-chord-define-global "fh" 'recentf-open-files))))

(use-package composable
  :diminish composable-mode
  :init (composable-mode 1)
  :bind
  (:map composable-mode-map
        ([remap upcase-region] . composable-upcase-region)
        ([remap downcase-region] . composable-downcase-region)
        ("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package diff-hl
  :init
  (progn
    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
    (diff-hl-margin-mode 1)
    (diff-hl-flydiff-mode 1))
  :bind
  (("C-M-[" . diff-hl-previous-hunk)
   ("C-M-]" . diff-hl-next-hunk)))

(use-package multiple-cursors
  :bind
  (("C-M-." . mc/mark-next-like-this)
   ("C-M-," . mc/mark-previous-like-this)
   ("C-M-/" . mc/mark-all-like-this)))

(provide 'my-init)
;;; my-init.el ends here
