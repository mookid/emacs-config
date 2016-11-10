;;; my-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(require 'cl-lib)


;;; Macros
(defmacro my-ignore (&rest _body)
  "Ignore the arguments.  Use it to disable a part of the file."
  (declare (indent defun))
  nil)

(defmacro my-goto-buffer (buffer-name)
  "Defines a command to jump to the buffer designated by BUFFER-NAME."
  (let* ((buffer-name-str (symbol-name buffer-name))
         (command-name (intern (concat "my-goto-" buffer-name-str))))
    `(defun ,command-name ()
       ,(concat "Goto buffer `" buffer-name-str "'.")
       (interactive)
       (pop-to-buffer ,buffer-name-str))))

;;; Key chords pre-setup
(defvar my-key-chords-alist nil)
(defun my-key-chord-define-global (key symb)
  (push (cons key symb) my-key-chords-alist))


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

(define-key global-map [remap move-beginning-of-line]
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
(define-key global-map [remap kill-buffer] 'my-kill-buffer)

(defun my-other-window-kill-buffer ()
  "Kill the buffer in the other window."
  (interactive)
  (let ((buf-name (buffer-name)))
    (save-window-excursion
      (select-window (next-window))
      (if (string= buf-name (buffer-name))
          (error "The next window dislays the same buffer!")
        (kill-buffer (current-buffer))))))
(define-key global-map (kbd "C-x K") 'my-other-window-kill-buffer)

(defun my-yank (&optional arg)
  "My replacement command for `yank'.

With C-u C-u as prefix argument ARG, clone either the active
region (if any) or the next sexp."
  (interactive "*P")
  (if (not (equal '(16) arg))
      (yank arg)
    (unless (region-active-p)
      (back-to-indentation)
      (mark-sexp))
    (let ((hi (region-end)))
      (copy-region-as-kill (region-beginning) hi)
      (goto-char hi)
      (newline nil t)
      (yank nil))))
(define-key global-map [remap yank] 'my-yank)

(defun my-open-line-above (arg)
  "Move to the previous line and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(define-key global-map (kbd "C-S-o") 'my-open-line-above)

(defun my-open-line-below (arg)
  "Move to the next line and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (indent-according-to-mode))
(define-key global-map [remap open-line] 'my-open-line-below)

;; Recursive edit preserving windows
(defmacro my-recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
or \\[abort-recursive-edit] (abort)), restore window configuration
in current frame.
Inspired by Erik Naggum's `recursive-edit-with-single-window'."
  (declare (indent defun))
  `(defun ,(cl-gensym "my-recursive-edit-command-") ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

(define-key global-map (kbd "C-x 1")
  (my-recursive-edit-preserving-window-config
    (if (one-window-p 'ignore-minibuffer)
        (error "Current window is the only window in its frame")
      (delete-other-windows))))

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
(define-key global-map (kbd "M-`") 'other-window)
(define-key global-map (kbd "M-S-<up>") 'split-window-below)
(define-key global-map (kbd "M-S-<down>") 'delete-other-windows-vertically)
(define-key global-map (kbd "M-S-<left>") 'delete-other-windows)
(define-key global-map (kbd "M-S-<right>") 'split-window-horizontally)
(define-key global-map (kbd "M-+") 'balance-windows) ;; M-S-=
(define-key global-map (kbd "C-S-SPC") 'rectangle-mark-mode)
(define-key global-map (kbd "<M-left>") 'previous-buffer)
(define-key global-map (kbd "<M-right>") 'next-buffer)
(define-key global-map (kbd "C-c /") 'rgrep)
(define-key global-map (kbd "M-%") 'query-replace-regexp)

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

(progn
  (defface mode-line-vc-project-face
    '((t (:inherit 'dired-directory)))
    "Face for the mode-line project.")
  (defun my-mode-line-project ()
    (concat (if (fboundp 'projectile-project-name)
                (concat (projectile-project-name) "|")
              nil)
            (let ((backend (vc-backend (buffer-file-name))))
              (substring vc-mode (+ (or (string-match "Git-\\|Git:\\|SVN:\\|SVN-" vc-mode) -4) 4)))))

  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval
             (propertize (my-mode-line-project)
                         'face 'mode-line-vc-project-face)))))

;; Jump to grep buffer
(define-key global-map (kbd "<f10>") (my-goto-buffer *grep*))

;; Find *scratch* buffer
(my-key-chord-define-global "fs" (my-goto-buffer *scratch*))

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
(use-package vc
  :init (define-key global-map (kbd "S-<f7>") (my-goto-buffer *vc-diff*))
  :bind
  (("<f7>" . vc-diff)
   ("C-<f7>" . vc-root-diff)))

;; Reduce echo delay
(setq echo-keystrokes 0.3)

;; Remove gui elements
(progn
  (and (fboundp 'fringe-mode) (fringe-mode +8))
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
(setq completion-cycle-threshold 5)

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

;; Setting up fonts
(progn
  (defvar my-default-font nil "The font used almost everywhere.")
  (setq my-default-font "Inconsolata 14")
  (set-default-coding-systems 'utf-8)
  (with-eval-after-load 'init
    (add-to-list 'default-frame-alist `(font . ,my-default-font))))

;; Customize proportional font"
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")

;; Upcase / downcase commands
(define-key global-map (kbd "C-S-u") 'upcase-region)
(define-key global-map (kbd "C-S-l") 'downcase-region)

(defun my-kill-line-backward (&optional arg)
  "The same as `kill-line', but backward (and reindent).

If non nil, ARG overrides the `back-to-indentation' function."
  (interactive)
  (let ((start (point)))
    (funcall (or arg 'back-to-indentation))
    (kill-region (point) start)))
(define-key global-map (kbd "C-S-k") 'my-kill-line-backward)

;; Setting up the order for recenter-top-bottom"
(setq recenter-positions '(2 middle -2))

;; Pop mark
(setq set-mark-command-repeat-pop t)

;; Wrap long lines
(use-package diminish)

(progn
  (global-visual-line-mode 1)
  (setcdr visual-line-mode-map nil)
  (diminish 'visual-line-mode))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (with-eval-after-load 'fullframe
    (fullframe ibuffer ibuffer-quit)))

(use-package package
  :init
  (with-eval-after-load 'fullframe
    (fullframe list-packages quit-window)))

;; Shell
(progn
  (let* ((cygwin-root "c:")
         (cygwin-bin (expand-file-name "bin" cygwin-root)))
    (when (and (eq 'windows-nt system-type)
               (file-readable-p cygwin-root))
      (add-to-list 'exec-path cygwin-bin)
      (add-to-list 'exec-path "/usr/local/bin")
      (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
      (setq shell-file-name "bash")
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

  (add-hook 'shell-mode 'dirtrack-mode))

(let (my-zap-to-char-last-arg)
  (defun my-zap-to-char ()
    "Case sensitive, repetition friendly version of `zap-to-char'."
    (interactive)
    (let ((case-fold-search nil)
          (arg (if (eq last-repeatable-command 'my-zap-to-char)
                   my-zap-to-char-last-arg
                 (setq my-zap-to-char-last-arg (read-char "zap to char: ")))))
      (zap-to-char 1 arg))))
(define-key global-map [remap zap-to-char] 'my-zap-to-char)

(define-key global-map (kbd "C-.") 'my-jump-to-char)
(let (my-jump-to-char-last-arg)
  (defun my-jump-to-char ()
    "Jump to the next occurence of CHAR."
    (interactive)
    (let ((char (if (eq last-repeatable-command 'my-jump-to-char)
                    my-jump-to-char-last-arg
                  (setq my-jump-to-char-last-arg (read-char "look for: ")))))
      (forward-char 1)
      (let ((case-fold-search t))
        (search-forward (char-to-string char) nil t))
      (backward-char 1))))

(defun my-previous-buffer ()
  "Not the current buffer but the buffer before."
  (other-buffer (current-buffer) 1))

(defun my-insert-buffer-name ()
  "Insert the previous buffer name.  Useful for compilation."
  (interactive)
  (insert (file-name-sans-extension (buffer-name (my-previous-buffer)))))

(defun my-insert-buffer-file-name (arg)
  "Insert the previous buffer path.

With a prefix argument ARG, insert `file:' before."
  (interactive "P")
  (insert (concat (if arg "file:" "")
                  (buffer-file-name (my-previous-buffer)))))

;; Display page delimiter as a horizontal line
;; (aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))

(define-key global-map (kbd "M-DEL") 'kill-whole-line)
(advice-add 'kill-whole-line :before #'append-next-kill)


;;; Colors
(load-theme 'xemacs t nil)


(use-package dired
  :demand t
  :init
  (use-package dired-x
    :bind
    (("M-<up>" . dired-jump)
     :map dired-mode-map
     ("M-<up>" . dired-jump)
     ("C-x C-j" . dired-jump)))
  :bind (:map dired-mode-map ("M-<down>" . dired-find-file))
  :config
  (progn
    (setq dired-listing-switches "-alh")
    (set-face-attribute 'dired-ignored nil
                        :strike-through t
                        :foreground "gray40")))


;;; Find file at point
(use-package find-file-at-point
  :bind
  ([remap find-file] . find-file-at-point))

;;; Google search
(defun my-prompt ()
  "Default value for prompt is a current word or active region,
if its size is 1 line."
  (cond ((and transient-mark-mode mark-active)
         (let ((pos1 (region-beginning))
               (pos2 (region-end)))
           ;; Check if the start and the end of an active region is on
           ;; the same line
           (when (save-excursion
                   (goto-char pos1)
                   (<= pos2 (line-end-position)))
             (buffer-substring-no-properties pos1 pos2))))
        (t
         (current-word))))

(defun my-google-search (w)
  "Search on google the word at point."
  (interactive (list (read-string "query: " (my-prompt))))
  (browse-url (concat "https://www.google.com/search?q=" w)))
(define-key global-map (kbd "C-h g") 'my-google-search)


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
(define-key global-map (kbd "<f5>") (my-goto-buffer *compilation*))
(define-key global-map (kbd "<f12>") 'recompile)
(define-key global-map (kbd "C-<end>") 'recompile)
(define-key global-map (kbd "C-<prior>") 'previous-error)
(define-key global-map (kbd "C-<next>") 'next-error)

;; Show grep matches at the end of the *grep* buffer
(with-eval-after-load 'grep
  ;; from grep.el; keep in sync
  (add-to-list 'grep-mode-font-lock-keywords
               '("^Grep[/a-zA-z]* finished \\(?:(\\([0-9]+ match\\(es\\)? found\\))\\).*"
                 (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
                 (1 compilation-info-face nil t)))

  (defun my-count-grep-matches (buf _msg)
    (save-excursion
      (set-buffer buf)
      (let* ((count (- (count-lines (point-min) (point-max))
                       6))
             (match (if (> count 1) "matches" "match")))
        (unless (zerop count)
          (goto-char (point-max))
          (search-backward "Grep finished (matches found)" nil t)
          (let ((msg (format "Grep finished (%d %s found)" count match)))
            (replace-match msg nil t)
            (message msg))))))

  (defun my-count-grep-matches-hook ()
    (add-hook 'compilation-finish-functions 'my-count-grep-matches nil t))

  (add-hook 'grep-mode-hook 'my-count-grep-matches-hook))

 
;;; Use package
(defvar use-package-verbose)
(setq use-package-verbose t)
(require 'use-package)


;;; Mouse
(use-package mouse
  :bind
  (("<S-mouse-1>" . my-acme-search-forward)
   ("<S-mouse-3>" . my-acme-search-backward)
   ("<C-wheel-up>" . text-scale-increase)
   ("<C-wheel-down>" . text-scale-decrease))
  :config
  (progn
    (global-unset-key (kbd "<S-down-mouse-1>"))
    (global-unset-key (kbd "<S-down-mouse-3>"))

    (setq mouse-drag-copy-region t)
    (setq mouse-yank-at-point t)

    (defun my-acme-search-forward (click)
      "Move mouse to the next occurence of either the active region,
or the symbol at point, and highlight it."
      (interactive "e")
      (my-acme-search--driver click t))

    (defun my-acme-search-backward (click)
      "Move mouse to the previous occurence of either the active
region or the symbol at point, and highlight it."
      (interactive "e")
      (my-acme-search--driver click nil))

    (defun my-acme-search--driver (click forward)
      "Move mouse to another occurence of either the active region,
or the symbol at point and highlight it.

If FORWARD then move forward, otherwise move backward."
      (let ((sym (if (region-active-p)
                     (buffer-substring (mark) (point))
                   (mouse-set-point click)
                   (thing-at-point 'filename))))
        (cond ((not (and sym (stringp sym))) nil)
              ((file-readable-p sym)
               (special-display-popup-frame (find-file-noselect sym nil nil nil)))
              (t
               (or (my-acme-search--move sym forward)
                   (let ((saved-point (point)))
                     (message "Wrapped search")
                     (if forward
                         (goto-char (point-min))
                       (goto-char (point-max)))
                     (or (my-acme-search--move sym forward)
                         (goto-char saved-point))))))
        ;; Redisplay the screen if we search off the bottom of the window.
        (unless (posn-at-point)
          (universal-argument)
          (recenter))
        (my-move-mouse-to-point forward)))

    (defun my-move-mouse-to-point (forward)
      "Move the mouse pointer to point in the current window."
      (let* ((coords (posn-col-row (posn-at-point)))
             (window-coords (window-inside-edges))
             (offset (if forward -1 1))
             (x (+ (car coords) (car window-coords) offset))
             (y (+ (cdr coords) (cadr window-coords)
                   (if header-line-format -1 0))))
        (set-mouse-position (selected-frame) x y)))

    (defun my-acme-search--move (sym forward)
      "Search from point for SYM and highlight it.

If FORWARD then move forward, otherwise move backward.

If there is no match, returns NIL."
      (push-mark-command nil t)
      (when (if forward
                (search-forward sym nil t)
              (search-backward sym nil t))
        (my-acme-highlight-search sym forward)
        t))

    (defun my-acme-highlight-search (sym forward)
      "Set the region to the current search result."
      (set-mark (point))
      (if forward
          (search-backward sym nil t)
        (search-forward sym nil t))
      (exchange-point-and-mark))))

(use-package mouse-copy
  :bind
  (("<C-down-mouse-1>" . mouse-drag-secondary-pasting)
   ("<C-S-down-mouse-1>" . mouse-drag-secondary-moving))
  :config
  (setq mouse-drag-copy-region t))


;;; Recentf
(use-package recentf
  :init
  (recentf-mode +1)
  :bind
  (("C-x C-h" . recentf-open-files))
  :config
  (progn
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 150)))


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
(use-package isearch
  :diminish isearch-mode
  :bind
  (("C-M-s" . my-isearch-region)
   :map
   minibuffer-local-isearch-map
   ("TAB" . isearch-complete-edit)
   :map
   isearch-mode-map
   ("C-o" . my-isearch-occur)
   ("TAB" . isearch-complete)
   ("M-<" . my-isearch-beginning-of-buffer)
   ("M->" . my-isearch-end-of-buffer))

  :init
  (progn
    (fset 'my-isearch-occur
          (my-recursive-edit-preserving-window-config
            (progn
              (delete-other-windows)
              (call-interactively 'isearch-occur))))

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
      (goto-char (region-beginning))
      (isearch-mode t)
      (isearch-yank-pop))))


;;; Windows
(setq tags-add-tables nil)

(use-package winner
  :bind
  (("S-<left>" . windmove-left)
   ("S-<right>" . windmove-right)
   ("S-<up>" . windmove-up)
   ("S-<down>" . windmove-down))
  :init (winner-mode 1))

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
(use-package fullframe)

(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c c". evilnc-copy-and-comment-lines)))

(use-package anzu
  :init (global-anzu-mode 1)
  :diminish anzu-mode
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

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
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
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
  :diminish projectile-mode
  :defer 5
  :init
  (progn
    (my-key-chord-define-global "pf" 'projectile-find-file)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (projectile-mode +1)))

(use-package slime
  :defer t
  :bind (("C-c y" . hyperspec-lookup))
  :init
  (progn
    (defvar common-lisp-hyperspec-root)
    (defvar inferior-lisp-program)
    (use-package slime-autoloads)
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
  :bind (("C-z" . avy-goto-char-2))
  :init (setq avy-all-windows 'all-frames))

(use-package flycheck
  :defer t
  :bind (:map flycheck-mode-map ("C-S-<next>" . flycheck-next-error)))

(use-package tuareg
  :mode ("\\.m[lf][ily]?$" . tuareg-mode)
  :bind
  (:map
   tuareg-mode-map
   ("C-c ." . nil)
   ("C-c /" . nil))
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
                        :weight 'bold)
    (use-package ocp-indent
      :demand t
      :bind (:map tuareg-mode-map ("C-=" . ocp-indent-buffer)))))

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
(use-package image-mode
  :config
  (use-package image+
    :init
    (progn
      (define-key image-mode-map (kbd "+") 'imagex-sticky-zoom-in)
      (define-key image-mode-map (kbd "-") 'imagex-sticky-zoom-out))))

(use-package yasnippet
  :defer t
  :config (yas-reload-all))

(use-package key-chord
  :init
  (progn
    (my-key-chord-define-global "fk" 'abort-recursive-edit)
    (my-key-chord-define-global "jk" 'execute-extended-command)
    (my-key-chord-define-global "fj" 'find-file)
    (my-key-chord-define-global "f." 'repeat)
    (my-key-chord-define-global "fb" 'switch-to-buffer)
    (my-key-chord-define-global "ha" 'apropos-command)
    (my-key-chord-define-global "hl" 'describe-function)
    (my-key-chord-define-global "hv" 'describe-variable)
    (my-key-chord-define-global "hk" 'describe-key)
    (my-key-chord-define-global "fh" 'recentf-open-files)
    (defun my-key-chord-setup ()
      (key-chord-mode +1)
      (mapc (lambda (pair) (key-chord-define-global (car pair) (cdr pair)))
            my-key-chords-alist))
    (with-eval-after-load 'init
      (my-key-chord-setup))))

(use-package composable
  :diminish composable-mode
  :init (composable-mode 1)
  :bind
  (:map
   composable-mode-map
   ([remap upcase-region] . composable-upcase-region)
   ([remap downcase-region] . composable-downcase-region)
   ("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package diff-hl
  :init
  (progn
    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
    (diff-hl-flydiff-mode 1)
    (set-face-attribute 'diff-hl-insert nil :inverse-video t)
    (set-face-attribute 'diff-hl-delete nil :inverse-video t)
    (set-face-attribute 'diff-hl-change nil :inverse-video t))
  :bind
  (("C-M-[" . diff-hl-previous-hunk)
   ("C-M-]" . diff-hl-next-hunk)))

(use-package multiple-cursors
  :bind
  (("C-M-." . mc/mark-next-like-this)
   ("C-M-," . mc/mark-previous-like-this)
   ("C-M-/" . mc/mark-all-like-this)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit)))

(use-package hi-lock
  :diminish "hl"
  :init
  (defun my-unhighlight-all ()
    "Unhighlight all hi-lock highlighted symbols."
    (interactive)
    (unhighlight-regexp t))
  :bind
  (:map
   hi-lock-map
   ("C-z ." . highlight-symbol-at-point)
   ("C-z r" . unhighlight-regexp)
   ("C-z u" . my-unhighlight-all)))

(use-package eshell
  :init
  (progn
    (fset 'my-recursive-edit-eshell
          (my-recursive-edit-preserving-window-config
            (unwind-protect
                (progn
                  (popwin-mode -1)
                  (save-some-buffers)
                  (delete-other-windows)
                  (my-eshell-here))
              (popwin-mode 1))))

    (defun my-eshell-here ()
      "Opens up a new shell in the directory associated with the
current buffer's file, or switch to it when it already exists.

The eshell buffer is renamed to match that directory to make
multiple eshell windows easier."
      (interactive)
      (let* ((file-name (buffer-file-name))
             (parent (if file-name
                         (file-name-directory file-name)
                       default-directory))
             (eshell-buffer-name (concat "*eshell: " parent "*"))
             (buf (get-buffer eshell-buffer-name)))
        (if buf
            (pop-to-buffer buf)
          (eshell "*eshell: ``new''*")
          (rename-buffer eshell-buffer-name)))))

  :config
  (progn
    (add-hook 'eshell-mode-hook 'my-eshell-face-setup)
    (defun my-eshell-face-setup ()
      (face-remap-add-relative 'default :foreground "white" :background "#363033")
      (set-face-foreground 'eshell-ls-directory "SkyBlue"))

    (defun eshell/gl ()
      (insert "git log --all --decorate --oneline --graph --color -n 5"))

    (defun eshell/gd ()
      (insert "git diff --cached | cat")
      (backward-char 14))

    (defun eshell/up ()
      (insert "git stash && git pull && git stash pop"))

    (defun eshell/amen ()
      (insert "git commit --amend --no-edit")
      (backward-char 17))

    (defun eshell/q ()
      (insert "exit"))

    (defun my-eshell-kill-line-backward ()
      (interactive)
      (my-kill-line-backward 'eshell-bol))

    (defun my-eshell-keymap-setup ()
      (and eshell-mode-map
           (define-key eshell-mode-map
             [remap my-kill-line-backward]
             'my-eshell-kill-line-backward)))
    (add-hook 'eshell-mode-hook #'my-eshell-keymap-setup))
  :bind
  (("<f1>" . my-eshell-here)
   ("<M-f1>" . my-recursive-edit-eshell)))

(use-package popwin
  :demand t
  :bind ("M-<down>" . popwin:close-popup-window)
  :init
  (setq popwin:popup-window-height 0.33)
  :config
  (setq popwin:special-display-config
        '(("*Miniedit Help*" :noselect t)
          help-mode
          (completion-list-mode :noselect t)
          (compilation-mode :noselect t :stick t)
          (grep-mode :noselect t :stick t)
          (occur-mode :noselect t :stick t :position right :width 0.35)
          ("*Pp Macroexpand Output*" :noselect t)
          "*Shell Command Output*"
          (" *undo-tree*" :width 60 :position right)
          ((lambda (buf)
             (string-prefix-p "*eshell: " (buffer-name buffer)))
           :dedicated t :position bottom :stick t)))
  (popwin-mode 1))

(provide 'my-init)
;;; my-init.el ends here
