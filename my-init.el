;;; my-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'use-package)
(setq use-package-verbose t)


;;; Basic configuration
(setq line-number-mode t)
(setq column-number-mode t)
(setq indent-tabs-mode nil)
(setq echo-keystrokes 0.3)
(setq recenter-positions '(top middle bottom))
(setq set-mark-command-repeat-pop t)
(setq completion-cycle-threshold 5)
(setq initial-scratch-message nil)
(setq mouse-wheel-progressive-speed nil)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq inhibit-startup-message t)
(setq auto-save-default nil)
(setq frame-title-format (list mode-line-modified " %b  " '(:eval vc-mode)))
(setq minibuffer-depth-indicate-mode t)
(setq tags-add-tables nil)
(setq load-prefer-newer t)
(setq switch-to-visible-buffer nil)
(setq blink-matching-paren 't)
(set-default-coding-systems 'utf-8)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(defalias 'display-startup-echo-area-message 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))
(keyboard-translate ?\C-p ?\C-h)
(keyboard-translate ?\C-h ?\C-p)

(add-to-list 'completion-styles 'partial-completion)

(progn
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
          compile-command)))

(progn
  (global-visual-line-mode +1)
  (setf (cdr visual-line-mode-map) nil)
  (diminish 'visual-line-mode))


;;; Windows
(defun my-delete-side-windows ()
  (interactive)
  (walk-windows (lambda (win)
                  (when (window-at-side-p win)
                    (delete-window win)))
                nil
                (selected-frame)))

(defmacro my-balance-after (orig-fun)
  (let ((interactive-form (interactive-form orig-fun))
        (gensym (intern (concat "my-" (symbol-name orig-fun)))))
    `(progn
       (define-key global-map [remap ,orig-fun] ',gensym)
       (defun ,gensym (&rest args)
         ,interactive-form
         (apply #',orig-fun args)
         (balance-windows)))))

(defun my-goto-buffer (buffer-name)
  "Select buffer named BUFFER-NAME."
  (select-window (split-window-vertically))
  (switch-to-buffer-other-window (get-buffer-create buffer-name)))

(my-balance-after split-window-right)
(my-balance-after split-window-below)


;;; MS Windows utilities
(let ((cygwin-root "c:"))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))
    (define-key global-map (kbd "C-c u") 'my-dos2unix)
    (defun my-dos2unix ()
      (interactive)
      (and buffer-file-name
           (shell-command (format "dos2unix %s" buffer-file-name))))
    (setq shell-file-name "bash")
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))


;;; Keybindings
(define-key global-map (kbd "C-<f4>") 'my-kmacro-end-or-call-macro-infinity)
(define-key global-map (kbd (format "<C-%s>" mouse-wheel-down-event)) 'text-scale-increase)
(define-key global-map (kbd (format "<C-%s>" mouse-wheel-up-event)) 'text-scale-decrease)
(define-key global-map (kbd "<escape> <escape>") 'my-delete-side-windows)
(define-key global-map (kbd "C-x n s") 'my-narrow-to-sexp)
(define-key global-map (kbd "C-M-h") 'backward-kill-sexp)
(define-key global-map (kbd "C-h g") 'my-google-search)
(define-key global-map (kbd "C-x K") 'my-other-window-kill-buffer)
(define-key global-map (kbd "C-h C-k") 'describe-key)
(define-key global-map (kbd "C-c C-v") 'my-insert-buffer-name)
(define-key global-map (kbd "C-c k") 'delete-frame)
(define-key global-map (kbd "C-c n") 'make-frame)
(define-key global-map (kbd "M-n") 'my-scroll-up)
(define-key global-map (kbd "M-h") 'my-scroll-down)
(define-key global-map (kbd "S-<next>") 'my-scroll-up-other-window)
(define-key global-map (kbd "S-<prior>") 'my-scroll-down-other-window)
(define-key global-map (kbd "C-M-<backspace>") 'my-clone-line)
(define-key global-map (kbd "C-c C-M-<up>") 'raise-sexp)
(define-key global-map (kbd "C-c C-M-u") 'raise-sexp)
(define-key global-map (kbd "C-c .") 'repeat)
(define-key global-map (kbd "M-=") 'align-regexp)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "C-x g") 'move-to-column)
(define-key global-map (kbd "M-`") 'my-other-window-or-switch-buffer)
(define-key global-map (kbd "C-c /") 'rgrep)
(define-key global-map (kbd "M-%") 'query-replace-regexp)
(define-key global-map (kbd "M-k") 'my-copy-line)
(define-key global-map (kbd "M-<f4>") 'my-name-last-kbd-macro)
(define-key global-map (kbd "C-<prior>") 'previous-error)
(define-key global-map (kbd "C-<next>") 'next-error)
(define-key global-map (kbd "C-S-<right>") 'my-next-beginning)
(define-key global-map (kbd "C-S-<left>") 'my-previous-end)
(define-key global-map (kbd "M-SPC") 'my-space-and-back)
(define-key global-map (kbd "C-<return>") (kbd "<return>"))
(define-key global-map (kbd "M-<return>") (kbd "<return>"))
(define-key global-map (kbd "C-M-<return>") (kbd "<return>"))
(define-key global-map (kbd "C-c F") 'my-toggle-debug)
(define-key global-map (kbd "C-c C-r")  'my-revert-buffer-noconfirm)
(define-key global-map (kbd "C-c (") 'delete-pair)
(define-key global-map (kbd "C-S-u") 'upcase-region)
(define-key global-map (kbd "C-S-l") 'downcase-region)
(define-key global-map (kbd "C-S-k") 'my-kill-line-backward)
(define-key global-map (kbd "C-c <f5>") 'revert-buffer)
(define-key global-map (kbd "<mode-line> <mouse-2>") 'my-goto-*scratch*)
(define-key global-map (kbd "<f2>") 'rename-buffer)
(define-key global-map (kbd "M-j") 'my-delete-indentation-forward)


;;; Appearance
(progn
  (and (fboundp 'fringe-mode) (fringe-mode -1))
  (and (fboundp 'tooltip-mode) (tooltip-mode -1))
  (and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (and (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (and (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
  (setq use-dialog-box nil)
  (setq pop-up-windows nil))

(setq-default mode-line-format
              (mapcar (lambda (elt)
                        (if (and (stringp elt) (string-match "  +" elt)) " " elt))
                      mode-line-format))

(defun my-visual-ring-bell ()
  (let ((face-background (face-background 'mode-line))
        (face-foreground (face-foreground 'mode-line)))
    (unwind-protect
        (progn
          (set-face-background 'mode-line "DodgerBlue")
          (set-face-foreground 'mode-line "White")
          (sit-for 0.1))
      (set-face-background 'mode-line face-background)
      (set-face-foreground 'mode-line face-foreground))))
(setq ring-bell-function 'my-visual-ring-bell)

(progn
  (load-theme 'paper t)
  (dolist (face (face-list))
    (set-face-underline face nil)
    (set-face-italic face nil))
  (let ((face 'font-lock-doc-face))
    (set-face-foreground face "DodgerBlue")
    (set-face-background face "ghost white"))
  (let ((face 'font-lock-comment-face))
    (set-face-foreground face "grey")
    (set-face-background face "ghost white"))
  (let ((face 'font-lock-constant-face))
    (set-face-foreground face "dark green"))
  (let ((face 'success))
    (set-face-foreground face "ForestGreen")
    (set-face-bold face t))
  (dolist (face '(mode-line-buffer-id mode-line-highlight))
    (set-face-bold face t))
  (with-eval-after-load 'dired
    (set-face-foreground 'dired-directory "blue"))
  (with-eval-after-load 'hl-line
    (set-face-background 'hl-line "honeydew")
    (set-face-foreground 'hl-line "orange"))
  (with-eval-after-load 'diff-mode
    (set-face-foreground 'diff-added "green4")
    (set-face-foreground 'diff-removed "red3")))

(defvar my-default-font
  (cond ((eq 'darwin system-type) "Menlo 18")
        (t "DejaVu Sans Mono 14"))
  "The font used almost everywhere.")
(with-eval-after-load 'init
  (add-to-list 'default-frame-alist '(height . 30))
  (add-to-list 'default-frame-alist '(width . 80))
  (add-to-list 'initial-frame-alist '(top . 20))
  (add-to-list 'initial-frame-alist '(left . 120))
  (add-to-list 'default-frame-alist `(font . ,my-default-font)))


;;; defuns
(let ((depth 1))
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
      (when (< depth 20) (g 2)))

    (defun my-selective-display-decrease ()
      "Decrease the cap for `toogle-selective-display'.

See `my-selective-display-toggle' and `my-selective-display-increase'."
      (interactive)
      (when (> depth 1) (g -2)))))

(defun my-delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(defun my-set-indentation (n)
  (interactive "nset indentation to: ")
  (setq c-basic-offset n))

(defvar my-narrowed-buffers nil)
(defun my-kill-buffer-on-widen ()
  (when (member (current-buffer) my-narrowed-buffers)
    (kill-buffer))
  (setq my-narrowed-buffers
        (cl-remove-if-not #'buffer-live-p my-narrowed-buffers)))
(advice-add 'widen :after #'my-kill-buffer-on-widen)

(defun my-narrow-to-sexp ()
  (interactive)
  (let ((start (point))
        (end (progn (forward-sexp) (point))))
    (goto-char start)
    (move-beginning-of-line 1)
    (let ((clone-buffer (clone-indirect-buffer nil nil))
          (narrow-start (point)))
      (with-current-buffer clone-buffer
        (goto-char start)
        (narrow-to-region narrow-start end)
        (push clone-buffer my-narrowed-buffers)
        (switch-to-buffer clone-buffer)))))

(defun my-recentf-command () (interactive))
(cl-flet ((always-yes (&rest _) t))
  (defun my-no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(defun my-occur-skip-gribberish-hook (&rest _)
  (when isearch-mode (isearch-exit))
  (select-window (get-buffer-window "*Occur*"))
  (goto-char (point-min))
  (next-logical-line 1)
  (recenter 0)
  (select-window (next-window)))
(advice-add 'occur :after #'my-occur-skip-gribberish-hook)

(defun my-revert-buffer-noconfirm ()
  (interactive)
  (revert-buffer t t))

(defun my-kmacro-end-or-call-macro-infinity ()
  (interactive)
  (kmacro-end-or-call-macro 99))

(defun my-find-init-file ()
  (interactive)
  (find-file my-main-init-file))

(defun my-find-project ()
  (interactive)
  (find-file (expand-file-name "projects" (getenv "HOME"))))

(defun my-find-shell-config-file ()
  (interactive)
  (find-file (expand-file-name ".bashrc" (getenv "HOME"))))

(defun my-bury-buffer (orig-fun &rest args)
  "Bury *scratch* buffer instead of killing it."
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (bury-buffer)
    (apply orig-fun args)))
(advice-add 'kill-buffer :around #'my-bury-buffer)

(defun my-toggle-debug ()
  "Change the value of `debug-on-error'."
  (interactive)
  (message "`debug-on-error' set to %S" (cl-callf not debug-on-error)))

(defun my-view-echo-area-messages (arg)
  (interactive "P")
  "With prefix argument, open the buffer in a new frame."
  (and arg (make-frame))
  (view-echo-area-messages))
(define-key global-map [remap view-echo-area-messages]
  'my-view-echo-area-messages)

(defun my-name-last-kbd-macro ()
  (interactive)
  (let ((name (read-string "Name for last kbd macro: " "test")))
    (name-last-kbd-macro (intern name))
    (kill-new name)))

(defun my-next-beginning ()
  "Go to the beginning of the next word."
  (interactive)
  (forward-word 2)
  (backward-word 1))

(defun my-previous-end ()
  "Go to the end of the previous word."
  (interactive)
  (backward-word 2)
  (forward-word 1))

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

(defmacro my-with-other-window (&rest body)
  `(let ((buf-name (buffer-name))
         (orig-window (selected-window)))
     (select-window (next-window))
     (if (string= buf-name (buffer-name))
         (error "No next window")
       (unwind-protect
           (progn ,@body)
         (select-window orig-window)))))
(put 'my-with-other-window 'lisp-indent-function 0)

(defun my-scroll-up-command (&optional arg)
  (interactive))

(defun my-scroll-down-command (&optional arg)
  (interactive))

(fset 'my-scroll-up-command 'scroll-up)
(fset 'my-scroll-down-command 'scroll-down)

(define-key global-map [remap scroll-up-command] 'my-scroll-up-command)
(define-key global-map [remap scroll-down-command] 'my-scroll-down-command)

(defun my-scroll-down-other-window (&optional arg)
  (interactive)
  (my-with-other-window
    (my-scroll-down-command arg)))

(defun my-scroll-up-other-window (&optional arg)
  (interactive)
  (my-with-other-window
    (my-scroll-up-command arg)))

(defmacro my-save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'my-save-column 'lisp-indent-function 0)

(defun my-clone-line ()
  (interactive)
  (my-save-column
    (copy-region-as-kill (line-beginning-position)
                         (goto-char (line-end-position)))
    (newline)
    (yank)
    (pop kill-ring)))

(defun my-kill-buffer ()
  "Kill buffer without confirmation."
  (interactive)
  (kill-buffer nil))
(define-key global-map [remap kill-buffer] 'my-kill-buffer)

(defun my-copy-line (&optional n)
  "Copy from point to the end of line."
  (interactive "p")
  (copy-region-as-kill (point) (line-end-position n)))

(defun my-space-and-back ()
  (interactive)
  (insert " ")
  (backward-char 1))

(defun my-other-window-or-switch-buffer ()
  "Call `other-window' if more than one window is visible, switch
to next buffer otherwise."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer nil)
    (other-window 1)))

(defun my-kill-region-or-backward-word (&rest _)
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(define-key global-map [remap kill-region] 'my-kill-region-or-backward-word)

(defun my-other-window-kill-buffer ()
  (interactive)
  (my-with-other-window
    (kill-buffer (current-buffer))))

(defun my-yank-clone (orig-fun &rest args)
  "With C-u C-u as prefix argument ARG, clone either the active
region (if any) or the next sexp."
  (interactive "*P")
  (if (not (equal '((16)) args))
      (apply orig-fun args)
    (unless (region-active-p)
      (mark-sexp))
    (let ((hi (region-end)))
      (copy-region-as-kill (region-beginning) hi)
      (goto-char hi)
      (newline nil t)
      (apply orig-fun nil))))
(advice-add 'yank :around #'my-yank-clone)

(progn
  (defvar my-untabify-this-buffer)
  (defun my-yank-and-reindent-hook (&rest _)
    (and my-untabify-this-buffer (indent-region (mark) (point))))

  (defun my-toggle-untabify-this-buffer ()
    "Toggle untabification of the current buffer."
    (interactive)
    (setq-local my-untabify-this-buffer (not my-untabify-this-buffer))
    (message "%s %s"
             (if my-untabify-this-buffer "Untabify" "Don't untabify")
             (buffer-name)))
  (defun my-untabify-buffer ()
    "Untabify the current buffer and delete trailing whitespaces,
unless `my-untabify-this-buffer' is nil."
    (cond (my-untabify-this-buffer
           (whitespace-mode -1)
           (untabify (point-min) (point-max))
           (delete-trailing-whitespace))
          (t (whitespace-mode +1))))

  (make-variable-buffer-local 'my-untabify-this-buffer)
  (define-minor-mode my-untabify-mode
    "Untabify buffer on save. When not applicable, turn on `whitespace-mode'.

\\{my-untabify-mode-map}"
    :lighter (:eval (and my-untabify-this-buffer " untab"))
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c <tab>") 'my-toggle-untabify-this-buffer)
              map)
    (cond (my-untabify-mode
           (advice-add 'yank :after #'my-yank-and-reindent-hook)
           (setq my-untabify-this-buffer
                 (not (or (derived-mode-p 'makefile-mode)
                          (re-search-forward "\t" nil t))))
           (or my-untabify-this-buffer (whitespace-mode 1))
           (add-hook 'before-save-hook #'my-untabify-buffer nil t))
          (t
           (advice-remove 'yank #'my-yank-and-reindent-hook)
           (and my-untabify-this-buffer (whitespace-mode -1))
           (remove-hook 'before-save-hook #'my-untabify-buffer t))))
  (add-hook 'prog-mode-hook 'my-untabify-mode))

(defun my-save-all-buffers (&rest _)
  "Save all buffers."
  (interactive)
  (save-some-buffers t))
(define-key global-map [remap save-some-buffers] 'my-save-all-buffers)

(defun my-kill-line-backward (&optional arg)
  "The same as `kill-line', but backward (and reindent).

If non nil, ARG overrides the `back-to-indentation' function."
  (interactive)
  (let ((start (point)))
    (funcall (or arg 'back-to-indentation))
    (kill-region (point) start)))

(let (my-zap-to-char-last-arg)
  (defun my-zap-to-char ()
    "Case sensitive, repetition friendly version of `zap-to-char'."
    (interactive)
    (let ((case-fold-search nil)
          (arg (if (eq last-repeatable-command this-command)
                   my-zap-to-char-last-arg
                 (setq my-zap-to-char-last-arg (read-char "zap to char: ")))))
      (zap-to-char 1 arg))))
(define-key global-map [remap zap-to-char] 'my-zap-to-char)

(let (my-jump-to-char-last-arg)
  (defun my-jump-to-char ()
    "Jump to the next occurence of CHAR."
    (interactive)
    (let ((char (if (eq last-repeatable-command this-command)
                    my-jump-to-char-last-arg
                  (setq my-jump-to-char-last-arg (read-char "look for: ")))))
      (forward-char 1)
      (let ((case-fold-search t))
        (search-forward (char-to-string char) nil t))
      (backward-char 1))))

(defun my-previous-buffer ()
  "The current buffer, or the previous one if in the minibuffer."
  (if (minibufferp)
      (other-buffer (current-buffer) 1)
    (current-buffer)))

(defun my-insert-buffer-name ()
  "Insert the previous buffer name.  Useful for compilation."
  (interactive)
  (insert (buffer-name (my-previous-buffer))))


;;; Packages
(use-package fullframe)

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode +1))

(use-package diff
  :init (add-hook 'diff-mode-hook 'read-only-mode))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (fullframe ibuffer quit-window))

(use-package package
  :init
  (fullframe list-packages quit-window))

(use-package vc
  :bind
  (("<f7>" . vc-diff)
   ("C-<f7>" . vc-root-diff)
   :map
   vc-prefix-map
   ("q" . my-vc-add-current-buffer))
  :config
  (progn
    (advice-add 'vc-diff :before #'my-save-all-buffers)
    (advice-add 'diff-apply-hunk :around #'my-no-confirm)
    (use-package vc-dir
      :config
      (fullframe vc-dir quit-window)
      :bind
      (([remap vc-dir] . my-vc-dir-root)
       ("C-M-<f7>" . vc-dir)
       :map vc-dir-mode-map
       ("d" . vc-diff)))
    (defun my-vc-add-current-buffer ()
      (interactive)
      (when (and buffer-file-name (stringp buffer-file-name))
        (let ((filename (file-name-nondirectory buffer-file-name)))
          (vc-git--call nil "add" "--" filename)
          (message "Added %s!" filename))))
    (defun my-vc-dir-root ()
      (interactive)
      (when-let (root (vc-root-dir))
        (vc-dir root)))))

(use-package ediff-wind
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package elec-pair
  :init
  (progn
    (defvar electric-pair-pairs)
    (defvar show-paren-delay)
    (add-hook 'prog-mode-hook 'electric-pair-local-mode))
  :config
  (progn
    (and (fboundp 'electric-pair-mode) (electric-pair-mode -1))
    (add-to-list 'electric-pair-pairs '(?\{ . ?\}))
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)))

(use-package paren
  :init
  (progn
    (setq show-paren-delay 0)
    (show-paren-mode 1)))

(use-package key-chord
  :init
  (progn
    (defvar my-key-chords-alist nil
      "A list of key chords bindings.

Each binding is a list of 3 elements: KEYS, KEYMAP, COMMAND.
KEYS is string of length 2; KEYMAP defaults to the global map.")
    (defun my-key-chord-define (keymap keys command)
      (push (list keymap keys command) my-key-chords-alist)
      (and (fboundp 'my-key-chord-setup) (my-key-chord-setup)))
    (my-key-chord-define global-map "hv" 'describe-variable)
    (my-key-chord-define global-map "hk" 'describe-key)
    (my-key-chord-define global-map "hj" 'describe-function)
    (my-key-chord-define global-map "fy" 'my-find-init-file)
    (my-key-chord-define global-map "fb" 'my-find-shell-config-file)
    (my-key-chord-define global-map "fq" 'my-find-project)
    (my-key-chord-define global-map "fh" 'my-recentf-command))
  :config
  (progn
    (defun my-key-chord-setup ()
      (with-eval-after-load 'init
        (or key-chord-mode (key-chord-mode +1))
        (dolist (mapping my-key-chords-alist)
          (apply #'key-chord-define mapping))))
    (my-key-chord-setup)))

(use-package hippie-expand
  :bind
  (("C-M-/" . my-expand-lines))
  :init
  (progn
    (defvar my-expand-lines)
    (fset 'my-expand-lines
          (make-hippie-expand-function
           '(try-expand-line
             try-expand-line-all-buffers)))))

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  :config (winner-mode +1)
  :bind
  (("C-," . winner-undo)
   ("C-." . winner-redo)))

(use-package hydra
  :init
  (progn
    (defhydra my-selective-display (global-map "C-x")
      "Selective display"
      ("$" my-selective-display-toggle nil)
      ("0" my-selective-display-toggle "on/off" :bind nil)
      ("+" my-selective-display-increase "more" :bind nil)
      ("=" my-selective-display-increase nil :bind nil)
      ("-" my-selective-display-decrease "less" :bind nil)
      ("q" nil "quit"))
    (defhydra my-previous-next-buffer-repeat (global-map "C-x")
      "Buffers"
      ("<left>" previous-buffer "previous")
      ("<right>" next-buffer "next")
      ("k" my-kill-buffer "kill")
      ("q" nil "quit"))))

(use-package dired
  :bind (:map dired-mode-map ("M-<down>" . dired-find-file))
  :init
  (progn
    (setq dired-dwim-target t)
    (use-package dired-x
      :commands (dired-omit-mode dired-jump)
      :init
      (add-hook 'dired-mode-hook 'dired-omit-mode)
      :bind
      (("M-<up>" . dired-jump)
       ("C-x C-j" . dired-jump)
       :map dired-mode-map
       ("M-<up>" . dired-jump)
       ([mouse-2] . my-dired-mouse-find-file)
       ("C-x C-j" . dired-jump)))
    (defun my-dired-mouse-find-file (event)
      "In Dired, visit the file or directory name you click on."
      (interactive "e")
      (let (window pos file)
        (save-excursion
          (setq window (posn-window (event-end event))
                pos (posn-point (event-end event)))
          (if (not (windowp window))
              (error "No file chosen"))
          (set-buffer (window-buffer window))
          (goto-char pos)
          (setq file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (or (and (cdr dired-subdir-alist)
                     (dired-goto-subdir file))
                (progn
                  (select-window window)
                  (dired file)))
          (select-window window)
          (find-file (file-name-sans-versions file t)))))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-listing-switches "-alh")
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always))
  :config
  (progn
    (set-face-attribute 'dired-ignored nil
                        :foreground "gray60")))

(use-package ffap
  :bind
  ("<S-mouse-2>" . ffap-at-mouse)
  :config
  (progn
    (my-key-chord-define global-map "fj" 'find-file-at-point)
    (defun my-ffap-prompter-noconfirm (fn &optional guess)
      "Remove confirmation."
      (and (fboundp 'xref-push-marker-stack) (xref-push-marker-stack))
      (or guess (ffap-guesser) (funcall fn)))
    (advice-add 'ffap-prompter :around #'my-ffap-prompter-noconfirm)))

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

(defmacro my-defweb-search (name engine-url)
  `(defun ,name (w)
     ,(format "Search on %s the word at point."
              (substring engine-url 0 (string-match "/" engine-url)))
     (interactive (list (read-string "query: " (my-prompt))))
     (browse-url (format "https://%s%s"
                         ,engine-url
                         (replace-regexp-in-string search-whitespace-regexp
                                                   "+"
                                                   w)))))
(put 'my-defweb-search 'lisp-indent-function 'defun)

(my-defweb-search my-google-search
  "www.google.com/search?q=")
(my-defweb-search my-hoogle-search
  "www.haskell.org/hoogle/?hoogle=")
(my-defweb-search my-msdn-search
  "social.msdn.microsoft.com/Search/en-US?query=")

(use-package grep
  :init
  (progn
    (defun my-disable-jump-to-error ()
      "Disable `compilation-auto-jump-to-next' local variable."
      (kill-local-variable 'compilation-auto-jump-to-next))
    (add-hook 'grep-mode-hook 'my-disable-jump-to-error))

  :config
  (progn
    (use-package compile
      :init
      (progn
        (setq compilation-ask-about-save nil)
        (setq compilation-always-kill t)
        (setq compilation-scroll-output 'first-error))
      :bind
      (("<f5>" . recompile)))

    (add-to-list 'display-buffer-alist
                 '("*compilation*\\|*Occur*\\|*xref*\\|*ivy-occur\\|*Cargo"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (reusable-frames . nil)
                   (side . bottom)
                   (window-height . 0.3)))

    (defun my-compile-finish-hook (buf status)
      (with-current-buffer buf
        (goto-char (point-min))
        (and (string= (buffer-name buf) "*compilation*")
             (string-match "^finished\\b" status)
             (not (re-search-forward "warning" nil t))
             (sit-for 0.4)
             (delete-window (get-buffer-window buf)))))

    (add-hook 'compilation-finish-functions #'my-compile-finish-hook)))

(use-package iedit
  :init
  (progn
    (defun my-iedit-occur (&optional nlines)
      (interactive "p")
      (when iedit-mode
        (occur iedit-initial-string-local nlines))))
  :bind
  (("C-;" . iedit-mode)
   :map iedit-mode-keymap
   ("C-s" . iedit-next-occurrence)
   ("C-r" . iedit-prev-occurrence)
   ("M-o" . my-iedit-occur)))


;;; elisp
(use-package ielm
  :init
  (progn
    (setq initial-major-mode 'emacs-lisp-mode)
    (setq eval-expression-print-level nil)
    (use-package elisp-mode
      :bind
      ("C-c C-z" . ielm)
      (:map
       emacs-lisp-mode-map
       ("C-c C-k" . eval-buffer))))
  :bind
  (:map ielm-map
        ("C-c C-z" . bury-buffer)))


;;; Mouse
(use-package mouse
  :bind
  (("<S-mouse-1>" . my-acme-search-forward)
   ("C-c ." . my-delete-mouse-secondary-overlay))
  :config
  (progn
    (defun my-delete-mouse-secondary-overlay ()
      "Remove the overlay create by `mouse-drag-secondary'."
      (interactive)
      (delete-overlay mouse-secondary-overlay))
    (global-unset-key (kbd "<S-down-mouse-1>"))
    (global-unset-key (kbd "<S-down-mouse-3>"))

    (setq mouse-drag-copy-region t)
    (setq mouse-yank-at-point t)

    (defun my-acme-search-forward (click)
      "Move mouse to the next occurence of either the active region,
or the symbol at point, and highlight it."
      (interactive "e")
      (let ((sym (if (region-active-p)
                     (buffer-substring (mark) (point))
                   (mouse-set-point click)
                   (thing-at-point 'filename))))
        (cond ((not (and sym (stringp sym))) nil)
              ((file-readable-p sym)
               (special-display-popup-frame (find-file-noselect sym nil nil nil)))
              (t
               (or (my-acme-search--move sym)
                   (let ((saved-point (point)))
                     (message "Wrapped search")
                     (goto-char (point-min))
                     (or (my-acme-search--move sym)
                         (goto-char saved-point))))))
        ;; Redisplay the screen if we search off the bottom of the window.
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

    (defun my-acme-search--move (sym)
      "Search from point for SYM and highlight it.

If there is no match, returns NIL."
      (push-mark-command nil t)
      (when (search-forward sym nil t)
        (my-acme-highlight-search sym)
        t))

    (defun my-acme-highlight-search (sym)
      "Set the region to the current search result."
      (set-mark (point))
      (search-backward sym nil t)
      (exchange-point-and-mark))))

(use-package mouse-copy
  :bind
  (("<C-down-mouse-1>" . mouse-drag-secondary-pasting)
   ("<C-S-down-mouse-1>" . mouse-drag-secondary-moving))
  :config
  (setq mouse-drag-copy-region t))

(use-package recentf
  :init
  (recentf-mode +1)
  :bind
  (("C-x C-h" . recentf-open-files))
  :config
  (progn
    (fset 'my-recentf-command 'recentf-open-files)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 150)))

(defun my-yank-diff (arg)
  "Yank a patch.

Yank, and then process the yanked region: remove the `+' that
start every line, kill the lines starting with `-', and reindent.

With prefix argument ARG, invert `+' and `-'."
  (interactive "P")
  (yank)
  (save-mark-and-excursion
   (while
       (and (<= (mark) (point))
            (progn
              (back-to-indentation)
              (cond ((if arg (looking-at "-") (looking-at "\+"))
                     (delete-char 1))
                    ((if arg (looking-at "\+") (looking-at "-"))
                     (let* ((this-line (line-beginning-position))
                            (next-line (progn (forward-line 1) (point))))
                       (delete-region this-line next-line))))
              ;; XXX: avoids an infinite loop in empty buffers
              (<= 0 (forward-line -1)))))
   (indent-region (point) (mark))))

(ert-deftest my-yank-diff-test ()
  (should (string= " test\n test"
                   (with-temp-buffer
                     (erase-buffer)
                     (kill-new "+ test\n+ test")
                     (my-yank-diff nil)
                     (buffer-string)))))


;;; Isearch
(use-package isearch
  :diminish isearch-mode
  :bind
  (("M-o" . my-occur-region)
   ("C-S-s" . isearch-forward-symbol-at-point)
   :map
   minibuffer-local-isearch-map
   ("TAB" . isearch-complete-edit)
   :map
   isearch-mode-map
   ("<escape>" . my-isearch-suspend)
   ("<f3>" . isearch-repeat-forward)
   ("S-<f3>" . isearch-repeat-backward)
   ("M-o" . isearch-occur)
   ("TAB" . isearch-complete)
   ("M-e" . isearch-toggle-symbol)
   ("M-<" . my-isearch-beginning-of-buffer)
   ("M->" . my-isearch-end-of-buffer))
  :init
  (progn
    (defun my-occur-region ()
      "Send region to occur when activated."
      (interactive)
      (and (my-isearch-region) (call-interactively 'isearch-occur)))

    (defun my-isearch-suspend ()
      "Invoke the editor command loop recursively, during Isearch.
Use `\\[exit-recursive-edit]' to end the recursive edit and
resume searching from there.  Or use `\\[abort-recursive-edit]' to
exit the recursive edit and cancel the previous search."
      (interactive)
      (with-isearch-suspended (recursive-edit)))

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

    (defun my-isearch-exit-beginning ()
      "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
      (when (and isearch-forward
                 (number-or-marker-p isearch-other-end)
                 (not mark-active)
                 (not isearch-mode-end-hook-quit))
        (goto-char isearch-other-end)))

    (defun my-isearch-region ()
      "Send region to isearch when activated.

Returns t if the region was activated, nil otherwise."
      (when (region-active-p)
        (deactivate-mark)
        (kill-ring-save (region-beginning) (region-end))
        (goto-char (region-beginning))
        (isearch-mode t)
        (isearch-yank-pop)
        t))
    (defun my-isearch-region-hook (orig-fun &rest args)
      "Send region to isearch when activated.

Otherwise, apply ORIG-FUN to ARGS."
      (or (my-isearch-region) (apply orig-fun args)))
    (add-hook 'isearch-mode-end-hook #'my-isearch-exit-beginning)
    (advice-add 'isearch-forward :around #'my-isearch-region-hook)
    (advice-add 'isearch-backward :around #'my-isearch-region-hook)
    (advice-add 'isearch-forward-regexp :around #'my-isearch-region-hook)
    (advice-add 'isearch-backward-regexp :around #'my-isearch-region-hook)))

(use-package shell
  :init
  (setenv "PAGER" "cat")
  :bind
  (:map shell-mode-map
        ([remap dired] . my-shell-dired)
        ("<f2>" . my-rename-shell-buffer)
        ("<f5>" . comint-previous-input)
        ("<f6>" . compilation-shell-minor-mode))
  :config
  (progn
    (defun my-rename-shell-buffer (name)
      "Make a new buffer name for the current shell buffer based on NAME."
      (interactive "sBuffer name: ")
      (rename-buffer
       (if (string= "" name) "*shell*" (format "*shell %s*" name))))
    (defvar my-shell-prompt-pwd-regexp "\\(.*\\)$ "
      "How to get the $PWD from the shell prompt.

A cons cell with a regexp that captures one match.")
    (defun my-shell-pwd ()
      "Get the current value of $PWD."
      (pcase-let ((`(,lo . ,hi) comint-last-prompt))
        (when (and lo hi)
          (let* ((last-prompt (buffer-substring-no-properties lo hi))
                 (filename (and (string-match my-shell-prompt-pwd-regexp
                                              last-prompt)
                                (match-string 1 last-prompt))))
            (and (stringp filename) (expand-file-name filename))))))
    (defun my-shell-dired ()
      (interactive)
      "Run dired in $PWD."
      (dired (my-shell-pwd)))
    (setq-default comint-input-ignoredups +1)
    (unless window-system
           (define-key global-map (kbd "C-z") 'suspend-emacs))
    (defun my-end-of-buffer-hook (&rest _) (goto-char (point-max)))
    (advice-add 'comint-previous-input :before #'my-end-of-buffer-hook)
    (advice-add 'comint-next-input :before #'my-end-of-buffer-hook)))

(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c c". evilnc-copy-and-comment-lines)))

(use-package anzu
  :init (global-anzu-mode 1)
  :diminish anzu-mode
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package lispy
  :defer t
  :init
  (progn (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
         (add-hook 'lisp-mode-hook 'lispy-mode))
  :config
  (progn
    (lispy-set-key-theme '(special))))

(use-package magit
  :defer 15
  :bind
  (("C-x g" . magit-status)
   ("C-x v p" . magit-blame)
   ("M-<f7>" . magit-status))
  :config
  (add-to-list 'display-buffer-alist
               '("*magit"
                 (display-buffer-use-some-frame display-buffer-pop-up-frame)
                 (pop-up-frame-parameters . ((dedicated-to . magit)))
                 (reusable-frames . nil)
                 (frame-predicate . my-is-dedicated-to-magit)))
  (setq magit-bury-buffer-function 'bury-buffer)
  (defun my-is-dedicated-to-magit (frame)
    (let ((cell (assq 'dedicated-to (frame-parameters frame))))
      (and (consp cell) (eq (cdr cell) 'magit))))

  (advice-add 'magit-discard-hunk :around 'my-no-confirm)
  (set-face-foreground 'magit-diff-file-heading "blue")
  (defun my-create-tags ()
    (interactive)
    (let ((default-directory (or (magit-toplevel)
                                 (read-directory-name "Directory: "))))
      (eshell-command
       (format "find %s -type f -name %S | etags -"
               default-directory
               (read-string "file extension: " "*.[ch]")))))
  (dolist (command '(magit-commit magit-commit-amend magit-status))
    (advice-add command :before #'my-save-all-buffers))
  (setq magit-commit-ask-to-stage t)
  (setq magit-backup-mode nil))

(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (dotimes (i 9)
      (set-face-bold
       (intern (format "rainbow-delimiters-depth-%d-face" (1+ i))) t))
    (defun my-rainbow-delimiters-disable ()
      "Disable rainbow-delimiters mode."
      (rainbow-delimiters-mode -1))

    (add-hook 'shell-mode-hook #'my-rainbow-delimiters-disable)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground "red"
                        :inherit 'error
                        :box t)))

(use-package loccur
  :disabled t
  :bind
  (("M-s o" . loccur-current)
   ("M-s i" . loccur)
   ("M-s p" . loccur-previous-match)))

(use-package company
  :defer t
  :bind
  (:map
   company-mode-map
   ("C-\\" . company-complete))
  :init
  (defun my-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number
         (if (equal k "0")
             10
           (string-to-number k))))))
  (defun my-company-abort-with-space ()
    (interactive)
    (company-abort)
    (self-insert-command 1))
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    (let ((map company-active-map))
      (dotimes (x 10)
        (define-key map (format "%d" x) 'my-company-number))
      (define-key map (kbd " ") 'my-company-abort-with-space)
      (define-key map (kbd "<return>") nil)
      (define-key map (kbd "C-n") 'company-select-next)
      (define-key map (kbd "C-p") 'company-select-previous))
    (setq company-show-numbers t)
    (setq company-idle-delay nil)
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-flip-when-above t)))

(use-package ivy
  :demand t
  :diminish ivy-mode
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ("C-c b" . counsel-bookmark)
   ("<f8>" . my-counsel-rg)
   ("C-S-r" . ivy-resume)
   ("C-M-y". counsel-yank-pop)
   ("C-z" . counsel-switch-to-shell-buffer)
   ("C-c M-x" . counsel-M-x)
   ("<f10>" . counsel-git-change-worktree)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<next>" . ivy-scroll-up-command)
   ("<prior>" . ivy-scroll-down-command)
   ("<right>" . ivy-alt-done))
  :init
  (progn
    (setq counsel-describe-function-preselect 'ivy-function-called-at-point)
    (defun ivy-display-function-lv (text)
      (require 'lv)
      (let ((lv-force-update t))
        (lv-message
         (if (string-match "\\`\n" text)
             (substring text 1)
           text))))

    (advice-add 'counsel-switch-to-shell-buffer :around 'my-select-shell-buffer)
    (defun my-select-shell-buffer (orig-fun &rest args)
      (if-let ((buf (get-buffer "*shell*")))
          (switch-to-buffer buf)
        (apply orig-fun args)))

    (defun my-counsel-rg (p)
      (interactive "P")
      (let ((directory (if p (read-directory-name "counsel from directory: "))))
        (counsel-rg (my-prompt) directory)))

    (defun ivy-display-function-popup (text)
      (require 'popup)
      (with-ivy-window
        (popup-tip
         (substring text 1)
         :nostrip nil)))

    (setq ivy-display-functions-alist
          '((ivy-completion-in-region . ivy-display-function-lv)))
    (use-package shell
      :bind
      (:map shell-mode-map
            ("C-z" . bury-buffer)))
    (defvar ivy-use-virtual-buffers)
    (fset 'my-recentf-command 'ivy-switch-buffer)
    (setq completion-in-region-function #'ivy-completion-in-region)
    (setq counsel-rg-base-command
          (concat
           "rg --no-heading --line-number --vimgrep "
           "--path-separator / "
           "--max-columns 120 "
           "--color never "
           "%s ."))
    (setq ivy-use-virtual-buffers t))
  :config
  (progn
    (add-to-list 'ivy-initial-inputs-alist
                 '(counsel-switch-to-shell-buffer . "*shell*"))
    (add-to-list 'ivy-initial-inputs-alist
                 '(projectile-completing-read . "/"))
    (setq ivy-virtual-abbreviate 'full)
    (my-key-chord-define ivy-minibuffer-map "fh" 'ivy-avy)))

(use-package projectile
  :diminish projectile-mode
  :defer 5
  :init
  (progn
    (my-key-chord-define global-map "pf" 'projectile-find-file)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (projectile-mode +1)))

(use-package slime
  :bind
  (:map
   slime-mode-map
   ("C-c s" . slime-selector))
  :config
  (progn
    (defvar common-lisp-hyperspec-root)
    (defvar inferior-lisp-program)
    (use-package slime-autoloads)
    (setq inferior-lisp-program "sbcl")
    (add-hook 'comint-mode-hook 'rainbow-delimiters-mode)
    (setq common-lisp-hyperspec-root
          (format "file:///%s"
                  (expand-file-name "HyperSpec/" user-emacs-directory)))))

(use-package expand-region
  :defer t
  :bind (([remap mark-sexp] . er/expand-region))
  :config
  (setq expand-region-contract-fast-key "z"
        expand-region-reset-fast-key "x"
        expand-region-fast-keys-enabled t))

(use-package flycheck
  :defer t
  :bind (:map flycheck-mode-map ("C-S-<next>" . flycheck-next-error))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enable))))

(use-package tuareg
  :mode ("\\.m[lf][ily]?$" . tuareg-mode)
  :bind
  (:map
   tuareg-mode-map
   ("M-;" . nil)
   ("C-c ." . nil)
   ("C-c /" . nil))
  :config
  (progn
    (add-to-list 'face-remapping-alist
                 '(tuareg-font-lock-governing-face (:inherit font-lock-builtin-face)))
    (advice-add 'tuareg-mode :after #'my-preserve-comment-style)
    (defun my-preserve-comment-style () (setq comment-style 'indent))
    (use-package ocp-indent
      :demand t
      :bind (:map tuareg-mode-map ("C-=" . ocp-indent-buffer))
      :config
      (progn
        (add-hook 'tuareg-mode-hook 'ocp-setup-indent)))))

(use-package cc-vars
  :defer t
  :config
  (progn
    (defun my-c-setup ()
      "My setup for C."
      (defvar c-default-style)
      (defvar indent-tabs-mode)
      (setq c-default-style "linux")
      (setq c-basic-offset 8)
      (setq indent-tabs-mode nil)
      (use-package cc-mode
        :bind
        (:map
         c-mode-base-map
         ("C-c C-c" . compile)
         ("C-c C-a" . ff-find-other-file))))
    (add-hook 'c-initialization-hook #'my-c-setup)))

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

(use-package composable
  :disabled t
  :diminish composable-mode
  :init (composable-mode 1)
  :bind
  (:map
   composable-mode-map
   ([remap upcase-region] . composable-upcase-region)
   ([remap downcase-region] . composable-downcase-region)
   ("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package diff-hl
  :bind
  (("C-M-[" . diff-hl-previous-hunk)
   ("C-M-]" . diff-hl-next-hunk)
   :map diff-hl-mode-map
   ("S-<f7>" . diff-hl-revert-hunk))
  :config
  (progn
    (cl-flet ((my-diff-hl-on (&rest _) (turn-on-diff-hl-mode)))
      (dolist (fun '(diff-hl-previous-hunk diff-hl-next-hunk vc-diff))
        (advice-add fun :before #'my-diff-hl-on)))
    (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
    (add-hook 'diff-hl-mode-hook #'diff-hl-margin-mode)
    (advice-add 'diff-hl-diff-goto-hunk :before #'my-save-all-buffers)
    (advice-add 'diff-hl-revert-hunk :around #'my-no-confirm)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (use-package mc-mark-more
    :bind
    (:map
     mc/keymap
     ("C-s" . my-jump-to-char)
     ("C-," . mc/unmark-next-like-this)
     ("C-." . mc/skip-to-next-like-this)
     ("C-'" . mc-hide-unmatched-lines-mode))))

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

(use-package woman
  :bind
  ("C-c d" . woman))

(use-package battery
  :demand t
  :config
  (and (functionp battery-status-function)
       (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
           (display-battery-mode))))

(use-package macrostep
  :bind
  (:map
   lisp-interaction-mode-map
   ("C-c e" . macrostep-expand)
   :map
   emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)))

(use-package erlang
  :defer 15
  :init
  (progn
    (add-hook 'erlang-mode-hook #'my-insert-erl-module-stub)
    (defun my-insert-erl-module-stub ()
      (when (and (= (point-min) (point-max))
                 (buffer-file-name)
                 (string-suffix-p ".erl" (buffer-file-name)))
        (let ((module-name (file-name-base)))
          (insert (format "-module(%s).\n-export([]).\n\n"
                          module-name)))))))

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package hl-todo
  :init
  (progn
    (add-hook 'prog-mode-hook 'hl-todo-mode)
    (setq hl-todo-keyword-faces '(("TODO" . hl-todo)
                                  ("XXX" . hl-todo)
                                  ("NOTE" . hl-todo)
                                  ("HACK" . hl-todo)
                                  ("FIXME" . hl-todo))))
  :config
  (progn
    (hl-todo-set-regexp)
    (set-face-attribute 'hl-todo nil :foreground "deep pink" :background "yellow")))

(use-package hl-line
  :init
  (progn
    (add-hook 'next-error-hook 'next-error-buffer-hl-line)
    (defun next-error-buffer-hl-line ()
      "Turn on `hl-line-mode' in buffer `next-error-last-buffer'."
      (when (and next-error-last-buffer (buffer-live-p next-error-last-buffer))
        (with-current-buffer next-error-last-buffer
          (hl-line-mode 1))))))

(use-package visible-mark
  :preface
  (progn
    (defface visible-mark-active
      '((((type tty) (class mono)))
        (t (:background "magenta")))
      "Face for visible-mark."
      :group 'visible-mark)
    (setq visible-mark-faces
          (let ((i 0) faces)
            (dolist (color '("light green" "yellow" "light blue"))
              (push (eval `(defface ,(intern (format "visible-mark-face%d" (setq i (1+ i))))
                             '((((class color) (background light)) :background ,color))
                             "Face for visible-mark."
                             :group 'visible-mark))
                    faces)
              (message "defined face %s" (format "visible-mark-face%d" (1+ i))))
            (nreverse faces))))
  :init
  (progn
    (setq show-paren-priority -1)
    (setq visible-mark-max (length visible-mark-faces))
    (setq visible-mark-forward-max 2)
    (global-visible-mark-mode 1)))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode +1))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (add-hook 'smartparens-mode-hook 'my-smartparens-mode-setup)
    (defun my-smartparens-mode-setup ()
      (add-to-list 'sp-pairs
                   (cons t
                         (cl-delete-if
                          (lambda (plist)
                            (and (consp plist)
                                 (member (plist-get plist :open)
                                         '("`" "'"))))
                          (cdr (assoc t sp-pairs)))))
      (and (fboundp 'electric-pair-mode)
           electric-pair-mode
           (electric-pair-local-mode -1)))
    (add-hook 'prog-mode-hook 'smartparens-mode t)))

(use-package icomplete-mode
  :disabled t
  :init (icomplete-mode +1)
  :bind
  (:map icomplete-minibuffer-map
        ("C-s" . icomplete-forward-completions)
        ("C-r" . icomplete-backward-completions)))

(use-package ispell
  :config
  (progn
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "english")))

(use-package centimacro
  :init
  (setq centi-assign-key (kbd "C-<f3>")))

(use-package sh-script
  :init
  (progn
    (defun my-sh-quote-or-unquote ()
      "Wrap or unwrap the sexp at point inside a pair of double quotes."
      (interactive)
      (save-excursion
        (backward-sexp)
        (if (progn
              (backward-char 1)
              (looking-at "\""))
            (delete-char 1)
          (forward-char 1)
          (insert ?\"))
        (forward-sexp)
        (if (looking-at "\"") (delete-char 1) (insert ?\")))))
  :bind
  (:map sh-mode-map
        ("C-c C-z" . nil)
        ("C-c q" . my-sh-quote-or-unquote))
  :config
  (progn
    (setq sh-basic-offset 8)
    (setq sh-indentation 8)))

(use-package move-text
  :bind
  (("C-S-<down>" . move-text-down)
   ("C-S-<up>" . move-text-up)))

(use-package windmove
  :bind
  (("S-<left>"  . windmove-left)
   ("S-<right>" . windmove-right)
   ("S-<up>"    . windmove-up)
   ("S-<down>"  . windmove-down)))

(use-package mb-depth
  :init
  (progn
    (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode +1)))

(use-package vimish-fold
  :bind
  (("C-`" . vimish-fold)
   ("C-c f" . vimish-fold-delete-all)))

(use-package restart-emacs
  :bind
  (([remap save-buffers-kill-terminal] . my-restart-emacs))
  :init
  (progn
    (defun my-restart-emacs (arg)
      (interactive "P")
      (if arg (save-buffers-kill-terminal) (restart-emacs)))))

(use-package whitespace
  :config
  (progn
    (set-face-background 'whitespace-space-after-tab nil)
    (set-face-background 'whitespace-indentation nil)
    (set-face-foreground 'whitespace-line nil)
    (set-face-background 'whitespace-line nil)))

(use-package ace-link
  :init
  (ace-link-setup-default))

(use-package rust-mode
  :init
  (progn
    (use-package cargo
      :diminish cargo-minor-mode
      :bind
      (:map
       cargo-minor-mode-map
       ("<f5>" . cargo-process-repeat))
      :init
      (progn
        (add-hook 'rust-mode-hook 'cargo-minor-mode)))
    (use-package racer
      :diminish racer-mode
      :init
      (progn
        (add-hook 'rust-mode-hook 'racer-mode)))))

(use-package helpful
  :init
  (progn
    (my-key-chord-define global-map "hh" 'helpful-at-point)))

(use-package dired-sidebar
  :ensure t
  :bind
  ("<f6>" . dired-sidebar-toggle-sidebar))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package javadoc-lookup
  :bind
  ("C-h j" . javadoc-lookup))

(use-package pager
  :init
  (defun my-scroll-up-command-pager (&optional arg)
    "Apply `pager-page-down'.

If provided, do it ARG times."
    (interactive)
    (dotimes (_ (or arg 1))
      (pager-page-down)))
  (defun my-scroll-down-command-pager (&optional arg)
    "Apply `pager-page-up'.

If provided, do it ARG times."
    (interactive)
    (dotimes (_ (or arg 1))
      (pager-page-up)))
  (fset 'my-scroll-up-command 'my-scroll-up-command-pager)
  (fset 'my-scroll-down-command 'my-scroll-down-command-pager))

(provide 'my-init)
;;; my-init.el ends here
