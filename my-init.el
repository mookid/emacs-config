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
(setq frame-title-format (list "%f"))
(setq minibuffer-depth-indicate-mode t)
(setq tags-add-tables nil)
(setq load-prefer-newer t)
(setq switch-to-visible-buffer nil)
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

(global-auto-revert-mode +1)
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

(defmacro my-window-command (key buffer-name)
  "Defines a command to jump to the buffer designated by
BUFFER-NAME and bind it."
  (let* ((key (symbol-name key))
         (buffer-name (symbol-name buffer-name))
         (command-name (intern (concat "my-goto-" buffer-name))))
    `(progn
       (defun ,command-name ()
         ,(concat "Goto buffer `" buffer-name "'.")
         (interactive)
         (my-goto-buffer ,buffer-name))
       (define-key global-map (kbd ,(concat "C-c w " key)) ',command-name))))

(my-window-command g *grep*)
(my-window-command d *vc-diff*)
(my-window-command c *compilation*)
(my-window-command o *Occur*)
(my-window-command s *scratch*)
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
(define-key global-map (kbd "C-M-h") 'backward-kill-sexp)
(define-key global-map (kbd "<f6>") 'my-selective-display-toggle)
(define-key global-map (kbd "C-<f6>") 'my-selective-display-increase)
(define-key global-map (kbd "S-<f6>") 'my-selective-display-decrease)
(define-key global-map (kbd "C-h g") 'my-google-search)
(define-key global-map (kbd "C-x K") 'my-other-window-kill-buffer)
(define-key global-map (kbd "C-h C-k") 'describe-key)
(define-key global-map (kbd "C-c C-v") 'my-insert-buffer-name)
(define-key global-map (kbd "C-c k") 'delete-frame)
(define-key global-map (kbd "C-c n") 'make-frame)
(define-key global-map (kbd "M-n") 'my-scroll-up)
(define-key global-map (kbd "M-p") 'my-scroll-down)
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
(define-key global-map (kbd "C-.") 'my-jump-to-char)


;;; Appearance
(progn
  (and (fboundp 'fringe-mode) (fringe-mode -1))
  (and (fboundp 'tooltip-mode) (tooltip-mode +1))
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

(defun my-set-frame-background (mode)
  (setq frame-background-mode mode)
  (mapc 'frame-set-background-mode (frame-list)))
(or frame-background-mode
    (my-set-frame-background 'light))

(defun my-visual-ring-bell ()
  (let ((face-background (face-background 'default)))
    (set-face-background 'default "DodgerBlue")
    (set-face-background 'default face-background)))
(setq ring-bell-function 'my-visual-ring-bell)

(defun my-set-colors (light)
  "Set light colors if LIGHT is non nil, dark colors otherwise."
  (interactive "P")
  (cond
   (light
    (set-face-background 'default "white")
    (set-face-foreground 'default "black")
    (my-set-frame-background 'light))
   (t
    (set-face-background 'default "grey10")
    (set-face-foreground 'default "white")
    (my-set-frame-background 'dark))))

(defun my-eclipse-theme-setup ()
  (load-theme 'eclipse t)
  (let ((face 'success))
    (set-face-foreground face "ForestGreen")
    (set-face-bold face t))
  (dolist (face '(mode-line-buffer-id mode-line-highlight))
    (set-face-bold face t))
  (let ((face 'font-lock-type-face))
    (set-face-underline face nil)
    (set-face-italic face nil)
    (set-face-foreground face "gray50")))

(defun my-toggle-colors ()
  (interactive)
  "Toggle between light and dark background."
  (cl-case frame-background-mode
    ('light (my-set-colors nil))
    ('dark (my-set-colors t))))

;; (my-set-colors frame-background-mode)
(my-eclipse-theme-setup)

(and (eq 'light frame-background-mode)
     (member "--dark" command-line-args)
     (my-toggle-colors))

(defvar my-default-font
  (cond ((eq 'windows-nt system-type) "Consolas 14")
        (t  "DejaVu Sans Mono 12"))
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
      (when (< depth 20) (g 1)))

    (defun my-selective-display-decrease ()
      "Decrease the cap for `toogle-selective-display'.

See `my-selective-display-toggle' and `my-selective-display-increase'."
      (interactive)
      (when (> depth 1) (g -1)))))

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

(defun my-find-init-file ()
  (interactive)
  (find-file my-main-init-file))

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
  "Kill the buffer in the other window."
  (interactive)
  (let ((buf-name (buffer-name)))
    (save-window-excursion
      (select-window (next-window))
      (if (string= buf-name (buffer-name))
          (error "The next window dislays the same buffer!")
        (kill-buffer (current-buffer))))))

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
  (defvar my-untabify-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c <tab>") 'my-toggle-untabify-this-buffer)
      map))

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
    nil " untab" my-untabify-mode-map
    (cond (my-untabify-mode
           (advice-add 'yank :after #'my-yank-and-reindent-hook)
           (setq my-untabify-this-buffer (not (derived-mode-p 'makefile-mode)))
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
  "Not the current buffer but the buffer before."
  (other-buffer (current-buffer) 1))

(defun my-insert-buffer-name ()
  "Insert the previous buffer name.  Useful for compilation."
  (interactive)
  (insert (buffer-name (my-previous-buffer))))

(defun my-insert-buffer-file-name (arg)
  "Insert the previous buffer path.

With a prefix argument ARG, insert `file:' before."
  (interactive "P")
  (insert (concat (if arg "file:" "")
                  (buffer-file-name (my-previous-buffer)))))


;;; Packages
(use-package fullframe)

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
  (:map
   vc-prefix-map
   ("q" . my-vc-add-current-buffer))
  :config
  (progn
    (advice-add 'vc-diff :before #'my-save-all-buffers)
    (advice-add 'diff-apply-hunk :around #'my-no-confirm)
    (use-package vc-git
      :bind
      ("<f8>" . vc-git-grep))
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
          (shell-command (format "git add %s" filename))
          (message "Added %s!" filename))))
    (defun my-vc-dir-root ()
      (interactive)
      (when-let (root (vc-root-dir))
        (vc-dir root))))
  :bind
  (("<f7>" . vc-diff)
   ("C-<f7>" . vc-root-diff)))

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
    (my-key-chord-define global-map "jk" 'execute-extended-command)
    (my-key-chord-define global-map "fj" 'find-file)
    (my-key-chord-define global-map "hv" 'describe-variable)
    (my-key-chord-define global-map "hk" 'describe-key)
    (my-key-chord-define global-map "fy" 'my-find-init-file)
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
  :config (winner-mode +1))

(use-package hydra
  :init
  (progn
    (defhydra my-previous-next-buffer-repeat (global-map "C-x")
      "Goto previous/next buffer"
      ("<left>" previous-buffer)
      ("<right>" next-buffer)
      ("k" my-kill-buffer "kill"))))

(use-package dired
  :bind (:map dired-mode-map ("M-<down>" . dired-find-file))
  :config
  (progn
    (use-package dired-x
      :commands (dired-omit-mode dired-jump)
      :init
      (add-hook 'dired-mode-hook 'dired-omit-mode)
      :bind
      (("M-<up>" . dired-jump)
       ("C-x C-j" . dired-jump)
       :map dired-mode-map
       ("M-<up>" . dired-jump)
       ("C-x C-j" . dired-jump)))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-listing-switches "-alh")
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    (set-face-attribute 'dired-ignored nil
                        :foreground "gray60")))

(use-package ffap
  :bind
  ([remap find-file] . find-file-at-point)
  ("<S-mouse-2>" . ffap-at-mouse)
  :config
  (progn
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

    ;; Show grep matches at the end of the *grep* buffer
    (add-to-list 'grep-mode-font-lock-keywords
                 '("^Grep[/a-zA-z]* finished \\(?:(\\([0-9]+ match\\(es\\)? found\\))\\).*"
                   (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
                   (1 compilation-info-face nil t)))

    (add-to-list 'display-buffer-alist
                 '("*compilation*"
                   (display-buffer-reuse-window display-buffer-in-side-window)
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
      (:map
       emacs-lisp-mode-map
       ("C-c C-k" . eval-buffer)
       ("C-c C-z" . ielm))))
  :bind
  (:map ielm-map
        ("C-c C-z" . bury-buffer)))


;;; Mouse
(use-package mouse
  :bind
  (("<S-mouse-1>" . my-acme-search-forward)
   ("<C-wheel-up>" . text-scale-increase)
   ("<C-wheel-down>" . text-scale-decrease)
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
   ("<f3>" . isearch-repeat-forward)
   ("S-<f3>" . isearch-repeat-backward)
   ("<f4>" . my-transient-isearch-map-mode)
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

    (defvar my-transient-isearch-map-bindings
      '(("n" . isearch-repeat-forward)
        ("N" . isearch-repeat-backward)
        ("%" . isearch-query-replace)
        ("g" . my-isearch-beginning-of-buffer)
        ("G" . my-isearch-end-of-buffer)
        ("q" . my-transient-isearch-map-mode)))

    (define-minor-mode my-transient-isearch-map-mode
      "Override some keys in isearch." nil nil nil
      (cond
       (my-transient-isearch-map-mode
        (add-hook 'isearch-mode-end-hook 'my-transient-isearch-map-mode-off)
        (dolist (binding my-transient-isearch-map-bindings)
          (define-key isearch-mode-map (car binding) (cdr binding))))
       (t
        (remove-hook 'isearch-mode-end-hook 'my-transient-isearch-map-mode-off)
        (dolist (binding my-transient-isearch-map-bindings)
          (define-key isearch-mode-map (car binding) 'isearch-printing-char)))))
    (defun my-transient-isearch-map-mode-off ()
      (my-transient-isearch-map-mode -1))

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
    (advice-add 'isearch-backward :around #'my-isearch-region-hook)))

(use-package shell
  :bind
  (:map shell-mode-map
        ("<f5>" . comint-previous-input)
        ("<f6>" . compilation-shell-minor-mode))
  :config
  (progn
    (defun my-reset-prompt-command ()
      (insert "PROMPT_COMMAND=\"\"")
      (comint-send-input))
    (add-hook 'shell-mode-hook 'my-reset-prompt-command)
    (add-hook 'shell-mode-hook 'dirtrack-mode)
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
  :defer t
  :bind
  (:map
   vc-prefix-map
   ("c" . magit-commit)
   ("p" . magit-commit-amend)
   ("/" . magit-log-popup))
  :config
  (dolist (command '(magit-commit magit-commit-amend magit-status))
    (advice-add command :before #'my-save-all-buffers))
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

(use-package ivy
  :diminish ivy-mode
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ("C-<f8>" . counsel-rg)
   ("C-c s" . swiper)
   ("C-c r" . ivy-resume)
   ("C-M-y". counsel-yank-pop)
   ("C-z" . counsel-switch-to-shell-buffer)
   ("C-c M-x" . counsel-M-x)
   ("<f10>" . counsel-git-change-worktree)
   :map isearch-mode-map
   ("M-s p" . swiper-from-isearch)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<next>" . ivy-scroll-up-command)
   ("<prior>" . ivy-scroll-down-command)
   ("<right>" . ivy-alt-done))
  :init
  (progn
    (defun ivy-display-function-lv (text)
      (require 'lv)
      (let ((lv-force-update t))
        (lv-message
         (if (string-match "\\`\n" text)
             (substring text 1)
           text))))

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
  :bind (:map flycheck-mode-map ("C-S-<next>" . flycheck-next-error)))

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
                 '(tuareg-font-lock-governing-face (:inherit font-lock-type-face)))
    (advice-add 'tuareg-mode :after #'my-preserve-comment-style)
    (defun my-preserve-comment-style () (setq comment-style 'indent))
    (use-package ocp-indent
      :demand t
      :bind (:map tuareg-mode-map ("C-=" . ocp-indent-buffer)))))

(use-package cc-vars
  :defer t
  :config
  (progn
    (defun my-c-setup ()
      "My setup for C."
      (defvar c-default-style)
      (defvar indent-tabs-mode)
      (setq c-default-style "linux")
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
  :after vc
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
  (("C-M-." . mc/mark-next-like-this)
   ("C-M-," . mc/mark-previous-like-this)
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
          (hl-line-mode 1)))))
  :config
  (set-face-attribute 'hl-line nil :inherit 'next-error))

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

(use-package mic-paren
  :init
  (paren-activate))

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
  (([remap save-buffers-kill-terminal] . restart-emacs)))

(use-package whitespace
  :config
  (progn
    (set-face-foreground 'whitespace-line nil)
    (set-face-background 'whitespace-line nil)))

(provide 'my-init)
;;; my-init.el ends here
