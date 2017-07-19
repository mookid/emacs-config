;;; my-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'use-package)
(setq use-package-verbose t)


;;; Macros
(defun my-goto-buffer (buffername)
  "Select buffer named BUFFERNAME."
  (select-window (split-window-vertically))
  (switch-to-buffer-other-window (get-buffer-create buffername)))

(defmacro my-window-command (key buffername)
  "Defines a command to jump to the buffer designated by BUFFER-NAME and bind it."
  (let ((command-name (intern (concat "my-goto-" buffername))))
    `(progn
       (defun ,command-name ()
         ,(concat "Goto buffer `" buffername "'.")
         (interactive)
         (my-goto-buffer ,buffername))
       (define-key global-map (kbd ,(concat "C-c w " key)) ',command-name))))

(defmacro my-save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'my-save-column 'lisp-indent-function 0)

(cl-flet ((always-yes (&rest _) t))
  (defun my-no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(defun my-recentf-command () (interactive))

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
    (my-key-chord-define global-map "fh" 'my-recentf-command))
  :config
  (progn
    (defun my-key-chord-setup ()
      (with-eval-after-load 'init
        (or key-chord-mode (key-chord-mode +1))
        (dolist (mapping my-key-chords-alist)
          (apply #'key-chord-define mapping))))
    (my-key-chord-setup)))



;;; Basic configuration
(defun display-startup-echo-area-message () "Inhibit welcome message." ())
(setq initial-scratch-message nil)
(with-current-buffer (get-buffer "*scratch*")
  (add-hook 'kill-buffer-hook
            (lambda () (error "DENIED! don't kill my precious *scratch*!!"))
            nil t))
(setq frame-title-format (list "%f"))
(defun my-toggle-debug ()
  "Change the value of `debug-on-error'."
  (interactive)
  (message "`debug-on-error' set to %S" (cl-callf not debug-on-error)))

(defun my-view-echo-area-messages (new-frame)
  "View the log of recent echo-area messages: the `*Messages*' buffer.
The number of messages retained in that buffer
is specified by the variable `message-log-max'.

If NEW-FRAME is non nil, display it in a new frame."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (display-buffer (current-buffer) nil (if new-frame (make-frame) nil))))
(define-key global-map [remap view-echo-area-messages] 'my-view-echo-area-messages)

(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

(add-to-list 'completion-styles 'partial-completion)

(setq minibuffer-depth-indicate-mode t)

;; Easily name kbd macros
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

(defun my-move-line-up ()
  (interactive)
  (my-save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun my-move-line-down ()
  (interactive)
  (my-save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

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

(add-function :after (symbol-function 'my-yank)
              #'my-yank-and-reindent-hook)

(defun my-yank-and-reindent-hook (&rest _)
  (indent-region (mark) (point)))

;; Hippie expand
(use-package hippie-expand
  :bind ([remap dabbrev-expand] . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name)))

;; Colors
(defvar my-colors-param 'light)
(defun my-toggle-colors ()
  (interactive)
  "Toggle between light and dark background."
  (cl-case my-colors-param
    ('light
     (set-background-color "black")
     (set-foreground-color "white")
     (setq my-colors-param 'dark))
    ('dark
     (set-background-color "white")
     (set-foreground-color "black")
     (setq my-colors-param 'light))))

;; Keybindings
(define-key global-map (kbd "C-h C-k") 'describe-key)
(define-key global-map (kbd "C-c C-v") 'my-insert-buffer-name)
(define-key global-map (kbd "C-c k") 'delete-frame)
(define-key global-map (kbd "C-c n") 'make-frame)
(define-key global-map (kbd "M-n") 'my-scroll-up)
(define-key global-map (kbd "M-p") 'my-scroll-down)
(define-key global-map (kbd "C-M-<backspace>") 'my-clone-line)
(define-key global-map (kbd "C-c C-M-<up>") 'raise-sexp)
(define-key global-map (kbd "C-c .") 'repeat)
(define-key global-map (kbd "M-=") 'align-regexp)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "C-x g") 'move-to-column)
(define-key global-map (kbd "M-`") 'my-other-window-or-switch-buffer)
(define-key global-map (kbd "<M-left>") 'previous-buffer)
(define-key global-map (kbd "<M-right>") 'next-buffer)
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
(define-key global-map (kbd "C-S-<up>") 'my-move-line-up)
(define-key global-map (kbd "C-S-<down>") 'my-move-line-down)
(define-key global-map (kbd "C-c F") 'my-toggle-debug)

;; elisp
(setq initial-major-mode 'emacs-lisp-mode)
(setq eval-expression-print-level nil)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)

;; Set mode line format
(setq-default mode-line-format
              (mapcar (lambda (elt)
                        (if (and (stringp elt) (string-match "  +" elt)) " " elt))
                      mode-line-format))

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
(defvar my-untabify-this-buffer)
(defun my-untabify-buffer ()
  "Untabify the current buffer, unless `my-untabify-this-buffer' is nil."
  (and my-untabify-this-buffer (untabify (point-min) (point-max))))

(define-minor-mode my-untabify-mode
  "Untabify buffer on save." nil " untab" nil
  (cond (my-untabify-mode
         (make-variable-buffer-local 'my-untabify-this-buffer)
         (setq my-untabify-this-buffer (not (derived-mode-p 'makefile-mode)))
         (add-hook 'before-save-hook 'my-untabify-buffer nil t))
        (t
         (kill-local-variable 'my-untabify-this-buffer)
         (remove-hook 'before-save-hook 'my-untabify-buffer t))))
(add-hook 'prog-mode-hook 'my-untabify-mode)
;; Delete trailing whitespaces when saving a file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Short answers to questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save all buffers when focus is lost
(defun my-save-all-buffers (&rest _)
  "Save all buffers."
  (interactive)
  (save-some-buffers t))
(define-key global-map [remap save-buffer] 'my-save-all-buffers)
(define-key global-map [remap save-some-buffers] 'my-save-all-buffers)

;; VC
(use-package vc
  :bind
  (("<f7>" . vc-diff)
   ("C-<f7>" . vc-root-diff)
   ("C-M-<f7>" . my-vc-remove-whitespace-diff))
  :config
  (progn
    (add-function :before (symbol-function 'vc-diff) #'my-save-all-buffers)
    (add-function :around (symbol-function 'diff-apply-hunk) #'my-no-confirm)
    (defun my-vc-remove-whitespace-diff (filename)
      "Remove whitespace diff in FILENAME.

FILENAME is a name of a file or a directory."
      (interactive "fRemove whitespace diff on subtree: ")
      (and filename
           (let* ((vc-root-dir (vc-root-dir))
                  (relative-file-name (file-relative-name filename vc-root-dir))
                  (patch-file-name (concat filename "~~~")))
             (shell-command
              (mapconcat #'identity (list (format "git diff -w %s > %s" relative-file-name patch-file-name)
                                          (format "git checkout %s" relative-file-name)
                                          (format "git apply %s" patch-file-name)
                                          (format "rm -f %s" patch-file-name))
                         "&& ")))))))

(use-package vc-hooks
  :bind
  (:map
   vc-prefix-map
   ("q" . my-vc-add-current-buffer))
  :init
  (progn
    (defun my-vc-add-current-buffer ()
      (interactive)
      (when (and buffer-file-name (stringp buffer-file-name))
        (let ((filename (file-name-nondirectory buffer-file-name)))
          (shell-command (format "git add %s" filename))
          (message "Added %s!" filename))))))

(use-package diff-mode
  :bind
  (:map
   diff-mode-shared-map
   ("j" . diff-hunk-next)
   ("k" . diff-hunk-prev)
   ("s" . diff-split-hunk)
   ("d" . diff-apply-hunk)
   ("r" . diff-refine-hunk)))

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
(use-package ediff-wind
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

;; completion
(define-key global-map (kbd "C-M-/") 'completion-at-point)
(setq completion-cycle-threshold 5)

(use-package elec-pair
  :init
  (progn
    (defvar electric-pair-pairs)
    (defvar show-paren-delay)
    (add-hook 'prog-mode-hook 'electric-pair-local-mode))
  :config
  (progn
    (electric-pair-mode -1)
    (add-to-list 'electric-pair-pairs '(?\{ . ?\}))
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)))

(use-package paren
  :init
  (progn
    (setq show-paren-delay 0)
    (show-paren-mode 1)))

(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 80))

;; Setting up fonts
(progn
  (defvar my-default-font
    (cond ((eq 'windows-nt system-type) "Consolas 14")
          (t  "DejaVu Sans Mono 12"))
    "The font used almost everywhere.")
  (set-default-coding-systems 'utf-8)
  (with-eval-after-load 'init
    (add-to-list 'default-frame-alist `(font . ,my-default-font))))

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
(setq recenter-positions '(top middle bottom))

;; Pop mark
(setq set-mark-command-repeat-pop t)

(use-package fullframe)

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (fullframe ibuffer quit-window))

(use-package package
  :init
  (fullframe list-packages quit-window))

;; Shell
(progn
  (let* ((cygwin-root "c:")
         (cygwin-bin (expand-file-name "bin" cygwin-root)))
    (when (and (eq 'windows-nt system-type)
               (file-readable-p cygwin-root))
      (define-key global-map (kbd "C-c u") 'my-dos2unix-cmd)
      (defun my-dos2unix-cmd ()
        (interactive)
        (and buffer-file-name
             (shell-command (format "dos2unix %s" buffer-file-name))))
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
          (arg (if (eq last-repeatable-command this-command)
                   my-zap-to-char-last-arg
                 (setq my-zap-to-char-last-arg (read-char "zap to char: ")))))
      (zap-to-char 1 arg))))
(define-key global-map [remap zap-to-char] 'my-zap-to-char)

(define-key global-map (kbd "C-.") 'my-jump-to-char)
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
  (insert (file-name-sans-extension (buffer-name (my-previous-buffer)))))

(defun my-insert-buffer-file-name (arg)
  "Insert the previous buffer path.

With a prefix argument ARG, insert `file:' before."
  (interactive "P")
  (insert (concat (if arg "file:" "")
                  (buffer-file-name (my-previous-buffer)))))

;; Display page delimiter as a horizontal line
;; (aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))



(use-package dired
  :demand t
  :init
  (use-package dired-x
    :config
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    :bind
    (("M-<up>" . dired-jump)
     ("C-x C-j" . dired-jump)
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
(use-package ffap
  :demand t
  :bind
  ([remap find-file] . find-file-at-point)
  ("<S-mouse-2>" . ffap-at-mouse)
  :init
  (progn
    (defun my-ffap-prompter-noconfirm (fn &optional guess)
      "Remove confirmation."
      (xref-push-marker-stack)
      (or guess (ffap-guesser) (funcall fn))))
  :config
  (progn
    (add-function :around (symbol-function 'ffap-prompter)
                  'my-ffap-prompter-noconfirm)))

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
     (browse-url (format "https://%s%s" ,engine-url w))))
(put 'my-defweb-search 'lisp-indent-function 'defun)

(my-defweb-search my-google-search
  "www.google.com/search?q=")

(define-key global-map (kbd "C-h g") 'my-google-search)


;;; Compilation
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

    (add-hook 'compilation-finish-functions #'my-compile-finish-hook)

    (defun my-count-grep-matches (buf _msg)
      (with-current-buffer buf
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

    (add-hook 'grep-mode-hook 'my-count-grep-matches-hook)))

(use-package iedit
  :init
  (progn
    (defun my-iedit-occur (&optional nlines)
      (interactive)
      (when iedit-mode
        (occur iedit-initial-string-local nlines))))
  :bind
  (:map iedit-mode-keymap
        ("C-s" . iedit-next-occurrence)
        ("C-r" . iedit-prev-occurrence)
        ("M-o" . my-iedit-occur)))


;;; Mouse
(use-package mouse
  :bind
  (("<S-mouse-1>" . my-acme-search-forward)
   ("<C-wheel-up>" . text-scale-increase)
   ("<C-wheel-down>" . text-scale-decrease)
   ("C-c ." . my-delete-mouse-secondary-overlay))
  :config
  (set-face-background 'secondary-selection "light cyan")
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


;;; Recentf
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

;; Scroll to the results in occur buffers
(progn
  (defun my-occur-skip-gribberish-hook (&rest _)
    (when isearch-mode (isearch-exit))
    (select-window (get-buffer-window "*Occur*"))
    (goto-char (point-min))
    (next-logical-line 1)
    (recenter 0)
    (select-window (next-window)))
  (add-function :after (symbol-function 'occur)
                #'my-occur-skip-gribberish-hook))


;;; Isearch
(use-package isearch
  :diminish isearch-mode
  :bind
  (("M-o" . my-occur-region)
   :map
   minibuffer-local-isearch-map
   ("TAB" . isearch-complete-edit)
   :map
   isearch-mode-map
   ("M-o" . isearch-occur)
   ("TAB" . isearch-complete)
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
    (add-function :around (symbol-function 'isearch-forward)
                  'my-isearch-region-hook)
    (add-function :around (symbol-function 'isearch-backward)
                  'my-isearch-region-hook)))


;;; Windows
(setq tags-add-tables nil)

(use-package winner
  :config (winner-mode +1))

(define-key global-map (kbd "<f2> <f2>") 'my-toggle-window-split)

(my-window-command "g" "*grep*")
(my-window-command "d" "*vc-diff*")
(my-window-command "c" "*compilation*")
(my-window-command "o" "*Occur*")
(my-window-command "s" "*scratch*")

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
  :init
  (progn (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
         (add-hook 'lisp-mode-hook 'lispy-mode))
  :config
  (progn
    (lispy-set-key-theme '(special))))

(use-package magit
  :disabled t
  :bind (("C-c <f7>" . magit-rebase-interactive)
         ("C-c g" . magit-status)
         ("<C-f7>" . magit-diff-working-tree))
  :config
  (fullframe magit-status magit-mode-quit-window))

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
  :demand t
  :diminish ivy-mode
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap completion-at-point] . my-ivy-completion-at-point)
   ([remap execute-extended-command] . my-M-x-command)
   ("C-<f8>" . counsel-rg)
   ("C-c s" . swiper)
   ("C-c r" . ivy-resume)
   ("C-M-y". counsel-yank-pop)
   :map isearch-mode-map
   ("M-s p" . swiper-from-isearch)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<next>" . ivy-scroll-up-command)
   ("<prior>" . ivy-scroll-down-command)
   ("<right>" . ivy-alt-done))
  :init
  (progn
    (defun my-M-x-command (use-ivy)
      (interactive "P")
      (call-interactively (if use-ivy 'counsel-M-x 'execute-extended-command)))
    (defvar ivy-use-virtual-buffers)
    (fset 'my-recentf-command 'ivy-switch-buffer)
    (defun my-ivy-completion-at-point ()
      (interactive)
      (if (and (boundp 'ivy-mode) (symbol-value 'ivy-mode))
          (completion-at-point)
        (ivy-mode +1)
        (unwind-protect
            (completion-at-point)
          (ivy-mode -1))))
    (setq ivy-use-virtual-buffers t))
  :config
  (progn
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
  :init
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
  :bind (("C-M-SPC" . er/expand-region))
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
   ("C-c ." . nil)
   ("C-c /" . nil))
  :config
  (progn
    (set-face-bold 'tuareg-font-lock-governing-face nil)
    (use-package ocp-indent
      :demand t
      :bind (:map tuareg-mode-map ("C-=" . ocp-indent-buffer)))))

;; C configuration
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
   ("S-<f7>" . diff-hl-revert-hunk))
  :config
  (progn
    (add-hook 'diff-hl-mode-hook #'diff-hl-margin-mode)
    (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
    (cl-flet ((my-diff-hl-on (&rest _) (or diff-hl-mode (diff-hl-mode +1))))
      (dolist (fun '(diff-hl-previous-hunk diff-hl-next-hunk vc-diff))
        (add-function :before (symbol-function fun) #'my-diff-hl-on)))
    (add-function :before (symbol-function 'diff-hl-diff-goto-hunk)
                  #'my-save-all-buffers)
    (add-function :around (symbol-function 'diff-hl-revert-hunk)
                  #'my-no-confirm)))

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

(use-package eshell
  :init
  (progn
    (setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)
    (defun my-eshell-here ()
      "Opens up a new shell in the directory associated with the
current buffer's file, or switch to it when it already exists.

The eshell buffer is renamed to match that directory to make
multiple eshell windows easier."
      (interactive)
      (run-hooks 'focus-out-hook)
      (let* ((file-name (buffer-file-name))
             (parent (if file-name
                         (file-name-directory file-name)
                       default-directory))
             (eshell-buffer-name (concat "*eshell: " parent "*"))
             (buf (get-buffer eshell-buffer-name)))
        (if buf
            (pop-to-buffer buf)
          (eshell "*eshell: ``new''*")
          (rename-buffer eshell-buffer-name)))
      (insert "ls")
      (eshell-send-input))

    (defun my-cursor-aware-insert (beg end)
      (insert beg)
      (insert end)
      (backward-char (length end))))

  :config
  (progn
    (add-hook 'eshell-mode-hook 'my-eshell-face-setup)
    (defun my-eshell-face-setup ()
      (face-remap-add-relative 'default :foreground "white" :background "#363033")
      (set-face-foreground 'eshell-ls-directory "SkyBlue"))

    (defun eshell/l ()
      (insert "git log --all --decorate --oneline --graph --color -5"))

    (defun eshell/cm ()
      (my-cursor-aware-insert "git commit -am\"" "\" && git show --name-only"))

    (defun eshell/co ()
      (insert "git checkout "))

    (defun eshell/df ()
      (my-cursor-aware-insert "git diff" " --color"))

    (defun eshell/d ()
      (dired "."))

    (defun eshell/dc ()
      (my-cursor-aware-insert "git diff" " --cached --color"))

    (defun eshell/s ()
      (insert "git status"))

    (defun eshell/show ()
      (insert "git show --color HEAD~0"))

    (defun eshell/amen ()
      (my-cursor-aware-insert "git commit " "--amend --no-edit && git show --name-only"))

    (defun eshell/reset ()
      (my-cursor-aware-insert "git reset HEAD~0" " --hard"))

    (defun eshell/wip ()
      (insert "git commit -am wip && git branch wip && git reset --hard HEAD~1"))

    (defun eshell/q ()
      (bury-buffer))

    (defun my-eshell-kill-line-backward ()
      (interactive)
      (my-kill-line-backward 'eshell-bol))

    (defun my-eshell-keymap-setup ()
      (when eshell-mode-map
        (define-key eshell-mode-map [remap my-kill-line-backward]
          'my-eshell-kill-line-backward)))
    (add-hook 'eshell-mode-hook #'my-eshell-keymap-setup))
  :bind
  (("<f1>" . my-eshell-here)))

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
                 (buffer-file-name))
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
  (set-face-attribute 'hl-line nil
                      :background "cornsilk"
                      :inherit nil))

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
      (and electric-pair-mode (electric-pair-local-mode -1)))
    (add-hook 'prog-mode-hook 'smartparens-mode t)))

(use-package icomplete-mode
  :disabled t
  :init (icomplete-mode +1)
  :bind
  (:map icomplete-minibuffer-map
        ("C-s" . icomplete-forward-completions)
        ("C-r" . icomplete-backward-completions)))

(provide 'my-init)
;;; my-init.el ends here
