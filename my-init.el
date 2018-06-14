;;; my-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-hook-name-suffix nil)


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
(setq frame-title-format (list mode-line-modified " %b -- %f  " '(:eval vc-mode)))
(setq minibuffer-depth-indicate-mode t)
(setq tags-add-tables nil)
(setq load-prefer-newer t)
(setq switch-to-visible-buffer nil)
(setq blink-matching-paren 't)
(setq dabbrev-case-fold-search nil)
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

(add-to-list 'completion-styles 'partial-completion)
(add-to-list 'completion-styles 'initials)

(defvar my-undo-command 'undo
  "A symbol to be funcalled to undo.")

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

(defmacro my-def-balance-after (newfun orig-fun)
  `(progn
     (define-key global-map [remap ,orig-fun] ',newfun)
     (defun ,newfun (&rest args)
       ,(interactive-form orig-fun)
       (apply #',orig-fun args)
       (balance-windows))))

(my-def-balance-after my-split-window-right split-window-right)
(my-def-balance-after my-split-window-below split-window-below)


;;; Keybindings
(define-key global-map (kbd "C-c u") 'my-dos2unix)
(when (boundp 'mouse-wheel-down-event)
  (let ((down-key (kbd (format "<C-%s>" mouse-wheel-down-event))))
    (define-key global-map down-key 'text-scale-increase)))
(when (boundp 'mouse-wheel-up-event)
  (let ((up-key (kbd (format "<C-%s>" mouse-wheel-up-event))))
    (define-key global-map up-key 'text-scale-decrease)))
(define-key global-map (kbd "<mode-line> <mouse-2>") 'my-kill-buffer)
(define-key global-map (kbd "<escape> <escape>") 'my-delete-side-windows)
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
(define-key global-map (kbd "<f12>") 'my-describe-face-at-point)
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
(define-key global-map (kbd "C-S-u") 'upcase-region)
(define-key global-map (kbd "C-S-l") 'downcase-region)
(define-key global-map (kbd "C-S-k") 'my-kill-line-backward)
(define-key global-map (kbd "C-c <f5>") 'revert-buffer)
(define-key global-map (kbd "<f2>") 'rename-buffer)
(define-key global-map (kbd "M-j") 'my-delete-indentation-forward)
(define-key global-map (kbd "<insert>") nil)
(define-key global-map (kbd "C-c T") 'my-insert-todo)
(define-key global-map (kbd "C-x C-S-<left>") 'my-all-previous-buffer)
(define-key global-map (kbd "C-x C-S-<right>") 'my-all-next-buffer)


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
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq ring-bell-function 'my-visual-ring-bell)

(defvar my-frame-params nil
  "Configuration for `default-frame-alist'.")
(setq my-frame-params
      (when window-system
        (pcase system-type
          (`darwin
           '(height 30 font "Menlo 18"))
          (`windows-nt
           '(height 45 font "Consolas 14"))
          (_
           '(height 45 font "DejaVu Sans Mono 14")))))

(defvar my-color-theme
  nil
  "The selected color theme.")
(setq my-color-theme 'my-color)

(with-eval-after-load 'init
  (let ((height (plist-get my-frame-params 'height))
        (font (plist-get my-frame-params 'font)))
    (when height
      (add-to-list 'default-frame-alist `(height . ,height))
      (set-frame-height nil height))
    (when font
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-face-font 'default font)))
  (add-to-list 'default-frame-alist '(width . 160))
  (when (= 1 (count-windows))
    (split-window-right))
  (add-to-list 'initial-frame-alist '(top . 20))
  (add-to-list 'initial-frame-alist '(left . 120))
  (condition-case nil
      (load-theme my-color-theme t)
    (warn (format "Error during loading of theme %s" my-color-theme))))


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

(defun my-insert-todo ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (comment-dwim nil)
  (insert "TODO(NM) "))

(defun my-on-all-visible-windows (fun &rest args)
  (walk-windows
   (lambda (window)
     (with-selected-window window
       (apply fun args)))
   nil
   'visible))

(defun my-all-previous-buffer ()
  (interactive)
  (my-on-all-visible-windows 'previous-buffer))

(defun my-all-next-buffer ()
  (interactive)
  (my-on-all-visible-windows 'next-buffer))

(defvar my-pairs-alist
  '((?\" . ?\")
    (?\( . ?\))
    (?\{ . ?\})
    (?\[ . ?\]))
  "Supported delimiters for `my-delete-pair' and `my-cycle-pair'.")

(let (my-delete-pair-last)
  (defun my-delete-pair ()
    "Remove the pair of parenthesis around the point, or undo it."
    (interactive)
    (cond (my-delete-pair-last
           (funcall my-undo-command))
          (t
           (save-excursion
             (cond ((rassoc (char-after (1- (point))) my-pairs-alist)
                    (backward-sexp)
                    (delete-pair))
                   ((assoc (char-after (point)) my-pairs-alist)
                    (delete-pair))
                   (t
                    (error "Not on a parenthesis-like character"))))))
    (cl-callf not my-delete-pair-last)))

(defun my-cycle-pair ()
  "Toggle the parenthesis pair around the point and set mark at the other end.

The pairs depend on the value of `my-pairs-alist'.  The command
fails if the point is not either on an opening parenthesis
character or right after a closing one."
  (interactive)
  (let ((old-point (point))
        cell
        new-mark)
    (unwind-protect
        (progn
          (cond ((setq cell (cl-member (char-after (1- (point))) my-pairs-alist :key 'cdr))
                 (backward-sexp)
                 (setq new-mark (point)))
                ((setq cell (cl-member (char-after (point)) my-pairs-alist :key 'car)))
                (t
                 (error "Not on a parenthesis-like character")))
          (let ((pair (car (or (cdr-safe cell) my-pairs-alist))))
            (save-excursion
              (forward-sexp)
              (unless new-mark
                (setq new-mark (point)))
              (delete-char -1)
              (insert (cdr pair)))
            (delete-char 1)
            (insert (car pair))))
      ;; save excursion does not work because of the buffer mofification
      (goto-char old-point)
      (set-mark new-mark))))

(defun my-dos2unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix t)
  (save-buffer))

(defun my-face-at-point ()
  (let* ((old-hl-line-mode (and (boundp 'hl-line-mode) hl-line-mode))
         (old-global-hl-line-mode (and (boundp 'global-hl-line-mode) global-hl-line-mode)))
    (when (and old-hl-line-mode (fboundp 'hl-line-mode))
      (hl-line-mode -1))
    (when (and old-global-hl-line-mode (fboundp 'global-hl-line-mode))
      (global-hl-line-mode -1))
    (prog1
        (let* (hl-line-mode global-hl-line-mode
                            (face (face-at-point)))
          (when face
            (format "%S: %s" face (face-documentation face))))
      (when old-hl-line-mode
        (hl-line-mode 1))
      (when old-global-hl-line-mode
        (global-hl-line-mode 1)))))

(defun my-describe-face-at-point ()
  (interactive)
  (describe-face (face-at-point)))

(define-minor-mode my-face-at-point-mode
  "Instruments `eldoc-mode' to describe the face at point."
  :lighter " face"
  (cond (my-face-at-point-mode
         (add-function :before-until (local 'eldoc-documentation-function)
                       #'my-face-at-point))
        (t
         (remove-function (local 'eldoc-documentation-function)
                          #'my-face-at-point))))

(defun my-delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(defun my-set-indentation (n)
  (interactive "P")
  (setq c-basic-offset
        (cl-etypecase n
          (null (read-number "set indentation to: " 2))
          (string (number-to-string n))
          (number n)))
  (message "c-basic-offset = %d" c-basic-offset))

(defvar my-narrowed-buffers nil)
(defun my-kill-buffer-on-widen ()
  (when (member (current-buffer) my-narrowed-buffers)
    (kill-buffer))
  (setq my-narrowed-buffers
        (cl-remove-if-not #'buffer-live-p my-narrowed-buffers)))
(advice-add 'widen :after #'my-kill-buffer-on-widen)

(defun my-narrow-to-region-hook (fun &rest args)
  (interactive "r")
  (when (region-active-p)
    (deactivate-mark t))
  (let ((cloned-buffer (clone-indirect-buffer nil nil)))
    (with-current-buffer cloned-buffer
      (apply fun args))
    (push cloned-buffer my-narrowed-buffers)
    (switch-to-buffer cloned-buffer)))
(advice-add 'narrow-to-region :around #'my-narrow-to-region-hook)

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

(defun my-find-project ()
  (interactive)
  (find-file (expand-file-name "projects" (getenv "HOME"))))

(defun my-find-shell-config-file ()
  (interactive)
  (find-file (expand-file-name ".bashrc" (getenv "HOME"))))

(defun my-toggle-debug ()
  "Change the value of `debug-on-error'."
  (interactive)
  (message "`debug-on-error' set to %S" (cl-callf not debug-on-error)))

(defun my-view-echo-area-messages (arg)
  "With prefix argument, open the buffer in a new frame."
  (interactive "P")
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
  "Forward to `scroll-up'.

Scroll up ARG lines."
  (interactive "P")
  (let ((arg (or arg 1)))
    (scroll-up arg)))

(defun my-scroll-down (arg)
  "Forward to `scroll-down'.

Scroll down ARG lines."
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

(defun my-scroll-up-command (&optional _arg)
  (interactive))

(defun my-scroll-down-command (&optional _arg)
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

(defun my-kill-region-or-whole-line (&rest args)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (apply 'kill-whole-line args)))
(define-key global-map [remap kill-region] 'my-kill-region-or-whole-line)

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
  (make-variable-buffer-local 'my-untabify-this-buffer)

  (defun my-save-buffers-without-untabification ()
    (interactive)
    (let ((my-untabify-this-buffer nil))
      (save-some-buffers t)))

  (defun my-untabify-buffer ()
    "Untabify the current buffer and delete trailing whitespaces,
unless `my-untabify-this-buffer' is nil."
    (when my-untabify-this-buffer
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace)))

  (define-minor-mode my-untabify-mode
    "Untabify buffer on save. When not applicable, turn on `whitespace-mode'.

\\{my-untabify-mode-map}"
    :lighter " untab"
    (cond (my-untabify-mode
           (setq my-untabify-this-buffer
                 (not (or (derived-mode-p 'makefile-mode)
                          (search-forward "\t" nil t))))
           (add-hook 'focus-out-hook #'my-save-buffers-without-untabification)
           (add-hook 'before-save-hook #'my-untabify-buffer nil t))
          (t
           (remove-hook 'focus-out-hook #'my-save-buffers-without-untabification)
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
(use-package savehist
  :init
  (savehist-mode t)
  :config
  (setq history-length 16384)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (mapc (lambda (item) (add-to-list 'savehist-additional-variables item))
        '(kill-ring
          search-ring
          regexp-search-ring
          compile-command)))

(use-package autorevert
  :commands 'global-auto-revert-mode
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode +1))

(use-package diff
  :hook (diff-mode-hook . read-only-mode))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package vc
  :defer t
  :commands vc-root-dir
  :preface
  (defun my-vc-add-current-buffer ()
    (interactive)
    (when (and buffer-file-name (stringp buffer-file-name))
      (let ((filename (file-name-nondirectory buffer-file-name)))
        (vc-git--call nil "add" "--" filename)
        (message "Added %s!" filename))))
  (defun my-vc-dir-root ()
    (interactive)
    (when-let (root (vc-root-dir))
      (vc-dir root)))
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
      :bind
      (([remap vc-dir] . my-vc-dir-root)
       ("C-M-<f7>" . vc-dir)
       :map vc-dir-mode-map
       ("d" . vc-diff)))))

(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package elec-pair
  :commands my-electric-pair-mode
  :defer t
  :commands electric-pair-mode
  :preface
  (defun my-electric-pair-mode (flag)
    (cond ((and (fboundp 'electric-pair-local-mode) (> flag 0))
           (add-hook 'prog-mode-hook 'electric-pair-local-mode))
          ((fboundp 'electric-pair-local-mode)
           (remove-hook 'prog-mode-hook 'electric-pair-local-mode))
          (t
           (electric-pair-mode flag))))
  :config
  (progn
    (my-electric-pair-mode 1)
    (add-to-list 'electric-pair-pairs '(?\{ . ?\}))
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)))

(use-package paren
  :defer t
  :commands show-paren-mode
  :init
  (progn
    (setq show-paren-delay 0)
    (show-paren-mode 1)))

(use-package key-chord
  :preface
  (defvar my-key-chords-alist nil
    "A list of key chords bindings.

Each binding is a list of 3 elements: KEYS, KEYMAP, COMMAND.
KEYS is string of length 2; KEYMAP defaults to the global map.")
  (defun my-key-chord-define (keymap keys command)
    (push (list keymap keys command) my-key-chords-alist)
    (and (fboundp 'my-key-chord-setup) (my-key-chord-setup)))
  (defun my-key-chord-setup ()
    (with-eval-after-load 'init
      (or key-chord-mode (key-chord-mode 1))
      (dolist (mapping my-key-chords-alist)
        (apply #'key-chord-define mapping))))
  :init
  (my-key-chord-define global-map "hv" 'describe-variable)
  (my-key-chord-define global-map "hk" 'describe-key)
  (my-key-chord-define global-map "hj" 'describe-function)
  (my-key-chord-define global-map "fy" 'my-find-init-file)
  (my-key-chord-define global-map "fb" 'my-find-shell-config-file)
  (my-key-chord-define global-map "fq" 'my-find-project)
  (my-key-chord-define global-map "fh" 'my-recentf-command)
  :config
  (my-key-chord-setup))

(use-package hippie-exp
  :defer t
  :bind
  (("C-M-/" . my-expand-lines))
  :init
  (fset 'my-expand-lines
        (make-hippie-expand-function
         '(try-expand-line
           try-expand-line-all-buffers))))

(use-package winner
  :commands winner-mode
  :defer t
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
      ("o" other-window "other window")
      ("`" my-other-window-or-switch-buffer "other window")
      ("0" delete-window "delete window")
      ("1" delete-other-windows "delete other windows")
      ("2" my-split-window-below "split below")
      ("3" my-split-window-right "split right")
      ("=" balance-windows "balance")
      ("q" nil "quit"))
    (defhydra my-cycle-pair (global-map "C-c")
      "parenthesis"
      ("(" my-delete-pair "delete")
      (")" my-cycle-pair "cycle"))))

(use-package dired
  :defer t
  :commands (dired dired-get-file-for-visit)
  :preface
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
  :bind (:map dired-mode-map ("M-<down>" . dired-find-file))
  :init
  (progn
    (use-package dired-aux
      :commands (dired-isearch-filenames-mode)
      :hook (dired-mode-hook . dired-isearch-filenames-mode))
    (setq dired-dwim-target t)
    (use-package dired-x
      :commands (dired-omit-mode dired-jump)
      :hook (dired-mode-hook . dired-omit-mode)
      :bind
      (("M-<up>" . dired-jump)
       ("C-x C-j" . dired-jump)
       :map dired-mode-map
       ("M-<up>" . dired-jump)
       ([mouse-2] . my-dired-mouse-find-file)
       ("C-x C-j" . dired-jump)))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-listing-switches "-alh")
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always))
  :config
  (set-face-foreground 'dired-ignored "gray60"))

(use-package ffap
  :defer t
  :commands (ffap-guesser find-file-at-point)
  :preface
  (defun my-ffap-prompter-noconfirm (fn &optional guess)
    "Remove confirmation."
    (and (fboundp 'xref-push-marker-stack) (xref-push-marker-stack))
    (or guess (ffap-guesser) (funcall fn)))
  :bind
  ("<S-mouse-2>" . ffap-at-mouse)
  :config
  (my-key-chord-define global-map "fj" 'find-file-at-point)
  (advice-add 'ffap-prompter :around #'my-ffap-prompter-noconfirm))

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
(my-defweb-search my-emacs-archive-search
  "www.google.com/search?q=site%3Ahttps%3A%2F%2Flists.gnu.org%2Farchive%2Fhtml%2Femacs-devel%2F+")
(my-defweb-search my-hoogle-search
  "www.haskell.org/hoogle/?hoogle=")
(my-defweb-search my-msdn-search
  "social.msdn.microsoft.com/Search/en-US?query=")

(use-package grep
  :defer t
  :preface
  (defun my-disable-jump-to-error ()
    "Disable `compilation-auto-jump-to-next' local variable."
    (kill-local-variable 'compilation-auto-jump-to-next))
  (defun my-compile-finish-hook (buf status)
    (with-current-buffer buf
      (goto-char (point-min))
      (and (string= (buffer-name buf) "*compilation*")
           (string-match "^finished\\b" status)
           (not (search-forward "warning" nil t))
           (run-with-timer 0.4 nil #'switch-to-prev-buffer (get-buffer-window buf)))))
  :hook (grep-mode-hook . my-disable-jump-to-error)
  :config
  (use-package compile
    :defer t
    :hook (compilation-finish-functions . my-compile-finish-hook)
    :init
    (setq compilation-ask-about-save nil)
    (setq compilation-always-kill t)
    (setq compilation-scroll-output 'first-error)
    :bind
    (("<f5>" . recompile))))

(use-package iedit
  :defer t
  :preface
  (defun my-iedit-occur (&optional nlines)
    (interactive "p")
    (when iedit-mode
      (occur iedit-initial-string-local nlines)))
  :bind
  (("C-;" . iedit-mode)
   :map iedit-mode-keymap
   ("M-o" . my-iedit-occur)))


;;; elisp
(use-package ielm
  :defer t
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
  :defer t
  :commands mouse-set-point
  :preface
  (defun my-delete-mouse-secondary-overlay (&rest _)
    (run-with-timer 3 nil #'delete-overlay mouse-secondary-overlay))
  :config
  (advice-add 'mouse-drag-secondary :after #'my-delete-mouse-secondary-overlay)
  (setq mouse-drag-copy-region t)
  (setq mouse-yank-at-point t))

(use-package mouse-copy
  :defer t
  :bind
  (("<C-down-mouse-1>" . mouse-drag-secondary-pasting)
   ("<C-S-down-mouse-1>" . mouse-drag-secondary-moving))
  :config
  (setq mouse-drag-copy-region t))

(use-package recentf
  :commands (recentf-open-files recentf-mode)
  :preface
  (defun my-recentf-open-files ()
    "Forward to `recentf-open-files' without arguments."
    (interactive)
    (recentf-open-files))
  :init
  (recentf-mode +1)
  :config
  (progn
    (fset 'my-recentf-command 'my-recentf-open-files)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 150)))

(defun my-yank-diff (invert)
  "Yank a patch.

Yank, and then process the yanked region: remove the `+' that
start every line and kill the lines starting with `-'.

With prefix argument INVERT, invert `+' and `-'."
  (interactive "P")
  (yank)
  (save-mark-and-excursion
   (exchange-point-and-mark)
   (let ((drop (if invert "\+" "-")))
     (while (< (point) (mark))
       (cond ((eolp)
              (forward-line 1))
             ((looking-at drop)
              (let ((old-point (point)))
                (forward-line 1)
                (delete-region old-point (point))))
             (t
              (delete-char 1)
              (forward-line 1)))))))


;;; Isearch
(use-package isearch
  :commands (isearch-mode isearch-done)
  :preface
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
  :diminish isearch-mode
  :bind
  (("M-o" . my-occur-region)
   ("C-S-s" . isearch-forward-symbol-at-point)
   :map
   minibuffer-local-isearch-map
   ("TAB" . isearch-complete-edit)
   :map
   isearch-mode-map
   ("<tab>" . isearch-repeat-forward)
   ("S-<tab>" . isearch-repeat-backward)
   ("<escape>" . my-isearch-suspend)
   ("<f3>" . isearch-repeat-forward)
   ("S-<f3>" . isearch-repeat-backward)
   ("M-o" . isearch-occur)
   ("TAB" . isearch-complete)
   ("M-e" . isearch-toggle-symbol)
   ("M-<" . my-isearch-beginning-of-buffer)
   ("M->" . my-isearch-end-of-buffer))
  :hook (isearch-mode-end-hook . my-isearch-exit-beginning)
  :init
  (advice-add 'isearch-forward :around #'my-isearch-region-hook)
  (advice-add 'isearch-backward :around #'my-isearch-region-hook)
  (advice-add 'isearch-forward-regexp :around #'my-isearch-region-hook)
  (advice-add 'isearch-backward-regexp :around #'my-isearch-region-hook))

(use-package shell
  :preface
  (defvar my-shell-prompt-pwd-regexp "\\(.*\\)$ "
    "How to get the $PWD from the shell prompt.

A regexp that captures one match.")
  (defun my-rename-shell-buffer (name)
    "Make a new buffer name for the current shell buffer based on NAME."
    (interactive "sBuffer name: ")
    (rename-buffer
     (if (string= "" name) "*shell*" (format "*shell %s*" name))))
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
  (defun my-end-of-buffer-hook (&rest _)
    (goto-char (point-max)))
  :init
  (setenv "PAGER" "cat")
  (use-package shell
    :if (eq system-type 'windows-nt)
    :init
    (setq shell-file-name "bash")
    :hook (comint-output-filter-functions . comint-strip-ctrl-m))
  (use-package shell
    :if (not window-system)
    :bind
    ("C-z" . suspend-emacs))
  :bind
  (:map shell-mode-map
        ([remap dired] . my-shell-dired)
        ("C-z" . bury-buffer)
        ("C-l" . comint-clear-buffer)
        ("<f2>" . my-rename-shell-buffer)
        ("<f5>" . comint-previous-input)
        ("<f6>" . compilation-shell-minor-mode))
  :config
  (progn
    (setq-default comint-input-ignoredups 1)
    (advice-add 'comint-previous-input :before #'my-end-of-buffer-hook)
    (advice-add 'comint-next-input :before #'my-end-of-buffer-hook)))

(use-package evil-nerd-commenter
  :defer t
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c c". evilnc-copy-and-comment-lines)))

(use-package anzu
  :defer t
  :init (global-anzu-mode 1)
  :diminish anzu-mode)

(use-package lispy
  :commands lispy-set-key-theme
  :defer t
  :init
  :hook
  ((emacs-lisp-mode-hook . lispy-mode)
   (lisp-mode-hook . lispy-mode))
  :config
  (lispy-set-key-theme '(special)))

(use-package magit
  :commands magit-toplevel
  :preface
  (defun my-is-dedicated-to-magit (frame)
    (let ((cell (assq 'dedicated-to (frame-parameters frame))))
      (and (consp cell) (eq (cdr cell) 'magit))))
  (defun my-create-tags ()
    (interactive)
    (let ((default-directory (or (magit-toplevel)
                                 (read-directory-name "Directory: "))))
      (shell-command
       (format "find %s -type f -name %S | etags -"
               default-directory
               (read-string "file extension: " "*.[ch]")))))
  :defer t
  :bind
  (("C-x g" . magit-status)
   ("C-x v p" . magit-blame)
   ("M-<f7>" . magit-status))
  :config
  (setq magit-bury-buffer-function 'bury-buffer)
  (advice-add 'magit-discard-hunk :around 'my-no-confirm)
  (dolist (command '(magit-commit magit-commit-amend magit-status))
    (advice-add command :before #'my-save-all-buffers))
  (setq magit-commit-ask-to-stage t)
  (setq magit-backup-mode nil))

(use-package rainbow-delimiters
  :defer t
  :preface
  (defun my-rainbow-delimiters-disable ()
    "Disable rainbow-delimiters mode."
    (rainbow-delimiters-mode -1))
  :hook ((prog-mode-hook . rainbow-delimiters-mode)
         (shell-mode-hook . my-rainbow-delimiters-disable))
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :inherit 'error
                      :box t))

(eval
 `(use-package company
    :commands (company-abort company-complete-number)
    :preface
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
    :defer t
    :bind
    (:map
     company-mode-map
     ("C-\\" . company-complete))
    (:map
     company-active-map
     (" " . 'my-company-abort-with-space)
     ("<return>" . nil)
     ("C-n" . 'company-select-next)
     ("C-p" . 'company-select-previous)
     ,@(cl-loop for i below 10
                collect `(,(format "%d" i) . my-company-number)))
    :config
    (setq company-show-numbers t)
    (setq company-idle-delay nil)
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-flip-when-above t)))

(use-package ivy
  :preface
  (defun my-ivy-kill-region-or-whole-line (&rest args)
    (interactive "p")
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (ivy-kill-whole-line)))
  (defun ivy-display-function-lv (text)
    (require 'lv)
    (let ((lv-force-update t))
      (lv-message
       (if (string-match "\\`\n" text)
           (substring text 1)
         text))))
  (defun my-select-shell-buffer (orig-fun &rest args)
    (let ((buf (get-buffer "*shell*")))
      (if buf
          (switch-to-buffer buf)
        (apply orig-fun args))))

  (defun my-counsel-rg (p)
    (interactive "P")
    (let* ((directory (if p (read-directory-name "counsel from directory: ")))
           (current-prefix-arg nil))
      (counsel-rg (my-prompt) directory)))

  (defun ivy-display-function-popup (text)
    (require 'popup)
    (with-ivy-window
      (popup-tip
       (substring text 1)
       :nostrip nil)))
  :demand t
  :diminish ivy-mode
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("C-c b" . counsel-bookmark)
   ("<f8>" . my-counsel-rg)
   ("M-i" . counsel-imenu)
   ("C-S-r" . ivy-resume)
   ("C-M-y". counsel-yank-pop)
   ("C-c M-x" . counsel-M-x)
   ("<f10>" . counsel-git-change-worktree)
   :map ivy-minibuffer-map
   ([remap kill-region] . my-ivy-kill-region-or-whole-line)
   ("C-o" . ivy-occur)
   ("<next>" . ivy-scroll-up-command)
   ("<prior>" . ivy-scroll-down-command)
   ("<right>" . ivy-alt-done))
  :init
  (use-package ivy
    :if window-system
    :bind
    ("C-z" . counsel-switch-to-shell-buffer))
  (setq counsel-describe-function-preselect 'ivy-function-called-at-point)
  (advice-add 'counsel-switch-to-shell-buffer :around 'my-select-shell-buffer)

  (setq ivy-display-functions-alist
        '((ivy-completion-in-region . ivy-display-function-lv)))
  (fset 'my-recentf-command 'ivy-switch-buffer)
  (setq completion-in-region-function #'ivy-completion-in-region)
  (setq counsel-rg-base-command
        (concat
         "rg --no-heading --line-number --vimgrep "
         "--path-separator / "
         "--max-columns 120 "
         "--color never "
         "%s ."))
  (setq ivy-use-virtual-buffers t)
  :config
  (progn
    (setq ivy-height 30)
    (add-to-list 'ivy-initial-inputs-alist
                 '(counsel-switch-to-shell-buffer . "*shell*"))
    (add-to-list 'ivy-initial-inputs-alist
                 '(projectile-completing-read . "/"))
    (setq ivy-virtual-abbreviate 'full)
    (my-key-chord-define ivy-minibuffer-map "fh" 'ivy-avy)))

(use-package projectile
  :diminish projectile-mode
  :defer t
  :init
  (progn
    (my-key-chord-define global-map "pf" 'projectile-find-file)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (projectile-mode +1)))

(use-package slime
  :defer t
  :bind
  (:map
   slime-mode-map
   ("C-c s" . slime-selector))
  :hook (comint-mode-hook . rainbow-delimiters-mode)
  :config
  (progn
    (use-package slime-autoloads)
    (setq inferior-lisp-program "sbcl")
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
  :bind
  (:map
   flycheck-mode-map
   ("<f6>" . flycheck-list-errors))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

(use-package tuareg
  :defer t
  :preface
  (defun my-preserve-comment-style ()
    (setq comment-style 'indent))
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
    (set-face-foreground tuareg-font-lock-operator-face nil)
    (advice-add 'tuareg-mode :after #'my-preserve-comment-style)
    (use-package ocp-indent
      :demand t
      :bind (:map tuareg-mode-map ("C-=" . ocp-indent-buffer))
      :hook (tuareg-mode-hook . ocp-setup-indent))))

(use-package cc-vars
  :commands compile
  :preface
  (defun my-c-setup ()
    "My setup for C."
    (setq c-default-style "linux")
    (setq c-basic-offset 8)
    (setq indent-tabs-mode nil)
    (use-package cc-mode
      :bind
      (:map
       c-mode-base-map
       ("C-c C-c" . compile)
       ("C-c C-a" . ff-find-other-file))))
  :defer t
  :hook (c-initialization-hook . my-c-setup))

(use-package image-mode
  :config
  (use-package image+
    :bind
    (:map
     image-mode-map
     ("+" . imagex-sticky-zoom-in)
     ("-" . imagex-sticky-zoom-out))))

(use-package yasnippet
  :defer t
  :config (yas-reload-all))

(use-package diff-hl
  :defer t
  :bind
  (("C-M-[" . diff-hl-previous-hunk)
   ("C-M-]" . diff-hl-next-hunk)
   :map diff-hl-mode-map
   ("S-<f7>" . diff-hl-revert-hunk))
  :hook
  ((diff-hl-mode-hook . diff-hl-flydiff-mode)
   (diff-hl-mode-hook . diff-hl-margin-mode))
  :config
  (progn
    (cl-flet ((my-diff-hl-on (&rest _) (turn-on-diff-hl-mode)))
      (dolist (fun '(diff-hl-previous-hunk diff-hl-next-hunk vc-diff))
        (advice-add fun :before #'my-diff-hl-on)))
    (advice-add 'diff-hl-diff-goto-hunk :before #'my-save-all-buffers)
    (advice-add 'diff-hl-revert-hunk :around #'my-no-confirm)))

(use-package multiple-cursors
  :defer t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (use-package mc-mark-more
    :defer t
    :bind
    (:map
     mc/keymap
     ("<return>" . nil)
     ("C-s" . my-jump-to-char)
     ("C-," . mc/unmark-next-like-this)
     ("C-." . mc/skip-to-next-like-this)
     ("C-'" . mc-hide-unmatched-lines-mode))))

(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq my-undo-command 'undo-tree-undo))

(use-package wgrep
  :defer t
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit)))

(use-package hi-lock
  :defer t
  :diminish "hl"
  :preface
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
  :defer t
  :bind
  ("C-c d" . woman))

(use-package battery
  :commands display-battery-mode
  :demand t
  :config
  (and (functionp battery-status-function)
       (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
           (display-battery-mode))))

(use-package macrostep
  :defer t
  :bind
  (:map
   lisp-interaction-mode-map
   ("C-c e" . macrostep-expand)
   :map
   emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)))

(use-package erlang
  :defer t
  :preface
  (defun my-insert-erl-module-stub ()
    (when (and (= (point-min) (point-max))
               (buffer-file-name)
               (string-suffix-p ".erl" (buffer-file-name)))
      (let ((module-name (file-name-base)))
        (insert (format "-module(%s).\n-export([]).\n\n"
                        module-name)))))
  :hook (erlang-mode-hook . 'my-insert-erl-module-stub))

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces '(("TODO" . hl-todo)
                                ("XXX" . hl-todo)
                                ("NOTE" . hl-todo)
                                ("HACK" . hl-todo)
                                ("FIXME" . hl-todo)))
  :config
  (progn
    (hl-todo-set-regexp)
    (set-face-foreground 'hl-todo "red")
    (set-face-background 'hl-todo "yellow")))

(use-package hl-line
  :commands (hl-line-mode global-hl-line-mode)
  :preface
  (defun next-error-buffer-hl-line ()
    "Turn on `hl-line-mode' in buffer `next-error-last-buffer'."
    (when (and next-error-last-buffer (buffer-live-p next-error-last-buffer))
      (with-current-buffer next-error-last-buffer
        (hl-line-mode 1))))
  :hook (next-error-hook . next-error-buffer-hl-line)
  :init
  (global-hl-line-mode 1))

(use-package smartparens
  :defer t
  :preface
  (defun my-smartparens-mode-setup ()
    (add-to-list
     'sp-pairs
     (cons t
           (cl-delete-if
            (lambda (plist)
              (and (consp plist)
                   (member (plist-get plist :open)
                           '("`" "'"))))
            (cdr (assoc t sp-pairs)))))
    (sp-with-modes '(rust-mode)
      (sp-local-pair "|" "|"))
    (my-electric-pair-mode -1))
  :hook
  ((smartparens-mode-hook . my-smartparens-mode-setup)
   (prog-mode-hook . smartparens-mode))
  :init
  (use-package smartparens-config))

(use-package ispell
  :config
  (progn
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "english")))

(use-package sh-script
  :defer t
  :preface
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
      (if (looking-at "\"") (delete-char 1) (insert ?\"))))
  :bind
  (:map sh-mode-map
        ("C-c C-z" . nil)
        ("C-c q" . my-sh-quote-or-unquote))
  :hook (sh-mode-hook . my-dos2unix)
  :config
  (setq sh-basic-offset 8)
  (setq sh-indentation 8))

(use-package move-text
  :defer t
  :bind
  (("C-S-<down>" . move-text-down)
   ("C-S-n" . move-text-down)
   ("C-S-<up>" . move-text-up)
   ("C-S-p" . move-text-up)))

(use-package windmove
  :disabled t
  :bind
  (("S-<left>"  . windmove-left)
   ("S-<right>" . windmove-right)
   ("S-<up>"    . windmove-up)
   ("S-<down>"  . windmove-down)))

(use-package mb-depth
  :commands minibuffer-depth-indicate-mode
  :init
  (progn
    (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode +1)))

(use-package vimish-fold
  :defer t
  :bind
  (("C-`" . vimish-fold)
   ("C-c f" . vimish-fold-delete-all)))

(use-package restart-emacs
  :preface
  (defun my-restart-emacs (arg)
    (interactive "P")
    (if arg (save-buffers-kill-terminal)
      (condition-case nil
          (restart-emacs)
        ;; XXX on console windows-nt, restart-emacs fails...
        (error (save-buffers-kill-terminal)))))
  :bind
  (([remap save-buffers-kill-terminal] . my-restart-emacs))
  :init
  (advice-add 'save-buffers-kill-terminal :around #'my-no-confirm)
  (advice-add 'restart-emacs :around #'my-no-confirm))

(use-package whitespace
  :init (global-whitespace-mode +1)
  :config
  (set-face-foreground 'whitespace-line nil)
  (set-face-background 'whitespace-line nil))

(use-package ace-link
  :init
  (ace-link-setup-default))

(use-package rust-mode
  :defer t
  :mode ("\\.rs?$" . rust-mode)
  :config
  (progn
    (use-package cargo
      :diminish cargo-minor-mode
      :hook (rust-mode-hook . cargo-minor-mode)
      :bind
      (:map
       cargo-minor-mode-map
       ("<f5>" . cargo-process-repeat)))
    (use-package racer
      :diminish racer-mode
      :hook (rust-mode-hook . racer-mode))))

(use-package helpful
  :defer t
  :init
  (progn
    (my-key-chord-define global-map "hh" 'helpful-at-point)))

(use-package saveplace
  :config
  (when (fboundp 'save-place-mode)
    (save-place-mode +1)))

(use-package javadoc-lookup
  :defer t
  :bind
  ("C-h j" . javadoc-lookup))

(use-package pager
  :defer t
  :preface
  (defun my-scroll-up-command-pager (arg)
    "Apply `pager-page-down'.

If provided, do it ARG times."
    (interactive "P")
    (dotimes (_ (if (numberp arg) arg 1))
      (pager-page-down)))
  (defun my-scroll-down-command-pager (arg)
    "Apply `pager-page-up'.

If provided, do it ARG times."
    (interactive "P")
    (dotimes (_ (if (numberp arg) arg 1))
      (pager-page-up)))
  :init
  (fset 'my-scroll-up-command 'my-scroll-up-command-pager)
  (fset 'my-scroll-down-command 'my-scroll-down-command-pager))

(use-package string-inflection
  :defer t
  :bind ("C-S-u" . string-inflection-all-cycle))

(provide 'my-init)
;;; my-init.el ends here
