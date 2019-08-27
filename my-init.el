;;; my-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs config, with simple options.

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(add-to-list 'load-path "~/.emacs.d/use-package")
(add-to-list 'load-path "~/.emacs.d/diminish.el")
(require 'use-package)
(require 'diminish)
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
(setq auto-save-default nil)
(setq frame-title-format (list mode-line-modified " %b -- %f  " '(:eval vc-mode)))
(setq minibuffer-depth-indicate-mode t)
(setq tags-add-tables nil)
(setq load-prefer-newer t)
(setq switch-to-visible-buffer nil)
(setq blink-matching-paren 't)
(setq null-device "/dev/null")
(set-default-coding-systems 'utf-8)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(defalias 'display-startup-echo-area-message 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(define-key key-translation-map [?\(] [?\[])
(define-key key-translation-map [?\[] [?\(])
(define-key key-translation-map [?\)] [?\]])
(define-key key-translation-map [?\]] [?\)])

(unless (member 'initials completion-styles)
  (setq completion-styles (append completion-styles (list 'initials))))

(progn
  (global-visual-line-mode +1)
  (setf (cdr visual-line-mode-map) nil)
  (diminish 'visual-line-mode))


;;; Windows
(defmacro my-def-balance-after (newfun orig-fun)
  "Define the function NEWFUN from ORIG-FUN.

It just applies ORIG-FUN and then balances windows."
  `(progn
     (define-key global-map [remap ,orig-fun] ',newfun)
     (defun ,newfun (&rest args)
       ,(format "Apply `%s' and then balances windows.

See `my-def-balance-after'." orig-fun)
       ,(interactive-form orig-fun)
       (apply #',orig-fun args)
       (balance-windows))))

(my-def-balance-after my-split-window-right split-window-right)
(my-def-balance-after my-split-window-below split-window-below)


;;; Keybindings
(define-key global-map (kbd "C-c u") 'my-dos2unix)
(when (boundp 'mouse-wheel-down-event)
  (let ((down-key (kbd (format "<C-%s>" mouse-wheel-down-event))))
    (define-key global-map down-key 'my-selective-display-increase)))
(when (boundp 'mouse-wheel-up-event)
  (let ((up-key (kbd (format "<C-%s>" mouse-wheel-up-event))))
    (define-key global-map up-key 'my-selective-display-decrease)))
(define-key global-map (kbd "<mode-line> <mouse-2>") 'my-kill-buffer)
(define-key global-map (kbd "C-M-;") 'backward-kill-sexp)
(define-key global-map (kbd "C-h g") 'my-google-search)
(define-key global-map (kbd "C-x K") 'my-other-window-kill-buffer)
(define-key global-map (kbd "C-x C-h") 'find-file-other-window)
(define-key global-map (kbd "C-x y") 'my-find-init-file)
(define-key global-map (kbd "C-h C-k") 'describe-key)
(define-key global-map (kbd "C-c C-v") 'my-insert-buffer-name)
(define-key global-map (kbd "C-c k") 'delete-frame)
(define-key global-map (kbd "C-c n") 'make-frame)
(define-key global-map (kbd "M-n") 'next-error)
(define-key global-map (kbd "M-p") 'previous-error)
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
(define-key global-map (kbd "C-S-<right>") 'my-next-beginning)
(define-key global-map (kbd "C-S-<left>") 'my-previous-end)
(define-key global-map (kbd "M-SPC") 'my-space-and-back)
(define-key global-map (kbd "C-<return>") (kbd "<return>"))
(define-key global-map (kbd "M-<return>") (kbd "<return>"))
(define-key global-map (kbd "C-M-<return>") (kbd "<return>"))
(define-key global-map (kbd "C-c F") 'toggle-debug-on-error)
(define-key global-map (kbd "C-c C-r")  'my-revert-buffer-noconfirm)
(define-key global-map (kbd "C-S-u") 'upcase-region)
(define-key global-map (kbd "C-S-l") 'downcase-region)
(define-key global-map (kbd "C-S-k") 'my-kill-line-backward)
(define-key global-map (kbd "C-c <f5>") 'revert-buffer)
(define-key global-map (kbd "<f2>") 'rename-buffer)
(define-key global-map (kbd "M-j") 'my-delete-indentation-forward)
(define-key global-map (kbd "C-c T") 'my-insert-todo)
(define-key global-map (kbd "C-x C-S-<left>") 'my-all-previous-buffer)
(define-key global-map (kbd "C-x C-S-<right>") 'my-all-next-buffer)
(define-key global-map (kbd "`") 'my-other-window-or-switch-buffer)
(define-key global-map (kbd "~") 'my-toggle-capitalization)
(define-key global-map (kbd "C-M-y") 'my-yank-diff)
(define-key global-map (kbd "C-x M-o") 'occur)
(define-key global-map (kbd "M-u") 'upcase-dwim)
(define-key global-map (kbd "M-l") 'downcase-dwim)
(define-key global-map (kbd "M-c") 'capitalize-dwim)
(define-key global-map (kbd "C-h l") 'find-library)
(define-key global-map (kbd "C-c g") 'my-gitk)
(define-key global-map (kbd "<C-next>") 'my-scroll-up-3lines)
(define-key global-map (kbd "<C-prior>") 'my-scroll-down-3lines)


;;; Unbound keys
(define-key global-map (kbd "<insert>") nil)
(define-key global-map (kbd "C-\\") nil)


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
(when (version<= "26.1" emacs-version)
  (setq mode-line-percent-position '(6 "%q")))

(defvar my-ring-bell-timer nil
  "Timer used for the ring-bell.")

(defun my-visual-ring-bell ()
  "My value for `ring-bell-function'."
  (when my-ring-bell-timer
    (cancel-timer my-ring-bell-timer))
  (set-face-inverse-video 'mode-line t)
  (setq my-ring-bell-timer (run-with-timer 0.1 nil 'set-face-inverse-video 'mode-line nil)))

(setq ring-bell-function 'my-visual-ring-bell)

(defvar my-font-list nil
  "Font configuration for `default-frame-alist'.")

(setq my-font-list
      '((:font-name "Menlo" :font-size 18)
        (:font-name "Consolas" :font-size 14)
        (:font-name "DejaVu Sans Mono" :font-size 14)))

(defvar my-color-theme
  nil
  "The selected color theme.")
(setq my-color-theme 'my-color)

(defvar my-font nil
  "The selected font.")

(with-eval-after-load 'init
  (let ((font-specs my-font-list) (font))
    (while (and font-specs (not font))
      (cl-destructuring-bind (&key font-name font-size) (pop font-specs)
        (when (and font-name (find-font (font-spec :name font-name)))
          (setq font (concat font-name " " (prin1-to-string font-size))))))
    (when font
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-face-font 'default font)
      (setq my-font font)))
  (cl-destructuring-bind
      (_ ofs-x _ display-pixel-width display-pixel-height)
      (assoc 'workarea (car (display-monitor-attributes-list)))
    (let* ((column-pixel-width 80)
           (max-height (/ display-pixel-height (window-font-height)))
           (height (- max-height 4))
           (columns-to-display
            (/ (- display-pixel-width (* 2 ofs-x))
               (* column-pixel-width (window-font-width))))
           (width (* column-pixel-width columns-to-display)))
      (cond ((< 1 columns-to-display)
             (add-to-list 'default-frame-alist `(height . ,height))
             (add-to-list 'default-frame-alist `(width . ,width))
             (set-frame-height nil height)
             (set-frame-width nil width)
             (add-to-list 'initial-frame-alist `(top . ,(window-font-height)))
             (add-to-list 'initial-frame-alist `(left . ,(* 2 ofs-x))))
            (t
             (setq columns-to-display 2)
             (set-frame-parameter nil 'fullscreen 'maximized)))
      (dotimes (_ (- columns-to-display (count-windows)))
        (split-window-right))
      (balance-windows)
      (condition-case nil
          (load-theme my-color-theme t)
        (warn (format "Error during loading of theme %s" my-color-theme))))))


;;; aliases
(defmacro my-defalias-and-rebind (name command map)
  "Define the alias NAME as a synonym for COMMAND and rebind it in MAP."
  `(progn
     (defalias ',name ',command)
     (define-key ,map [remap ,command] ',name)))

(my-defalias-and-rebind my-undo-command undo global-map)
(my-defalias-and-rebind my-switch-buffer-command switch-to-buffer global-map)
(my-defalias-and-rebind my-switch-buffer-other-window-command switch-to-buffer-other-window global-map)
(my-defalias-and-rebind my-scroll-up-command scroll-up-command global-map)
(my-defalias-and-rebind my-scroll-down-command scroll-down-command global-map)


;;; defuns
(defvar my-selective-display-width 1
  "Last non nil value of `selective-display'.")

(defun my-selective-display--incf (offset)
  "Increments `selective-display' by OFFSET."
  (setq my-selective-display-width (+ my-selective-display-width offset))
  (set-selective-display my-selective-display-width))

(defun my-selective-display-increase ()
  "Increase the cap for `selective-display'."
  (interactive)
  (when (< my-selective-display-width 20)
    (my-selective-display--incf 2)))

(defun my-selective-display-decrease ()
  "Decrease the cap for `selective-display'."
  (interactive)
  (when (> my-selective-display-width 1)
    (my-selective-display--incf -2)))

(defun my-toggle-capitalization ()
  (interactive)
  (let ((case-fold-search nil))
    (cond
     ((minibufferp) (self-insert-command 1))
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point))))
     (t (self-insert-command 1)))))

(defun my-insert-todo ()
  "Insert a TODO note above the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (comment-dwim nil)
  (insert "TODO(NM) "))

(defun my-gitk ()
  (interactive)
  (start-process "gitk" nil "gitk" "--all"))

(defun my-on-all-visible-windows (fun &rest args)
  "Apply FUN with ARGS on each visible window."
  (walk-windows
   (lambda (window)
     (with-selected-window window
       (apply fun args)))
   nil
   'visible))

(defun my-all-previous-buffer ()
  "Go to the previous buffer in each visible window."
  (interactive)
  (my-on-all-visible-windows 'previous-buffer))

(defun my-all-next-buffer ()
  "Go to the next buffer in each visible window."
  (interactive)
  (my-on-all-visible-windows 'next-buffer))

(defvar my-pairs-alist
  '((?\" . ?\")
    (?\( . ?\))
    (?\{ . ?\})
    (?\[ . ?\]))
  "Supported delimiters for `my-delete-pair' and `my-cycle-pair'.")
(defun my-delete-pair ()
  ;; TODO(NM): should calling that twice redo?
  "Remove the pair of parenthesis around the point, or undo it."
  (interactive)
  (cond ((equal last-command this-command)
         (my-undo-command))
        (t
         (save-excursion
           (cond ((rassoc (char-after (1- (point))) my-pairs-alist)
                  (backward-sexp)
                  (delete-pair))
                 ((assoc (char-after (point)) my-pairs-alist)
                  (delete-pair))
                 (t
                  (error "Not on a parenthesis-like character")))))))
(let (my-delete-pair-last)
  (defun my-cycle-pair ()
    "Toggle the parenthesis pair around the point and set mark at the other end.

The pairs depend on the value of `my-pairs-alist'.  The command
fails if the point is not either on an opening parenthesis
character or right after a closing one."
    (interactive)
    (cl-flet ((k (open-pos close-pos pairs end-pos)
                 (cl-destructuring-bind
                     ((open . close) . next-pairs)
                     (or pairs my-pairs-alist)
                   (goto-char open-pos) (delete-char 1) (insert open)
                   (goto-char close-pos) (delete-char 1) (insert close)
                   (goto-char end-pos)
                   (setq my-delete-pair-last
                         (list next-pairs open-pos close-pos)))))
      (if (equal last-command this-command)
          (cl-destructuring-bind
              (pairs open-pos close-pos)
              my-delete-pair-last
            (k open-pos close-pos pairs (point)))
        (let ((start-point (point))
              cell
              open-pos
              close-pos)
          (cond ((setq cell (cl-member (char-after (1- (point))) my-pairs-alist :key 'cdr))
                 (setq close-pos (1- (point)))
                 (backward-sexp)
                 (setq open-pos (point)))
                ((setq cell (cl-member (char-after (point)) my-pairs-alist :key 'car))
                 (setq open-pos (point))
                 (forward-sexp)
                 (setq close-pos (1- (point))))
                (t
                 (error "Not on a parenthesis-like character")))
          (k open-pos close-pos (cdr-safe cell) start-point)
          (set-mark open-pos))))))

(defun my-dos2unix ()
  "Convert the current buffer to unix style eol."
  (interactive)
  (set-buffer-file-coding-system 'unix t)
  (save-buffer))

(defun my-face-at-point ()
  "Computes the face at point and its documentation.

See `my-face-at-point-mode'."
  (let ((face (face-at-point)))
    (when face
      (format "%S: %s" face (face-documentation face)))))

(defun my-describe-face-at-point ()
  "Call `describe-face' skipping the interactive prompt."
  (interactive)
  (describe-face (face-at-point)))

(define-minor-mode my-face-at-point-mode
  "Instruments `eldoc-mode' to describe the face at point."
  :lighter " face"
  (remove-function (local 'eldoc-documentation-function)
                   #'my-face-at-point)
  (when my-face-at-point-mode
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'my-face-at-point)))

(defun my-delete-indentation-forward ()
  "Implementation of vi's J command."
  (interactive)
  (delete-indentation t))

(defvar my-narrowed-buffers nil
  "Variable that holds the buffers created by `my-narrow-to-region'.")
(defun my-kill-buffer-on-widen ()
  "Kill current buffer if it has been created by `my-narrow-to-region.

Also make such killed buffers eligible for GC."
  (when (member (current-buffer) my-narrowed-buffers)
    (kill-buffer))
  (setq my-narrowed-buffers
        (cl-remove-if-not #'buffer-live-p my-narrowed-buffers)))
(advice-add 'widen :after #'my-kill-buffer-on-widen)

(defun my-narrow-to-region (start end)
  "Narrow the region between START and END in an indirect buffer.

Such indirect buffers are automatically killed on `widen'."
  (interactive "r")
  (when (region-active-p)
    (deactivate-mark t))
  (let ((cloned-buffer (clone-indirect-buffer nil nil)))
    (with-current-buffer cloned-buffer
      (narrow-to-region start end))
    (push cloned-buffer my-narrowed-buffers)
    (switch-to-buffer cloned-buffer)))
(define-key global-map [remap narrow-to-region] 'my-narrow-to-region)

(cl-flet ((always-yes (&rest _) t))
  (defun my-no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(defun my-occur-skip-gribberish-hook (&rest _)
  "Skip the first line of the results buffer created by `occur-mode'."
  (when isearch-mode (isearch-exit))
  (select-window (get-buffer-window "*Occur*"))
  (goto-char (point-min))
  (next-logical-line 1)
  (recenter 0)
  (select-window (next-window)))
(advice-add 'occur :after #'my-occur-skip-gribberish-hook)

(defun my-revert-buffer-noconfirm ()
  "Revert current buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun my-find-init-file ()
  "Jump to the file where most of the configuration is defined."
  (interactive)
  (find-file my-main-init-file))

(defun my-find-shell-config-file ()
  "Jump to the .bashrc file."
  (interactive)
  (find-file (expand-file-name ".bashrc" (getenv "HOME"))))

(defun my-view-echo-area-messages (arg)
  "View the log of recent echo-area messages: the `*Messages*' buffer.

With prefix argument ARG, open the buffer in another window;
otherwise, in a new frame."
  (interactive "P")
  (let ((frame (if arg (selected-frame) (make-frame))))
    (save-excursion
      (select-frame frame)
      (view-echo-area-messages))))
(define-key global-map [remap view-echo-area-messages]
  'my-view-echo-area-messages)

(defun my-name-last-kbd-macro ()
  "Give a name to the last macro and copy it to the kill ring."
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

(defun my-scroll-up-3lines ()
  (interactive)
  (scroll-up-line 3))

(defun my-scroll-down-3lines ()
  (interactive)
  (scroll-down-line 3))

(defmacro my-with-other-window (&rest body)
  "Go to the other window, apply BODY, and go back."
  `(let ((buf-name (buffer-name))
         (orig-window (selected-window)))
     (select-window (next-window))
     (if (string= buf-name (buffer-name))
         (error "No next window")
       (unwind-protect
           (progn ,@body)
         (select-window orig-window)))))
(put 'my-with-other-window 'lisp-indent-function 0)

(defun my-scroll-down-other-window (&optional arg)
  "Scroll down in the other window."
  (interactive)
  (my-with-other-window
    (my-scroll-down-command arg)))

(defun my-scroll-up-other-window (&optional arg)
  "Scroll up in the other window."
  (interactive)
  (my-with-other-window
    (my-scroll-up-command arg)))

(defmacro my-save-column (&rest body)
  "Apply BODY and restore the current column afterwards."
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'my-save-column 'lisp-indent-function 0)

(defun my-clone-line ()
  "Clone the current line."
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
  "Copy from point to the end of line.

If provided, kill N lines forward."
  (interactive "p")
  (copy-region-as-kill (point) (line-end-position n)))

(defun my-space-and-back ()
  "Insert a space without moving the point."
  (interactive)
  (insert " ")
  (backward-char 1))

(defun my-other-window-or-switch-buffer ()
  "Switch to another window or to the next buffer."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer nil)
    (other-window 1)))

(defun my-kill-region-or-whole-line (&rest args)
  "Kill either the region if activated or the current line."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (apply 'kill-whole-line args)))
(define-key global-map [remap kill-region] 'my-kill-region-or-whole-line)

(defun my-other-window-kill-buffer ()
  "Kill the buffer in the other window."
  (interactive)
  (my-with-other-window
    (kill-buffer (current-buffer))))

(defun my-yank-clone (orig-fun &rest args)
  "Clone either the region if activated or the next sexp.

Do that with C-u C-u as prefix argument."
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
    (remove-hook 'focus-out-hook #'my-save-buffers-without-untabification)
    (remove-hook 'before-save-hook #'my-untabify-buffer t)
    (setq my-untabify-this-buffer
          (not (or (derived-mode-p 'makefile-mode)
                   (search-forward "\t" nil t))))
    (when my-untabify-mode
      (add-hook 'focus-out-hook #'my-save-buffers-without-untabification)
      (add-hook 'before-save-hook #'my-untabify-buffer nil t)))
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
(use-package dash
  :defer t
  :load-path "~/.emacs.d/dash.el")

(use-package dash-functional
  :defer t
  :load-path "~/.emacs.d/dash.el")

(use-package s
  :defer t
  :load-path "~/.emacs.d/s.el")

(use-package f
  :defer t
  :load-path "~/.emacs.d/f.el")

(use-package elisp-refs
  :after loop
  :defer t
  :load-path "~/.emacs.d/elisp-refs")

(use-package loop
  :defer t
  :load-path "~/.emacs.d/loop.el")

(use-package epl
  :defer t
  :load-path "~/.emacs.d/epl")

(use-package pkg-info
  :defer t
  :commands pkg-info-version-info
  :load-path "~/.emacs.d/pkg-info.el")

(use-package dabbrev
  :init
  (setq dabbrev-case-fold-search nil))

(use-package re-builder
  :defer t
  :preface
  (defun my-re-builder-kill-buffer-hook (orig-fun &rest args)
    (let ((reb-win (when (eq major-mode 'reb-mode)
                     (kill-new (string-trim (buffer-string)))
                     (selected-window))))
      (apply orig-fun args)
      (when reb-win
        (delete-window reb-win))))
  :init
  (advice-add 'kill-buffer :around #'my-re-builder-kill-buffer-hook))

(use-package delsel
  :init
  (delete-selection-mode 1))

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
  (global-auto-revert-mode +1)
  (advice-add 'revert-buffer :before #'my-save-all-buffers))

(use-package diff
  :preface
  (defun my-display-buffer-diff-hook (&rest _args)
    (when (and (string= "*Shell Command Output*"
                        (buffer-name (current-buffer)))
               (save-excursion
                 (goto-char (point-min))
                 (looking-at "diff\\|commit")))
      (diff-mode)))
  :hook (diff-mode-hook . read-only-mode)
  :init
  (advice-add 'display-buffer :after #'my-display-buffer-diff-hook))

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
  (defun my-vc-switch-to-buffer-other-window ()
    (interactive)
    (let* ((buffer-name "*vc-diff*")
           (buffer-window (get-buffer-window buffer-name)))
      (if buffer-window
          (select-window buffer-window)
        (switch-to-buffer-other-window buffer-name)
        (diff-mode))))
  :bind
  (("<f7>" . my-vc-switch-to-buffer-other-window)
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
           (add-hook 'prog-mode-hook 'electric-pair-local-mode)
           (electric-pair-local-mode 1))
          ((fboundp 'electric-pair-local-mode)
           (remove-hook 'prog-mode-hook 'electric-pair-local-mode)
           (electric-pair-local-mode -1))
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

(use-package benchmark-init-modes)

(use-package key-chord
  :load-path "~/.emacs.d/key-chord"
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
  (my-key-chord-define global-map "hf" 'my-switch-buffer-command)
  (my-key-chord-define global-map "fb" 'my-find-shell-config-file)
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

(use-package hydra
  :load-path "~/.emacs.d/hydra"
  :config
  (progn
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
      ("b" my-switch-buffer-command "buffer" :exit t)
      ("B" my-switch-buffer-other-window-command "buffer" :exit t)
      ("p" counsel-git "project" :exit t)
      ("q" nil "quit"))
    (defhydra my-cycle-pair (global-map "C-c")
      "parenthesis"
      ("(" my-delete-pair "delete")
      (")" my-cycle-pair "cycle"))))

(use-package dired
  :defer t
  :hook (dired-mode-hook . dired-hide-details-mode)
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
    (setq dired-recursive-copies 'always)))

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
  "A simple prompt with a default value.

The default value for prompt is either the first line of the
region if activated, or the word at point."
  (cond ((and transient-mark-mode mark-active)
         (let* ((region-beginning (region-beginning))
                (region-end (region-end))
                (eol (save-excursion
                       (goto-char region-beginning)
                       (line-end-position))))
           (buffer-substring-no-properties region-beginning
                                           (min region-end eol))))
        (t
         (current-word))))

(defmacro my-defweb-search (name template)
  "Define a web search command named NAME from TEMPLATE."
  `(defun ,name (query)
     ,(format "Search QUERY on the engine %s.

When called interactively, QUERY defaults to the word at point."
              (substring template 0 (string-match "/" template)))
     (interactive (list (read-string "query: " (my-prompt))))
     (browse-url (format "https://%s%s"
                         ,template
                         (replace-regexp-in-string search-whitespace-regexp
                                                   "+"
                                                   query)))))
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
  :load-path "~/.emacs.d/iedit"
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
  (defun my-eval-command ()
    (interactive)
    (if (region-active-p) (eval-region (region-beginning) (region-end))
      (eval-buffer)))
  (setq initial-major-mode 'emacs-lisp-mode)
  (setq eval-expression-print-level nil)
  (use-package elisp-mode
    :bind
    ("C-c C-z" . ielm)
    (:map
     emacs-lisp-mode-map
     ("C-c C-k" . my-eval-command)))
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
  (setq mouse-drag-and-drop-region t)
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
  (unless (fboundp 'isearch-yank-until-char)
    (defun isearch-yank-until-char (char)
      "Pull everything until next instance of CHAR from buffer into search string.
Interactively, prompt for CHAR."
      (interactive "cYank until character: ")
      (isearch-yank-internal
       (lambda () (let ((inhibit-field-text-motion t))
		    (search-forward (char-to-string char))
		    (forward-char -1)
		    (point)))))

    (define-key isearch-mode-map "\M-\C-c" 'isearch-yank-until-char))

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
   ("C-r" . counsel-minibuffer-history)
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
   ("C-M-o" . swiper-from-isearch)
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
    (cl-destructuring-bind (lo . hi) comint-last-prompt
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
    :hook (comint-output-filter-functions . comint-strip-ctrl-m))
  (use-package shell
    :if (not (display-graphic-p))
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
  :load-path "~/.emacs.d/evil-nerd-commenter"
  :defer t
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         ("C-c c". evilnc-copy-and-comment-lines)))

(use-package lispy
  :load-path "~/.emacs.d/lispy"
  :commands lispy-set-key-theme
  :defer t
  :init
  :hook
  ((emacs-lisp-mode-hook . lispy-mode)
   (lisp-mode-hook . lispy-mode))
  :bind
  (:map lispy-mode-map-special
        ("N" . nil)
        ("W" . nil)
        ("~" . nil))
  :config
  (lispy-set-key-theme '(special)))

(use-package rainbow-delimiters
  :load-path "~/.emacs.d/rainbow-delimiters"
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
                      :reverse-video t))

(use-package rainbow-mode
  :load-path "~/.emacs.d/rainbow-mode"
  :commands (rainbow-mode)
  :defer t)

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
     ("SPC" . 'my-company-abort-with-space)
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
  :load-path "~/.emacs.d/swiper"
  :preface
  (defun my-ivy-abort ()
    "Wipe the minibuffer if not empty, otherwise quit."
    (interactive)
    (cond ((> (length ivy-text) 0)
           (delete-minibuffer-contents))
          (t
           (minibuffer-keyboard-quit))))
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
  (defun ivy-display-function-popup (text)
    (require 'popup)
    (with-ivy-window
      (popup-tip
       (substring text 1)
       :nostrip nil)))
  :demand t
  :diminish ivy-mode
  :bind
  (("C-S-r" . ivy-resume)
   :map ivy-minibuffer-map
   ([remap kill-region] . my-ivy-kill-region-or-whole-line)
   ("C-g" . my-ivy-abort)
   ("C-o" . ivy-occur)
   ("<next>" . ivy-scroll-up-command)
   ("<prior>" . ivy-scroll-down-command)
   ("<right>" . ivy-alt-done))
  :init
  (use-package ivy-xref
    :load-path "~/.emacs.d/ivy-xref"
    :commands ivy-xref-show-xrefs
    :defer t
    :init
    (setq ivy-xref-use-file-path t)
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
  (setq counsel-describe-function-preselect 'ivy-function-called-at-point)
  (setq ivy-display-functions-alist
        '((ivy-completion-in-region . ivy-display-function-lv)))
  (setq completion-in-region-function #'ivy-completion-in-region)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (use-package ivy-hydra
    :load-path "~/.emacs.d/swiper")
  :config
  (fset 'my-switch-buffer-command 'ivy-switch-buffer)
  (fset 'my-switch-buffer-other-window-command 'ivy-switch-buffer-other-window)
  (setq ivy-height 10)
  (setq ivy-virtual-abbreviate 'full)
  (use-package avy
    :load-path "~/.emacs.d/avy"
    :init
    (my-key-chord-define ivy-minibuffer-map "fh" 'ivy-avy)))

(use-package counsel
  :load-path "~/.emacs.d/swiper"
  :commands counsel-rg
  :preface
  (defun my-counsel-rg (p)
    (interactive "P")
    (let* ((directory (if p (read-directory-name "counsel from directory: ")))
           (current-prefix-arg nil))
      (counsel-rg (my-prompt) directory)))
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ("C-c b" . counsel-bookmark)
   ("C-x p" . counsel-git)
   ("<f8>" . my-counsel-rg)
   ("M-i" . counsel-imenu)
   ("C-c M-x" . counsel-M-x)
   ("<f10>" . counsel-git-checkout))
  :init
  (advice-add 'counsel-switch-to-shell-buffer :around 'my-select-shell-buffer)
  :config
  (fset 'my-switch-buffer-command 'counsel-switch-buffer)
  (fset 'my-switch-buffer-other-window-command 'counsel-switch-buffer-other-window)
  (add-to-list 'ivy-initial-inputs-alist
               '(counsel-switch-to-shell-buffer . "*shell*"))
  (setq counsel-describe-function-preselect 'ivy-function-called-at-point))

(use-package counsel
  :load-path "~/.emacs.d/swiper"
  :if (display-graphic-p)
  :bind
  ("C-z" . counsel-switch-to-shell-buffer))

(use-package expand-region
  :load-path "~/.emacs.d/expand-region.el"
  :defer t
  :bind (([remap mark-sexp] . er/expand-region))
  :config
  (setq expand-region-contract-fast-key "z"
        expand-region-reset-fast-key "x"
        expand-region-fast-keys-enabled t))

(use-package flycheck
  :load-path "~/.emacs.d/flycheck"
  :commands (flycheck-mode)
  :defer t
  :preface
  (defun my-rust-flycheck-setup ()
    (with-eval-after-load 'flycheck
      (flycheck-select-checker 'rust)))
  :hook (rust-mode-hook . my-rust-flycheck-setup)
  :bind
  (:map
   flycheck-mode-map
   ("<f6>" . flycheck-list-errors))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

(use-package cc-vars
  :commands compile
  :preface
  (defun my-set-indentation (n)
    "Set indentation for c-like modes according to N."
    (interactive "P")
    (setq c-basic-offset
          (cl-etypecase n
            (null (read-number "set indentation to: " 2))
            (string (number-to-string n))
            (number n)))
    (message "c-basic-offset = %d" c-basic-offset))
  (defun my-c-setup ()
    "My setup for C/C++/C#."
    (setq c-default-style "bsd")
    (setq c-basic-offset 4)
    (setq indent-tabs-mode nil)
    (use-package cc-mode
      :bind
      (:map
       c-mode-base-map
       ("C-c C-c" . compile)
       ("C-c C-a" . ff-find-other-file))))
  :init
  (setq-default c-basic-offset 4)
  :defer t
  :hook ((c-initialization-hook . my-c-setup)
         (c++-mode-hook . my-c-setup)
         (c-mode-hook . my-c-setup)))

(use-package yasnippet
  :defer t
  :config (yas-reload-all))

(use-package diff-hl
  :load-path "~/.emacs.d/diff-hl"
  :defer t
  :bind
  ([remap vc-diff] . diff-hl-diff-goto-hunk)
  :config
  (advice-add 'diff-hl-diff-goto-hunk :before #'my-save-all-buffers))

(use-package multiple-cursors
  :load-path "~/.emacs.d/multiple-cursors.el"
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
  :load-path "~/.emacs.d/undo-tree"
  :commands (global-undo-tree-mode undo-tree-undo undo-tree-redo)
  :demand t
  :diminish undo-tree-mode
  :bind
  ("C-?" . undo-tree-redo)
  :config
  (global-undo-tree-mode)
  (defun my-turn-on-undo-tree-mode (&rest _)
    (interactive)
    (undo-tree-mode 1))
  (fset 'turn-on-undo-tree-mode 'my-turn-on-undo-tree-mode)
  (fset 'my-undo-command 'undo-tree-undo))

(use-package wgrep
  :load-path "~/.emacs.d/Emacs-wgrep"
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
  :load-path "~/.emacs.d/macrostep"
  :defer t
  :bind
  (:map
   lisp-interaction-mode-map
   ("C-c e" . macrostep-expand)
   :map
   emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)))

(use-package hl-todo
  :load-path "~/.emacs.d/hl-todo"
  :hook (prog-mode-hook . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces '(("TODO" . hl-todo)
                                ("XXX" . hl-todo)
                                ("NOTE" . hl-todo)
                                ("HACK" . hl-todo)
                                ("FIXME" . hl-todo)))
  :config
  (set-face-foreground 'hl-todo "red")
  (set-face-background 'hl-todo "yellow"))

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
  (global-hl-line-mode -1))

(use-package smartparens
  :load-path "~/.emacs.d/smartparens"
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
  (setq sh-basic-offset 8))

(use-package move-text
  :load-path "~/.emacs.d/move-text"
  :defer t
  :bind
  (("C-S-n" . move-text-down)
   ("C-S-p" . move-text-up)))

(use-package mb-depth
  :commands minibuffer-depth-indicate-mode
  :init
  (progn
    (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode +1)))

(use-package vimish-fold
  :load-path "~/.emacs.d/vimish-fold"
  :defer t
  :bind
  (("C-=" . vimish-fold)
   ("C-c f" . vimish-fold-delete-all)))

(use-package restart-emacs
  :defer t
  :load-path "~/.emacs.d/restart-emacs"
  :preface
  (defun my-restart-emacs (arg)
    (interactive "P")
    (if arg (save-buffers-kill-terminal)
      (condition-case nil
	  (progn
	    (require 'restart-emacs)
	    (restart-emacs))
        ;; XXX on console windows-nt, restart-emacs fails...
        (error (save-buffers-kill-terminal)))))
  :bind
  (([remap save-buffers-kill-terminal] . my-restart-emacs))
  :init
  (advice-add 'save-buffers-kill-terminal :around #'my-no-confirm)
  (advice-add 'restart-emacs :around #'my-no-confirm))

(use-package ace-link
  :load-path "~/.emacs.d/ace-link"
  :commands (ace-link-setup-default)
  :init
  (ace-link-setup-default))

(use-package rust-mode
  :load-path "~/.emacs.d/rust-mode"
  :defer t
  :mode ("\\.rs?$" . rust-mode)
  :config
  (progn
    (use-package cargo
      :load-path "~/.emacs.d/cargo.el"
      :diminish cargo-minor-mode
      :hook (rust-mode-hook . cargo-minor-mode)
      :init
      (setq cargo-process--command-check "check --tests")
      (setq cargo-process--command-build "build --release")
      (advice-add 'cargo-process--start :before #'my-save-all-buffers)
      :bind
      (:map
       cargo-minor-mode-map
       ("<f5>" . cargo-process-repeat)))
    (use-package racer
      :load-path "~/.emacs.d/emacs-racer"
      :diminish racer-mode
      :hook (rust-mode-hook . racer-mode))))

(use-package helpful
  :load-path "~/.emacs.d/helpful"
  :commands (helpful-symbol)
  :defer t
  :bind
  (("C-h h" . helpful-symbol)))

(use-package saveplace
  :config
  (when (fboundp 'save-place-mode)
    (save-place-mode +1)))

(use-package javadoc-lookup
  :defer t
  :bind
  ("C-h j" . javadoc-lookup))

(use-package pager
  :commands (pager-page-down pager-page-up)
  :load-path "~/.emacs.d/pager"
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
  :load-path "~/.emacs.d/string-inflection"
  :defer t
  :bind ("C-S-u" . string-inflection-all-cycle))

(use-package bm
  :load-path "~/.emacs.d/bm"
  :defer t
  :bind
  (("<C-f2>" . bm-toggle)
   ("<f2>" . bm-next)
   ("<S-f2>" . bm-previous))
  :init
  (setq bm-cycle-all-buffers t)
  :config
  (set-face-foreground bm-face nil)
  (set-face-background bm-face "#FFEAFF"))

(use-package csharp-mode
  :hook ((csharp-mode-hook . my-c-setup)))

(use-package subword
  :diminish subword-mode
  :init (global-subword-mode 1))

(use-package view
  :bind
  (("C-c q" . view-mode)
   :map view-mode-map
   ("w" . forward-word)
   ("b" . backward-word)
   ("(" . backward-paragraph)
   (")" . forward-paragraph)
   ("N" . View-search-last-regexp-backward)
   ("G" . View-goto-percent)
   ("<return>" . view-mode)
   ("i" . view-mode)
   ("j" . next-line)
   ("k" . previous-line)
   ("?" . View-search-regexp-backward)))

(use-package go-eldoc
  :load-path "~/.emacs.d/emacs-go-eldoc"
  :commands go-eldoc-setup)

(use-package golint
  :load-path "~/.emacs.d/lint/misc/emacs"
  :bind
  (:map go-mode-map
        ("C-c C-l" . golint)))

(use-package go-mode
  :load-path "~/.emacs.d/go-mode.el"
  :hook
  ((before-save-hook . gofmt-before-save)
   (go-mode . go-eldoc-setup))
  :bind
  (:map go-mode-map
        ("C-c C-c" . compile)))

(use-package profiler
  :bind
  ("C-c p s" . my-profile-start)
  ("C-c p RET" . profiler-report)
  :init
  (defun my-profile-start ()
    (interactive)
    (ignore-errors (profiler-start 'cpu+mem))))

(use-package vc-msg
  :bind
  ("C-x v c" . vc-msg-show))

(use-package highlight-symbol
  :load-path "~/.emacs.d/highlight-symbol.el"
  :commands highlight-symbol-mode
  :bind
  ("C-S-<up>" . highlight-symbol-prev)
  ("C-S-<down>" . highlight-symbol-next)
  :init
  (highlight-symbol-mode 1)
  (setq highlight-symbol-idle-delay 0.2))

(use-package markdown-mode
  :load-path "~/.emacs.d/markdown-mode"
  :commands markdown-mode
  :defer t)

(provide 'my-init)
;;; my-init.el ends here
