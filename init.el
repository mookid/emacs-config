;; -*- lexical-binding: t -*-
;;; init.el --- configuration file for emacs!

;;; Commentary:
;; My emacs config, with simple options.

;;; Code:

(package-initialize)

;; Common lisp functionalities
(with-no-warnings (require 'cl)) ; useless warning

;;; Useful macros for loading packages
(defmacro with-message (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "*** %s" ,msg) ,@body 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defmacro with-title (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "[%s]" ,msg) ,@body (message "[end]") 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defmacro ignore-all (&rest _) "Ignore arguments, which are not evaluated." nil)

(setq load-path
      (cons "~/.emacs.d/lisp/"
            load-path))

(with-title "Naked emacs configuration" (load "naked-emacs-config"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package loading and settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-title
 "Evil related packages"

 (with-message
  "Loading evil mode"
  (require 'evil)
  (evil-mode)
  (defvar evil-emacs-state-map)
  (defvar evil-motion-state-map)
  (defvar evil-insert-state-map)
  (defvar evil-visual-state-map)
  (defvar evil-normal-state-map)
  (defvar evil-replace-state-map)
  (defvar evil-operator-state-map)
  (setq-default evil-emacs-state-cursor '("purple" box))
  (setq-default evil-normal-state-cursor '("grey" box))
  (setq-default evil-visual-state-cursor '("green" box))
  (setq-default evil-insert-state-cursor '("red" bar))
  (setq-default evil-replace-state-cursor '("deep pink" box))
  (setq-default evil-motion-state-cursor '("gray" box))
  (define-key evil-normal-state-map
    (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map
    (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map
    (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map
    (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (setq-default evil-cross-lines t)
  (defun ex-substitute () (interactive) (evil-ex "%s/"))
  (define-key evil-normal-state-map (kbd "g s") 'ex-substitute))

 (with-message
  "Loading evil visualstar"
  (require 'evil-visualstar)
  (global-evil-visualstar-mode t))

 (with-message
  "Loading evil jumper"
  (require 'evil-jumper)
  (evil-jumper-mode t))

 (with-message
  "Loading evil search highlight persist"
  (require 'evil-search-highlight-persist)
  (mapc (lambda (face)
          (set-face-attribute face nil
                              :weight 'extra-bold
                              :foreground "blue"
                              :background "yellow1"))
        '(evil-search-highlight-persist-highlight-face
          isearch
          lazy-highlight))
  (setq-default evil-search-highlight-string-min-len 5)
  (global-evil-search-highlight-persist t))

 (with-message
  "Loading evil numbers"
  (require 'evil-numbers)
  (cl-loop
   for (key . val) in '((<f1> . evil-numbers/inc-at-pt)
                        (<f2> . evil-numbers/dec-at-pt))
   do (define-key evil-normal-state-map
        (kbd (concat "C-" (symbol-name key))) val))))

(with-message
 "Loading powerline"
 (require 'smart-mode-line-powerline-theme)
 (setq-default sml/no-confirm-load-theme t)
 ;; avoids a question at every startup
 (setq-default sml/theme 'powerline)
 (sml/setup))

(with-message
 "Loading rainbow delimiters and blocks"
 (require 'rainbow-delimiters)
 (require 'rainbow-blocks)
 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
 (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                     :foreground "red"
                     :inherit 'error
                     :box t)
 (let ((colors '("green" "violet" "orange red"))
       (kinds '(delimiters blocks)))
   (cl-labels ((set-bold (face color)
                         (set-face-attribute face nil
                                             :weight 'extra-bold
                                             :foreground color))
               (mk-symb (kind lvl)
                        (intern (concat "rainbow-"
                                        (prin1-to-string kind) "-depth-"
                                        (prin1-to-string lvl) "-face")))
               (set-level (lvl color)
                          (when (< 0 lvl 10)
                            (mapc (lambda (kind)
                                    (set-bold (mk-symb kind lvl) color))
                                  kinds))))
     (cl-loop
      with ncolors = (length colors)
      for lvl from 1 upto 9
      for icolor = (mod (- lvl 1) ncolors)
      do (set-level lvl (nth icolor colors))))))

(with-message
 "Loading company mode"
 (require 'company)
 (setq-default company-idle-delay 0.5)
 (setq-default company-tooltip-limit 5)
 (setq-default company-minimum-prefix-length 2)
 (setq-default company-tooltip-flip-when-above t))

(ignore-all
 "Loading paredit mode"
 (require 'paredit)
 (paredit-mode))

(with-title
 "Helm related packages"

 (with-message
  "Loading helm"
  (require 'helm)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-M-h") 'helm-recentf)
  (helm-mode))

 (with-message
  "Loading shackle"
  (require 'shackle)
  (setq-default helm-display-function #'pop-to-buffer)
  (setq-default shackle-rules
                '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.33)))
  (shackle-mode))

 (with-message
  "Loading helm swoop"
  (require 'helm-swoop)
  (global-set-key (kbd "C-\\") 'helm-swoop-from-evil-search)))

(with-message
 "Loading smartparens"
 (require 'smartparens-config)
 (show-smartparens-global-mode nil)
 (setq-default sp-autoskip-closing-pair 'always)
 (setq-default sp-hybrid-kill-entire-symbol nil)
 (sp-use-paredit-bindings)
 (define-key evil-insert-state-map (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
 (global-set-key (kbd "M-[") 'sp-backward-unwrap-sexp)
 (cl-loop
  for (key . val) in '((paren   . "(")
                       (bracket . "[")
                       (brace   . "{")
                       (squote  . "'")
                       (dquote  . "\""))
  for symb = (intern (concat "wrap-with-" (prin1-to-string key) "s"))
  for kbinding = (concat "C-c " val)
  do (eval
      `(defun ,symb (&optional arg)
         "Wrap the next form (or the selection) using `sp-wrap-with-pair'."
         (interactive "P")
         (sp-wrap-with-pair ,val)))
  do (eval `(global-set-key (kbd ,kbinding) ',symb))))

(with-message
 "Setting up flycheck"
 (require 'flycheck)
 (defvar flycheck-mode-map)
 (define-key flycheck-mode-map (kbd "C-S-<next>") 'flycheck-next-error))

(with-message
 "Setting up avy"
 (require 'avy)
 (setq-default avy-all-windows 'all-frames)
 (mapc (lambda (map) (eval `(define-key ,map (kbd "s") 'avy-goto-word-or-subword-1)))
       '(evil-motion-state-map
         evil-visual-state-map
         evil-normal-state-map)))

(ignore-all
 "Loading ace-isearch"
 (require 'ace-isearch)
 (global-ace-isearch-mode +1)
 (setq-default ace-isearch-function 'avy-goto-char))

(with-title
 "OCaml config"

 (with-message
  "Configure tuareg mode"
  (require 'tuareg)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode))
                auto-mode-alist)))

 (with-message
  "Configure ocp indent"
  (require 'ocp-indent)
  (define-key tuareg-mode-map (kbd "C-=") 'ocp-indent-buffer))

 (ignore-all
  "Configuring merlin"
  (require 'merlin)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (setq merlin-use-auto-complete-mode 'easy)
  (with-eval-after-load 'company
    (add-to-list 'company-backends
                 'merlin-company-backend))
  (add-hook 'merlin-mode-hook 'company-mode)))

(with-message
 "C settings"
 (require 'cc-mode)
 (setq-default c-default-style "linux" c-basic-offset 8)
 (define-key c-mode-base-map (kbd "C-c C-c") 'compile))

(with-message
 "Configuring highlight column"
 (require 'highlight-indentation)
 (set-face-background 'highlight-indentation-face "light steel blue")
 (set-face-background 'highlight-indentation-current-column-face "sienna4"))

(with-message
 "Loading private settings"
 (let ((f "~/.emacs.d/private.el")) (when (file-exists-p f) (load f))))

(provide 'init)
;;; init.el ends here
