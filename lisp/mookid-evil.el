;;; mookid-evil.el --- Configuration of emacs that depends on the evil package

;;; Commentary:

;;; Code:
(autoload 'with-message "mookid-macros")
(autoload 'with-title "mookid-macros")
(autoload 'ignore-all "mookid-macros")

(with-message
 "Loading evil mode"
 (require 'evil)
 (evil-mode)
 (defvar evil-motion-state-map)
 (defvar evil-normal-state-map)
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
 (define-key evil-normal-state-map (kbd "g s") 'ex-substitute)
 (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))

(with-eval-after-load 'evil
  (with-message
   "Loading evil visualstar"
   (require 'evil-visualstar)
   (global-evil-visualstar-mode t)))

(with-eval-after-load 'evil
  (with-message
   "Loading evil jumper"
   (require 'evil-jumper)
   (evil-jumper-mode t)))

(with-eval-after-load 'evil
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
   (global-evil-search-highlight-persist t)))

(with-eval-after-load 'evil
  (with-message
   "Loading evil numbers"
   (require 'evil-numbers)
   (cl-loop
    for (key . val) in '((<f1> . evil-numbers/inc-at-pt)
			 (<f2> . evil-numbers/dec-at-pt))
    do (define-key evil-normal-state-map
	 (kbd (concat "C-" (symbol-name key))) val))))

(provide 'mookid-evil)
;;; mookid-evil.el ends here
