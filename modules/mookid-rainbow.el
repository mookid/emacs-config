;;; mookid-rainbow --- Rainbow delimiters and blocks configuration

;;; Commentary:
;; I enable rainbow delimiters but not rainbow blocks.
;; To customize: change the list `colors'.

;;; Code:
(require 'rainbow-delimiters)
(require 'rainbow-blocks)

(autoload 'mookid-default-font "mookid-naked-emacs-config")

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(defun mookid-rainbow-delimiters-disable ()
  "Disable rainbow-delimiters mode."
  (rainbow-delimiters-mode -1))

(add-hook 'shell-mode-hook #'mookid-rainbow-disable)

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground "red"
                    :inherit 'error
                    :box t)
(let ((colors '("green3" "orange" "pale violet red"))
      (kinds '(delimiters blocks)))
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
     do (set-level lvl (nth icolor colors)))))

(provide 'mookid-rainbow)
;;; mookid-rainbow.el ends here
