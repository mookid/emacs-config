;;; mookid-rainbow --- Rainbow delimiters and blocks configuration

;;; Commentary:
;; I enable rainbow delimiters but not rainbow blocks.
;; To customize: change the list `colors'.

;;; Code:
(require 'rainbow-delimiters)
(require 'rainbow-blocks)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground "red"
                    :inherit 'error
                    :box t)
(let ((colors '(green violet tomato))
      (kinds '(delimiters blocks)))
  (cl-labels ((set-bold (face color)
			(set-face-attribute face nil
					    :weight 'ultra-bold
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
     do (set-level lvl (symbol-name (nth icolor colors))))))

(provide 'mookid-rainbow)
;;; mookid-rainbow.el ends here
