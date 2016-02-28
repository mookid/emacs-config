;; mookid-selective-display.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Selective display. Use <f6> to toggle, C-<f6> and M-<f6> to set.

;;; Code:
(let ((depth 1))
  (global-set-key (kbd "<f6>")
		  (defun toggle-selective-display ()
		    "Hide lines starting with a lot of spaces.

See `increase-selective-display' to increase the number of spaces.
See `decrease-selective-display' to decrease it."
		    (interactive)
		    (set-selective-display (unless selective-display depth))))
  (cl-flet ((g (offset)
	       (setq depth (+ depth offset))
	       (set-selective-display depth)))
    (global-set-key (kbd "C-<f6>")
		    (defun increase-selective-display ()
		      "Increase the cap for `toogle-selective-display'.

See `toggle-selective-display' and `decrease-selective-display'."
		      (interactive)
		      (when (< depth 20) (g 1))))
    (global-set-key (kbd "S-<f6>")
		    (defun decrease-selective-display ()
		      "Decrease the cap for `toogle-selective-display'.

See `toggle-selective-display' and `increase-selective-display'."
		      (interactive)
		      (when (> depth 1) (g -1))))))

(provide 'mookid-selective-display)
;;; mookid-selective-display.el ends here
