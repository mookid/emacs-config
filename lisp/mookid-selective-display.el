;; mookid-selective-display.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Selective display.  Use <f6> to toggle, C-<f6> and M-<f6> to set.

;;; Code:
(let ((depth 1))
  (define-key global-map (kbd "<f6>") 'mookid-selective-display-toggle)
  (define-key global-map (kbd "C-<f6>") 'mookid-selective-display-increase)
  (define-key global-map (kbd "S-<f6>") 'mookid-selective-display-decrease)

  (defun mookid-selective-display-toggle ()
    "Hide lines starting with a lot of spaces.

See `mookid-selective-display-increase' to increase the number of spaces.
See `mookid-selective-display-decrease' to decrease it."
    (interactive)
    (set-selective-display (unless selective-display depth)))
  (cl-flet ((g (offset)
	       (setq depth (+ depth offset))
	       (set-selective-display depth)))
    (defun mookid-selective-display-increase ()
      "Increase the cap for `toogle-selective-display'.

See `mookid-selective-display-toggle' and `mookid-selective-display-decrease'."
      (interactive)
      (when (< depth 20) (g 1)))

    (defun mookid-selective-display-decrease ()
      "Decrease the cap for `toogle-selective-display'.

See `mookid-selective-display-toggle' and `mookid-selective-display-increase'."
      (interactive)
      (when (> depth 1) (g -1)))))

(provide 'mookid-selective-display)
;;; mookid-selective-display.el ends here
