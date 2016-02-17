;;; mookid-utils --- Useful snippets of elisp.

;;; Commentary:

;;; Code:
(defun sds-old ()
  "Make an old copy."
  (interactive)
  (copy-file (buffer-file-name) (concat (buffer-file-name) ".old") 1 t)
  (ignore-errors
    (set-file-modes (buffer-file-name)
                    (eval-when-compile (+ 4 (* 8 4) (* 64 6)))))
  (setq buffer-read-only nil))

(provide 'mookid-utils)
;;; mookid-utils.el ends here
