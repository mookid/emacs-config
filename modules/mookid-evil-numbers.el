;;; mookid-evil-numbers.el --- Configuration of evil-numbers

;;; Commentary:

;;; Code:
(with-eval-after-load 'evil
  (require 'evil-numbers)
  (defvar evil-normal-state-map)
  (cl-loop
   for (key . val) in '((<f1> . evil-numbers/inc-at-pt)
			(<f2> . evil-numbers/dec-at-pt))
   do (define-key evil-normal-state-map
	(kbd (format "C-%S" (symbol-name key))) val)))

(provide 'mookid-evil-numbers)
;;; mookid-evil-numbers.el ends here
