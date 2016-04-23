;;; mookid-mouse.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration of mouse actions.

;;; Code:
(require 'mouse)
(global-unset-key (kbd "<S-down-mouse-1>"))
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-mark)
(setq mouse-drag-copy-region t)
(setq mouse-yank-at-point t)

(setq mouse-autoselect-window t)

(require 'mouse-copy)
(define-key global-map (kbd "<C-down-mouse-1>") 'mouse-drag-secondary-pasting)
(define-key global-map (kbd "<C-S-down-mouse-1>") 'mouse-drag-secondary-moving)

(setq mouse-drag-copy-region t)
(define-key global-map (kbd "<C-wheel-up>") 'text-scale-increase)
(define-key global-map (kbd "<C-wheel-down>") 'text-scale-decrease)

(defun mookid-mouse-isearch (click)
  "Start `isearch-forward-symbol-at-point' on CLICK."
  (interactive "e")
  (goto-char (posn-point (event-start click)))
  (isearch-forward-symbol-at-point))

(define-key global-map (kbd "<mouse-3>") 'mookid-mouse-isearch)

(define-key isearch-mode-map (kbd "<mouse-3>") 'isearch-repeat-forward)

(define-key global-map (kbd "<mode-line> <down-mouse-1>") 'enlarge-window)

(global-unset-key (kbd "<mode-line> <mouse-2>"))
(define-key global-map (kbd "<mode-line> <down-mouse-2>") 'delete-other-windows-vertically)

(require 'mouse-drag)
(defun mookid-mouse-drag-throw (start-event)
  "Similar to `mouse-drag-throw' but only vertically.

To test this function, evaluate:
    (define-key global-map [down-mouse-2] \\='mookid-mouse-drag-throw)"
  (interactive "e")
  (save-selected-window
    (let* ((start-posn (event-start start-event))
           (start-window (posn-window start-posn))
           (start-row (cdr (posn-col-row start-posn)))
           (start-col (car (posn-col-row start-posn)))
           event end row scroll-delta
           have-scrolled
           col
           (scroll-col-delta 0))
      (select-window start-window)
      (track-mouse
        ;; Don't change the mouse pointer shape while we drag.
        (setq track-mouse 'dragging)
        (while (progn
                 (setq event (read-event)
                       end (event-end event)
                       row (cdr (posn-col-row end))
                       col (car (posn-col-row end)))
                 (or (mouse-movement-p event)
                     (eq (car-safe event) 'switch-frame)))
          (when (eq start-window (posn-window end))
            (setq scroll-delta (mouse-drag-scroll-delta (- start-row row))))

          (if (or (/= 0 scroll-delta)
                  (/= 0 scroll-col-delta))
              (progn
                (setq have-scrolled t)
                (mouse-drag-safe-scroll scroll-delta scroll-col-delta)
                (mouse-drag-repeatedly-safe-scroll scroll-delta scroll-col-delta))))) ;xxx
      ;; If it was a click and not a drag, prepare to pass the event on.
      ;; Is there a more correct way to reconstruct the event?
      (if (and (not have-scrolled)
               (mouse-drag-events-are-point-events-p start-posn end))
          (push (cons (event-basic-type start-event) (cdr start-event))
                unread-command-events)))))

(provide 'mookid-mouse)
;;; mookid-mouse.el ends here
