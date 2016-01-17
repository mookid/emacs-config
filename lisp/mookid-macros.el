;;; mookid-macros --- Useful macros for emacs initialization

;;; Commentary:
;; Macros for Emacs configuration.
;; They make sure that the configuration files load, catching errors
;; induced by each module.

;;; Code:
(require 'mookid-values) ; mookid-prefix

(defmacro with-title (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "[%s]" ,msg) ,@body (message "[end]") 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defmacro with-message (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "*** %s" ,msg) ,@body 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defun init-load (symb)
  "Load a package with log message.

SYMB has to be a suffix of a package starting with `mookid-prefix'."
  (let* ((name (symbol-name symb))
	 (symb (intern (concat mookid-prefix name))))
    (with-message (format "Loading %s" name)
		  (require symb))))

(provide 'mookid-macros)
;;; mookid-macros.el ends here
