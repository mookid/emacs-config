;;; mookid-macros --- Useful macros for emacs initialization

;;; Commentary:
;; Macros for emacs configuration.
;; They make sure that the configuration files load, catching errors
;; induced by each module.

;;; Code:
(defvar mookid-prefix  "mookid-"
  "The prefix common to packages of my configuration.")

(defmacro with-message (msg &rest body)
  "Prints MSG before evaluating BODY, and report problems.

Warnings are still displayed, and errors are catched.
The return value reports success or failure."
  `(condition-case nil
       (progn (message "*** %s" ,msg) ,@body 'ok)
     (error (message "Error during phase called \"%s\"" ,msg) 'fail)))

(defun init-load (symb)
  "Load a package with log message.

SYMB has to be a name of package starting with the value of `mookid-prefix'.
Otherwise, nothing happens."
  (let* ((name   (symbol-name symb))
	 (prefix mookid-prefix)
	 (prefix-length (length prefix)))
    (if (and (> (length name) prefix-length)
	       (string-equal (substring name 0 prefix-length) prefix))
	(with-message (format "Loading %s" (substring name prefix-length))
		      (require symb))
      (message "** ERROR: %s does not start with %s" name mookid-prefix))))

(provide 'mookid-macros)
;;; mookid-macros.el ends here
