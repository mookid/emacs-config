;;; javadoc-lookup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "javadoc-import" "javadoc-import.el" (23123
;;;;;;  39025 0 0))
;;; Generated autoloads from javadoc-import.el

(autoload 'javadoc-sort-imports "javadoc-import" "\
Sort the imports in the import section in proper order.

\(fn)" t nil)

(define-obsolete-function-alias 'sort-java-imports 'javadoc-sort-imports "1.1.0")

(autoload 'javadoc-add-import "javadoc-import" "\
Insert an import statement at import section at the top of the file.

\(fn)" t nil)

(define-obsolete-function-alias 'add-java-import 'javadoc-add-import "1.1.0")

;;;***

;;;### (autoloads nil "javadoc-lookup" "javadoc-lookup.el" (23123
;;;;;;  39025 0 0))
;;; Generated autoloads from javadoc-lookup.el

(autoload 'javadoc-lookup "javadoc-lookup" "\
Lookup based on class name.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "maven-fetch" "maven-fetch.el" (23123 39025
;;;;;;  0 0))
;;; Generated autoloads from maven-fetch.el

(autoload 'javadoc-add-artifacts "maven-fetch" "\
Add Maven repository artifacts to the javadoc-lookup index.
An artifact is specified by a sequence of three strings:
 [groupId artifactId version].

\(fn &rest ARTIFACTS)" nil nil)

;;;***

;;;### (autoloads nil nil ("javadoc-lookup-pkg.el") (23123 39025
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; javadoc-lookup-autoloads.el ends here
