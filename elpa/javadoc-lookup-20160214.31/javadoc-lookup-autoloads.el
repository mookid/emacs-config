;;; javadoc-lookup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "javadoc-import" "javadoc-import.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from javadoc-import.el

(autoload 'javadoc-sort-imports "javadoc-import" "\
Sort the imports in the import section in proper order.\n\n(fn)" t nil)

(define-obsolete-function-alias 'sort-java-imports 'javadoc-sort-imports "1.1.0")

(autoload 'javadoc-add-import "javadoc-import" "\
Insert an import statement at import section at the top of the file.\n\n(fn)" t nil)

(define-obsolete-function-alias 'add-java-import 'javadoc-add-import "1.1.0")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "javadoc-import" '("jdl/")))

;;;***

;;;### (autoloads nil "javadoc-lookup" "javadoc-lookup.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from javadoc-lookup.el

(autoload 'javadoc-lookup "javadoc-lookup" "\
Lookup based on class name.\n\n(fn NAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "javadoc-lookup" '("javadoc-" "jdl/")))

;;;***

;;;### (autoloads nil "maven-fetch" "maven-fetch.el" (0 0 0 0))
;;; Generated autoloads from maven-fetch.el

(autoload 'javadoc-add-artifacts "maven-fetch" "\
Add Maven repository artifacts to the javadoc-lookup index.\nAn artifact is specified by a sequence of three strings:\n [groupId artifactId version].\n\n(fn &rest ARTIFACTS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "maven-fetch" '("maven-" "unzip-program-name")))

;;;***

;;;### (autoloads nil nil ("javadoc-lookup-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; javadoc-lookup-autoloads.el ends here
