;;; pkg-info-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pkg-info" "pkg-info.el" (0 0 0 0))
;;; Generated autoloads from pkg-info.el

(autoload 'pkg-info-library-original-version "pkg-info" "\
Get the original version in the header of LIBRARY.\n\nThe original version is stored in the X-Original-Version header.\nThis header is added by the MELPA package archive to preserve\nupstream version numbers.\n\nLIBRARY is either a symbol denoting a named feature, or a library\nname as string.\n\nIf SHOW is non-nil, show the version in the minibuffer.\n\nReturn the version from the header of LIBRARY as list.  Signal an\nerror if the LIBRARY was not found or had no X-Original-Version\nheader.\n\nSee Info node `(elisp)Library Headers' for more information\nabout library headers.\n\n(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-library-version "pkg-info" "\
Get the version in the header of LIBRARY.\n\nLIBRARY is either a symbol denoting a named feature, or a library\nname as string.\n\nIf SHOW is non-nil, show the version in the minibuffer.\n\nReturn the version from the header of LIBRARY as list.  Signal an\nerror if the LIBRARY was not found or had no proper header.\n\nSee Info node `(elisp)Library Headers' for more information\nabout library headers.\n\n(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-original-version "pkg-info" "\
Get the original version of the library defining FUNCTION.\n\nThe original version is stored in the X-Original-Version header.\nThis header is added by the MELPA package archive to preserve\nupstream version numbers.\n\nIf SHOW is non-nil, show the version in mini-buffer.\n\nThis function is mainly intended to find the version of a major\nor minor mode, i.e.\n\n   (pkg-info-defining-library-version 'flycheck-mode)\n\nReturn the version of the library defining FUNCTION.  Signal an\nerror if FUNCTION is not a valid function, if its defining\nlibrary was not found, or if the library had no proper version\nheader.\n\n(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-version "pkg-info" "\
Get the version of the library defining FUNCTION.\n\nIf SHOW is non-nil, show the version in mini-buffer.\n\nThis function is mainly intended to find the version of a major\nor minor mode, i.e.\n\n   (pkg-info-defining-library-version 'flycheck-mode)\n\nReturn the version of the library defining FUNCTION.  Signal an\nerror if FUNCTION is not a valid function, if its defining\nlibrary was not found, or if the library had no proper version\nheader.\n\n(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-package-version "pkg-info" "\
Get the version of an installed PACKAGE.\n\nIf SHOW is non-nil, show the version in the minibuffer.\n\nReturn the version as list, or nil if PACKAGE is not installed.\n\n(fn PACKAGE &optional SHOW)" t nil)

(autoload 'pkg-info-version-info "pkg-info" "\
Obtain complete version info for LIBRARY and PACKAGE.\n\nLIBRARY is a symbol denoting a named feature, or a library name\nas string.  PACKAGE is a symbol denoting an ELPA package.  If\nomitted or nil, default to LIBRARY.\n\nIf SHOW is non-nil, show the version in the minibuffer.\n\nWhen called interactively, prompt for LIBRARY.  When called\ninteractively with prefix argument, prompt for PACKAGE as well.\n\nReturn a string with complete version information for LIBRARY.\nThis version information contains the version from the headers of\nLIBRARY, and the version of the installed PACKAGE, the LIBRARY is\npart of.  If PACKAGE is not installed, or if the PACKAGE version\nis the same as the LIBRARY version, do not include a package\nversion.\n\n(fn LIBRARY &optional PACKAGE SHOW)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pkg-info" '("pkg-info-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pkg-info-autoloads.el ends here
