;;; key-chord-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "key-chord" "key-chord.el" (0 0 0 0))
;;; Generated autoloads from key-chord.el

(autoload 'key-chord-mode "key-chord" "\
Toggle key chord mode.\nWith positive ARG enable the mode. With zero or negative arg disable the mode.\nA key chord is two keys that are pressed simultaneously, or one key quickly\npressed twice.\n\nSee functions `key-chord-define-global', `key-chord-define-local', and\n`key-chord-define' and variables `key-chord-two-keys-delay' and\n`key-chord-one-key-delay'.\n\n(fn ARG)" t nil)

(autoload 'key-chord-define-global "key-chord" "\
Define a key-chord of the two keys in KEYS starting a COMMAND.\n\nKEYS can be a string or a vector of two elements. Currently only elements\nthat corresponds to ascii codes in the range 32 to 126 can be used.\n\nCOMMAND can be an interactive function, a string, or nil.\nIf COMMAND is nil, the key-chord is removed.\n\nNote that KEYS defined locally in the current buffer will have precedence.\n\n(fn KEYS COMMAND)" t nil)

(autoload 'key-chord-define-local "key-chord" "\
Locally define a key-chord of the two keys in KEYS starting a COMMAND.\n\nKEYS can be a string or a vector of two elements. Currently only elements\nthat corresponds to ascii codes in the range 32 to 126 can be used.\n\nCOMMAND can be an interactive function, a string, or nil.\nIf COMMAND is nil, the key-chord is removed.\n\nThe binding goes in the current buffer's local map,\nwhich in most cases is shared with all other buffers in the same major mode.\n\n(fn KEYS COMMAND)" t nil)

(autoload 'key-chord-define "key-chord" "\
Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.\n\nKEYS can be a string or a vector of two elements. Currently only elements\nthat corresponds to ascii codes in the range 32 to 126 can be used.\n\nCOMMAND can be an interactive function, a string, or nil.\nIf COMMAND is nil, the key-chord is removed.\n\n(fn KEYMAP KEYS COMMAND)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "key-chord" '("key-chord-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; key-chord-autoloads.el ends here
