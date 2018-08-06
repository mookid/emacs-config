;;; cargo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cargo" "cargo.el" (0 0 0 0))
;;; Generated autoloads from cargo.el

(autoload 'cargo-minor-mode "cargo" "\
Cargo minor mode. Used to hold keybindings for cargo-mode.\n\n\\{cargo-minor-mode-map}\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cargo" '("cargo-minor-mode")))

;;;***

;;;### (autoloads nil "cargo-process" "cargo-process.el" (0 0 0 0))
;;; Generated autoloads from cargo-process.el

(autoload 'cargo-process-bench "cargo-process" "\
Run the Cargo bench command.\nWith the prefix argument, modify the command's invocation.\nCargo: Run the benchmarks.\n\n(fn)" t nil)

(autoload 'cargo-process-build "cargo-process" "\
Run the Cargo build command.\nWith the prefix argument, modify the command's invocation.\nCargo: Compile the current project.\n\n(fn)" t nil)

(autoload 'cargo-process-clean "cargo-process" "\
Run the Cargo clean command.\nWith the prefix argument, modify the command's invocation.\nCargo: Remove the target directory.\n\n(fn)" t nil)

(autoload 'cargo-process-doc "cargo-process" "\
Run the Cargo doc command.\nWith the prefix argument, modify the command's invocation.\nCargo: Build this project's and its dependencies' documentation.\n\n(fn)" t nil)

(autoload 'cargo-process-doc-open "cargo-process" "\
Run the Cargo doc command with the --open switch.\nWith the prefix argument, modify the command's invocation.\nCargo: Open this project's documentation.\n\n(fn)" t nil)

(autoload 'cargo-process-new "cargo-process" "\
Run the Cargo new command.\nWith the prefix argument, modify the command's invocation.\nNAME is the name of your application.\nIf BIN is t then create a binary application, otherwise a library.\nCargo: Create a new cargo project.\n\n(fn NAME &optional BIN)" t nil)

(autoload 'cargo-process-init "cargo-process" "\
Run the Cargo init command.\nWith the prefix argument, modify the command's invocation.\nDIRECTORY is the directory you want to create a cargo project in.\nIf BIN is t then create a binary application, otherwise a library.\nCargo: Create a new cargo project in current directory.\n\n(fn DIRECTORY &optional BIN)" t nil)

(autoload 'cargo-process-run "cargo-process" "\
Run the Cargo run command.\nWith the prefix argument, modify the command's invocation.\nCargo: Build and execute src/main.rs.\n\n(fn)" t nil)

(autoload 'cargo-process-run-bin "cargo-process" "\
Run the Cargo run command --bin <name>.\nWith the prefix argument, modify the command's invocation.\nCargo: Build and execute a specific binary\n\n(fn COMMAND)" t nil)

(autoload 'cargo-process-run-example "cargo-process" "\
Run the Cargo run command --example <name>.\nWith the prefix argument, modify the command's invocation.\nCargo: Build and execute with --example <name>.\n\n(fn COMMAND)" t nil)

(autoload 'cargo-process-search "cargo-process" "\
Run the Cargo search command.\nWith the prefix argument, modify the command's invocation.\nSEARCH-TERM is used as the search term for the Cargo registry.\nCargo: Search registry for crates.\n\n(fn SEARCH-TERM)" t nil)

(autoload 'cargo-process-test "cargo-process" "\
Run the Cargo test command.\nWith the prefix argument, modify the command's invocation.\nCargo: Run the tests.\n\n(fn)" t nil)

(autoload 'cargo-process-current-test "cargo-process" "\
Run the Cargo test command for the current test.\nWith the prefix argument, modify the command's invocation.\nCargo: Run the tests.\n\n(fn)" t nil)

(autoload 'cargo-process-current-file-tests "cargo-process" "\
Run the Cargo test command for the current file.\nWith the prefix argument, modify the command's invocation.\nCargo: Run the tests.\n\n(fn)" t nil)

(autoload 'cargo-process-update "cargo-process" "\
Run the Cargo update command.\nWith the prefix argument, modify the command's invocation.\nCargo: Update dependencies listed in Cargo.lock.\n\n(fn)" t nil)

(autoload 'cargo-process-fmt "cargo-process" "\
Run the Cargo fmt command.\nWith the prefix argument, modify the command's invocation.\nRequires Cargo Fmt to be installed.\n\n(fn)" t nil)

(autoload 'cargo-process-check "cargo-process" "\
Run the Cargo check command.\nWith the prefix argument, modify the command's invocation.\nCargo: Check compile the current project.\nRequires cargo-check to be installed.\n\n(fn)" t nil)

(autoload 'cargo-process-clippy "cargo-process" "\
Run the Cargo clippy command.\nWith the prefix argument, modify the command's invocation.\nCargo: Clippy compile the current project.\nRequires Cargo clippy to be installed.\n\n(fn)" t nil)

(autoload 'cargo-process-repeat "cargo-process" "\
Run the last cargo-process command.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cargo-process" '("cargo-process-" "manifest-path-argument" "set-rust-backtrace" "rustc-errno")))

;;;***

;;;### (autoloads nil nil ("cargo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cargo-autoloads.el ends here
