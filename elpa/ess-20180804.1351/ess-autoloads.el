;;; ess-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ess" "ess.el" (0 0 0 0))
;;; Generated autoloads from ess.el

(autoload 'ess-mode "ess" "\
Major mode for editing ESS source.\nOptional arg ALIST describes how to customize the editing mode.\nOptional arg PROC-NAME is name of associated inferior process.\n\n\\{ess-mode-map}\n\nExtra binding to note:  'ESC C-\\' indent-region.\n\nEntry to this mode runs the hooks in ess-mode-hook.\n\nYou can send text to the inferior ESS process from other buffers containing\nESS source.\n    `ess-eval-region' sends the current region to the ESS process.\n    `ess-eval-buffer' sends the current buffer to the ESS process.\n    `ess-eval-function' sends the current function to the ESS process.\n    `ess-eval-line' sends the current line to the ESS process.\n    `ess-beginning-of-function' and `ess-end-of-function' move the point to\n        the beginning and end of the current ESS function.\n    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.\n    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process\n        buffer and puts point at the end of it.\n\n    `ess-eval-region-and-go', `ess-eval-buffer-and-go',\n        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S\n        process buffer after sending their text.\n\n    `ess-load-file' sources a file of commands to the ESS process.\n\n\\[ess-indent-command] indents for ESS code.\n\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.\nComments are indented in a similar way to Emacs-lisp mode:\n       `###'     beginning of line\n       `##'      the same level of indentation as the code\n       `#'       the same column on the right, or to the right of such a\n                 column if that is not possible.(default value 40).\n                 \\[indent-for-comment] command automatically inserts such a\n                 `#' in the right place, or aligns such a comment if it is\n                 already inserted.\n\\[ess-indent-exp] command indents each line of the syntactic unit following point.\n\nVariables controlling indentation style:\n `ess-tab-always-indent'\n    Non-nil means TAB in ESS mode should always reindent the current line,\n    regardless of where in the line point is when the TAB command is used.\n `ess-auto-newline'\n    Non-nil means automatically newline before and after braces inserted in S\n    code.\n `ess-indent-offset'\n    Indentation of ESS statements within surrounding block.\n    The surrounding block's indentation is the indentation of the line on\n    which the open-brace appears.\n `ess-offset-block'\n    Indentation of blocks opened with curly braces or anonymous parentheses.\n `ess-offset-arguments'\n    Indentation of function arguments or bracket indices.\n `ess-offset-arguments-newline'\n    Indentation of function arguments or bracket indices when the opening\n    delimiter is immediately followed by a newline.\n `ess-offset-continued'\n    Indentation style for continued statements.\n `ess-align-nested-calls'\n    Functions whose nested calls should be aligned.\n `ess-align-arguments-in-calls'\n    Calls in which arguments should be aligned.\n `ess-align-continuations-in-calls'\n    Whether ignore indentation after an operator in calls\n `ess-align-blocks'\n    Blocks that should always be aligned vertically.\n `ess-indent-from-lhs'\n    Whether function calls given as argument should be indented from the\n    parameter name.\n `ess-indent-from-chain-start'\n    Whether to indent arguments from the first of several consecutive calls.\n `ess-indent-with-fancy-comments'\n    Non-nil means distinguish between #, ##, and ### for indentation.\n\nFurthermore, \\[ess-set-style] command enables you to set up predefined ess-mode\nindentation style. At present, predefined style are `BSD', `GNU', `K&R', `C++',\n`CLB' (quoted from C language style).\n\n(fn &optional ALIST PROC-NAME IS-DERIVED)" nil nil)

(autoload 'ess-dump-object-into-edit-buffer "ess" "\
Edit an ESS OBJECT in its own buffer.\nWithout a prefix argument, this simply finds the file pointed to by\n`ess-source-directory'.  If this file does not exist, or if a\nprefix argument is given, a dump() command is sent to the ESS process to\ngenerate the source buffer.\n\n(fn OBJECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess" '("ess-")))

;;;***

;;;### (autoloads nil "ess-arc-d" "ess-arc-d.el" (0 0 0 0))
;;; Generated autoloads from ess-arc-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-arc-d" '("ARC")))

;;;***

;;;### (autoloads nil "ess-bugs-d" "ess-bugs-d.el" (0 0 0 0))
;;; Generated autoloads from ess-bugs-d.el

(autoload 'ess-bugs-mode "ess-bugs-d" "\
ESS[BUGS]: Major mode for BUGS.\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode))

(add-to-list 'auto-mode-alist '("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode))

(add-to-list 'auto-mode-alist '("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-bugs-d" '("ess-")))

;;;***

;;;### (autoloads nil "ess-bugs-l" "ess-bugs-l.el" (0 0 0 0))
;;; Generated autoloads from ess-bugs-l.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-bugs-l" '("ess-bugs-")))

;;;***

;;;### (autoloads nil "ess-custom" "ess-custom.el" (0 0 0 0))
;;; Generated autoloads from ess-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-custom" '("ess-" "inferior-" "julia-basic-offset" "comint-highlight-prompt" "SAS-mode-hook" "S+" "S-" "Rnw-mode-hook" "R-" "no-doc")))

;;;***

;;;### (autoloads nil "ess-dde" "ess-dde.el" (0 0 0 0))
;;; Generated autoloads from ess-dde.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-dde" '("ess-")))

;;;***

;;;### (autoloads nil "ess-font-lock" "ess-font-lock.el" (0 0 0 0))
;;; Generated autoloads from ess-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-font-lock" '("ess-font-lock-")))

;;;***

;;;### (autoloads nil "ess-generics" "ess-generics.el" (0 0 0 0))
;;; Generated autoloads from ess-generics.el

(autoload 'ess-defgeneric "ess-generics" "\
Define a new function, as with `defun', which can be overloaded.\nNAME is the name of the function to create. ARGS are the\narguments to the function. DOCSTRING is a documentation string to\ndescribe the function.  The docstring will automatically have\ndetails about its overload symbol appended to the end. BODY is\ncode that would be run when there is no override defined.  The\ndefault is to signal error if {name}-function is not defined.\n\n(fn NAME ARGS DOCSTRING &rest BODY)" nil t)

(function-put 'ess-defgeneric 'doc-string-elt '3)

(function-put 'ess-defgeneric 'lisp-indent-function 'defun)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-generics" '("ess-")))

;;;***

;;;### (autoloads nil "ess-gretl" "ess-gretl.el" (0 0 0 0))
;;; Generated autoloads from ess-gretl.el

(autoload 'gretl-mode "ess-gretl" "\
Major mode for editing gretl source.  See `ess-mode' for more help.\n\n(fn &optional PROC-NAME)" t nil)

(autoload 'gretl "ess-gretl" "\
Call 'gretl',\nOptional prefix (C-u) allows to set command line arguments, such as\n--vsize.  This should be OS agnostic.\nIf you have certain command line arguments that should always be passed\nto gretl, put them in the variable `inferior-gretl-args'.\n\n(fn &optional START-ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-gretl" '("ess-gretl-post-run-hook" "inferior-gretl-args" "gretl-")))

;;;***

;;;### (autoloads nil "ess-help" "ess-help.el" (0 0 0 0))
;;; Generated autoloads from ess-help.el

(autoload 'ess-display-help-on-object "ess-help" "\
Display documentation for OBJECT in another window.\nIf prefix arg is given, force an update of the cached help topics\nand query the ESS process for the help file instead of reusing an\nexisting buffer if it exists.  Uses the variable\n`inferior-ess-help-command' for the actual help command.  Prompts\nfor the object name based on the cursor location for all cases\nexcept the S-Plus GUI.  With S-Plus on Windows (both GUI and in\nan inferior Emacs buffer) the GUI help window is used.\n\nIf COMMAND is suplied, it is used instead of `inferior-ess-help-command'.\n\n(fn OBJECT &optional COMMAND)" t nil)

(defalias 'ess-help 'ess-display-help-on-object)

(autoload 'ess-helpobjs-at-point "ess-help" "\
\n\n(fn SLIST)" nil nil)

(autoload 'ess-goto-info "ess-help" "\
Display node NODE from `ess-mode' info.\n\n(fn NODE)" nil nil)

(autoload 'ess-submit-bug-report "ess-help" "\
Submit a bug report on the ess-mode package.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-help" '("ess-")))

;;;***

;;;### (autoloads nil "ess-inf" "ess-inf.el" (0 0 0 0))
;;; Generated autoloads from ess-inf.el

(autoload 'ess-proc-name "ess-inf" "\
Return name of process N, as a string, with NAME prepended.\nIf `ess-plain-first-buffername', then initial process is number-free.\n\n(fn N NAME)" nil nil)

(autoload 'inferior-ess "ess-inf" "\
Start inferior ESS process.\n\nWithout a prefix argument, starts a new ESS process, or switches\nto the ESS process associated with the current buffer.  With\nESS-START-ARGS (perhaps specified via \\[universal-argument]),\nstarts the process with those args.  The current buffer is used\nif it is an `inferior-ess-mode' or `ess-transcript-mode' buffer.\n\nIf `ess-ask-about-transfile' is non-nil, you will be asked for a\ntranscript file to use.  If there is no transcript file, the\nbuffer name will be like *R* or *R2*, determined by\n`ess-gen-proc-buffer-name-function'.\n\nTakes the program name from the variable `inferior-ess-program'.\nAn initialization file (dumped into the process) is specified by\n`inferior-ess-start-file', and `inferior-ess-start-args' is used\nto accompany the call for `inferior-ess-program'.\n\nWhen creating a new process, the process buffer replaces the\ncurrent window if `inferior-ess-same-window' is non-nil.\nAlternatively, it can appear in its own frame if\n`inferior-ess-own-frame' is non-nil.\n\n(Type \\[describe-mode] in the process buffer for a list of\ncommands.)\n\nCUSTOMIZE-ALIST is the list of dialect-specific variables.  When\nnon-nil, NO-WAIT tells ESS not to wait for the process to finish.\nThis may be useful for debugging.\n\n(fn &optional ESS-START-ARGS CUSTOMIZE-ALIST NO-WAIT)" t nil)

(ess-defgeneric ess-load-file (&optional filename) "\
Load a source file into an inferior ESS process.\n\nThis handles Tramp when working on a remote." (interactive (list (or (and (memq major-mode '(ess-mode ess-julia-mode)) (buffer-file-name)) (expand-file-name (read-file-name "Load source file: " nil nil t))))) (ess-load-file--normalise-buffer filename) (save-selected-window (ess-switch-to-ESS t)) (:override (let ((file (ess-load-file--normalise-file filename))) (let ((command (ess-build-load-command file nil t))) (ess-send-string (ess-get-process) command t)))))

(autoload 'inferior-ess-mode "ess-inf" "\
Major mode for interacting with an inferior ESS process.\nRuns an S interactive job as a subprocess of Emacs, with I/O through an\nEmacs buffer.  Variable `inferior-ess-program' controls which S\nis run.\n\nCommands are sent to the ESS process by typing them, and pressing\n\\[inferior-ess-send-input].  Pressing \\[complete-dynamic-complete]\ncompletes known object names or filenames, as appropriate.  Other\nkeybindings for this mode are:\n\n\\{inferior-ess-mode-map}\n\nWhen editing S objects, the use of \\[ess-load-file] is advocated.\n`ess-load-file' keeps source files (if `ess-keep-dump-files' is non-nil) in\nthe directory specified by `ess-source-directory', with the\nfilename chosen according to `ess-dump-filename-template'. When a file is\nloaded, `ess-mode' parses error messages and jumps to the appropriate file\nif errors occur. The ess-eval- commands do not do this.\n\nCustomization: Entry to this mode runs the hooks on `comint-mode-hook' and\n`inferior-ess-mode-hook' (in that order).\n\nYou can send text to the inferior ESS process from other buffers containing\nS source. The key bindings of these commands can be found by typing\nC-h m (help for mode) in the other buffers.\n    `ess-eval-region' sends the current region to the ESS process.\n    `ess-eval-buffer' sends the current buffer to the ESS process.\n    `ess-eval-function' sends the current function to the ESS process.\n    `ess-eval-line' sends the current line to the ESS process.\n    `ess-beginning-of-function' and `ess-end-of-function' move the point to\n        the beginning and end of the current S function.\n    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.\n    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process\n        buffer and puts point at the end of it.\n\n    `ess-eval-region-and-go', `ess-eval-buffer-and-go',\n        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S\n        process buffer after sending their text.\n    `ess-dump-object-into-edit-buffer' moves an S object into a temporary file\n        and buffer for editing\n    `ess-load-file' sources a file of commands to the ESS process.\n\nCommands:\nReturn after the end of the process' output sends the text from the\n    end of process to point.\nReturn before the end of the process' output copies the sexp ending at point\n    to the end of the process' output, and sends it.\nDelete converts tabs to spaces as it moves back.\nC-M-q does Tab on each line starting within following expression.\nParagraphs are separated only by blank lines.  Crosshatches start comments.\nIf you accidentally suspend your process, use \\[comint-continue-subjob]\nto continue it.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-inf" '("ess-" "inferior-ess-" "update-ess-process-name-list" "with-ess-process-buffer")))

;;;***

;;;### (autoloads nil "ess-jags-d" "ess-jags-d.el" (0 0 0 0))
;;; Generated autoloads from ess-jags-d.el

(autoload 'ess-jags-mode "ess-jags-d" "\
ESS[JAGS]: Major mode for JAGS.\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode))

(add-to-list 'auto-mode-alist '("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode))

(add-to-list 'auto-mode-alist '("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-jags-d" '("ess-jags-")))

;;;***

;;;### (autoloads nil "ess-julia" "ess-julia.el" (0 0 0 0))
;;; Generated autoloads from ess-julia.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-julia" '("inferior-julia-args")))

;;;***

;;;### (autoloads nil "ess-lsp-l" "ess-lsp-l.el" (0 0 0 0))
;;; Generated autoloads from ess-lsp-l.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-lsp-l" '("Lisp-editing-alist")))

;;;***

;;;### (autoloads nil "ess-mouse" "ess-mouse.el" (0 0 0 0))
;;; Generated autoloads from ess-mouse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-mouse" '("ess-")))

;;;***

;;;### (autoloads nil "ess-noweb" "ess-noweb.el" (0 0 0 0))
;;; Generated autoloads from ess-noweb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-noweb" '("ess-")))

;;;***

;;;### (autoloads nil "ess-noweb-font-lock-mode" "ess-noweb-font-lock-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ess-noweb-font-lock-mode.el

(autoload 'ess-noweb-font-lock-mode "ess-noweb-font-lock-mode" "\
Minor mode for syntax highlighting when using `ess-noweb-mode' to edit noweb files.\nEach chunk is fontified in accordance with its own mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-noweb-font-lock-mode" '("ess-noweb-" "nwfl-donowt")))

;;;***

;;;### (autoloads nil "ess-noweb-mode" "ess-noweb-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ess-noweb-mode.el

(autoload 'ess-noweb-mode "ess-noweb-mode" "\
Minor meta mode for editing noweb files.\n`Meta' refers to the fact that this minor mode is switching major\nmodes depending on the location of point.\n\nThe following special keystrokes are available in noweb mode:\n\nMovement:\n\\[ess-noweb-next-chunk] 	goto the next chunk\n\\[ess-noweb-previous-chunk] 	goto the previous chunk\n\\[ess-noweb-goto-previous] 	goto the previous chunk of the same name\n\\[ess-noweb-goto-next] 	goto the next chunk of the same name\n\\[ess-noweb-goto-chunk] 		goto a chunk\n\\[ess-noweb-next-code-chunk] 		goto the next code chunk\n\\[ess-noweb-previous-code-chunk] 		goto the previous code chunk\n\\[ess-noweb-next-doc-chunk] 		goto the next documentation chunk\n\\[ess-noweb-previous-doc-chunk] 		goto the previous documentation chunk\n\nCopying/Killing/Marking/Narrowing:\n\\[ess-noweb-copy-chunk-as-kill] 		copy the chunk the point is in into the kill ring\n\\[ess-noweb-copy-chunk-pair-as-kill] 		copy the pair of doc/code chunks the point is in\n\\[ess-noweb-kill-chunk] 		kill the chunk the point is in\n\\[ess-noweb-kill-chunk-pair] 		kill the pair of doc/code chunks the point is in\n\\[ess-noweb-mark-chunk] 		mark the chunk the point is in\n\\[ess-noweb-mark-chunk-pair] 		mark the pair of doc/code chunks the point is in\n\\[ess-noweb-narrow-to-chunk] 		narrow to the chunk the point is in\n\\[ess-noweb-narrow-to-chunk-pair] 		narrow to the pair of doc/code chunks the point is in\n\\[widen] 	widen\n\\[ess-noweb-toggle-narrowing] 		toggle auto narrowing\n\nFilling and Indenting:\n\\[ess-noweb-fill-chunk] 	fill (or indent) the chunk at point according to mode\n\\[ess-noweb-fill-paragraph-chunk] 	fill the paragraph at point, restricted to chunk\n\\[ess-noweb-indent-line] 	indent the line at point according to mode\n\nInsertion:\n\\[ess-noweb-insert-default-mode-line] 	insert a line to set this file's code mode\n\\[ess-noweb-new-chunk] 		insert a new chunk at point\n\\[ess-noweb-complete-chunk] 	complete the chunk name before point\n\\[ess-noweb-electric-@] 		insert a `@' or start a new doc chunk\n\\[ess-noweb-electric-<] 		insert a `<' or start a new code chunk\n\nModes:\n\\[ess-noweb-set-doc-mode] 		set the major mode for editing doc chunks\n\\[ess-noweb-set-code-mode] 	set the major mode for editing code chunks\n\\[ess-noweb-set-this-code-mode] 	set the major mode for editing this code chunk\n\nMisc:\n\\[ess-noweb-occur] 		find all occurrences of the current chunk\n\\[ess-noweb-update-chunk-vector] 	update the markers for chunks\n\\[ess-noweb-describe-mode] 	describe ess-noweb-mode\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-noweb-mode" '("ess-noweb-")))

;;;***

;;;### (autoloads nil "ess-omg-d" "ess-omg-d.el" (0 0 0 0))
;;; Generated autoloads from ess-omg-d.el

(autoload 'OMG-mode "ess-omg-d" "\
Major mode for editing Omegahat source.  NOT EVEN STARTED.\n\n(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.omg\\'" . omegahat-mode))

(add-to-list 'auto-mode-alist '("\\.hat\\'" . omegahat-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-omg-d" '("OMG")))

;;;***

;;;### (autoloads nil "ess-omg-l" "ess-omg-l.el" (0 0 0 0))
;;; Generated autoloads from ess-omg-l.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-omg-l" '("ess-" "OMG-")))

;;;***

;;;### (autoloads nil "ess-r-a" "ess-r-a.el" (0 0 0 0))
;;; Generated autoloads from ess-r-a.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-a" '("pd::set-up-demo" "ess")))

;;;***

;;;### (autoloads nil "ess-r-completion" "ess-r-completion.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ess-r-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-completion" '("ess-" "ac-source-R" "company-R-")))

;;;***

;;;### (autoloads nil "ess-r-flymake" "ess-r-flymake.el" (0 0 0 0))
;;; Generated autoloads from ess-r-flymake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-flymake" '("ess-r-")))

;;;***

;;;### (autoloads nil "ess-r-gui" "ess-r-gui.el" (0 0 0 0))
;;; Generated autoloads from ess-r-gui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-gui" '("Rgui" "ess-" "inferior-")))

;;;***

;;;### (autoloads nil "ess-r-mode" "ess-r-mode.el" (0 0 0 0))
;;; Generated autoloads from ess-r-mode.el

(defvar ess-dev-map (let (ess-dev-map) (define-prefix-command 'ess-dev-map) (define-key ess-dev-map "\23" 'ess-r-set-evaluation-env) (define-key ess-dev-map "s" 'ess-r-set-evaluation-env) (define-key ess-dev-map "T" 'ess-toggle-tracebug) (define-key ess-dev-map "\f" 'ess-r-devtools-load-package) (define-key ess-dev-map "l" 'ess-r-devtools-load-package) (define-key ess-dev-map "`" 'ess-show-traceback) (define-key ess-dev-map "~" 'ess-show-call-stack) (define-key ess-dev-map "\27" 'ess-watch) (define-key ess-dev-map "w" 'ess-watch) (define-key ess-dev-map "\4" 'ess-debug-flag-for-debugging) (define-key ess-dev-map "d" 'ess-debug-flag-for-debugging) (define-key ess-dev-map "\25" 'ess-debug-unflag-for-debugging) (define-key ess-dev-map "u" 'ess-debug-unflag-for-debugging) (define-key ess-dev-map [(control 68)] 'ess-debug-unflag-for-debugging) (define-key ess-dev-map "\2" 'ess-bp-set) (define-key ess-dev-map "b" 'ess-bp-set) (define-key ess-dev-map [(control 66)] 'ess-bp-set-conditional) (define-key ess-dev-map "B" 'ess-bp-set-conditional) (define-key ess-dev-map "\f" 'ess-bp-set-logger) (define-key ess-dev-map "L" 'ess-bp-set-logger) (define-key ess-dev-map "\17" 'ess-bp-toggle-state) (define-key ess-dev-map "o" 'ess-bp-toggle-state) (define-key ess-dev-map "\13" 'ess-bp-kill) (define-key ess-dev-map "k" 'ess-bp-kill) (define-key ess-dev-map "\13" 'ess-bp-kill-all) (define-key ess-dev-map "K" 'ess-bp-kill-all) (define-key ess-dev-map "\16" 'ess-bp-next) (define-key ess-dev-map "n" 'ess-bp-next) (define-key ess-dev-map "i" 'ess-debug-goto-input-event-marker) (define-key ess-dev-map "I" 'ess-debug-goto-input-event-marker) (define-key ess-dev-map "\20" 'ess-bp-previous) (define-key ess-dev-map "p" 'ess-bp-previous) (define-key ess-dev-map "\5" 'ess-debug-toggle-error-action) (define-key ess-dev-map "e" 'ess-debug-toggle-error-action) (define-key ess-dev-map "0" 'ess-electric-selection) (define-key ess-dev-map "1" 'ess-electric-selection) (define-key ess-dev-map "2" 'ess-electric-selection) (define-key ess-dev-map "3" 'ess-electric-selection) (define-key ess-dev-map "4" 'ess-electric-selection) (define-key ess-dev-map "5" 'ess-electric-selection) (define-key ess-dev-map "6" 'ess-electric-selection) (define-key ess-dev-map "7" 'ess-electric-selection) (define-key ess-dev-map "8" 'ess-electric-selection) (define-key ess-dev-map "9" 'ess-electric-selection) (define-key ess-dev-map "?" 'ess-tracebug-show-help) ess-dev-map) "\
Keymap for commands related to development and debugging.")

(autoload 'run-ess-r "ess-r-mode" "\
Call 'R', the 'GNU S' system from the R Foundation.\nOptional prefix (\\[universal-argument]) allows to set command line arguments, such as\n--vsize.  This should be OS agnostic.\nIf you have certain command line arguments that should always be passed\nto R, put them in the variable `inferior-R-args'.\n\nSTART-ARGS can be a string representing an argument, a list of\nsuch strings, or any other non-nil value.  In the latter case, you\nwill be prompted to enter arguments interactively.\n\n(fn &optional START-ARGS)" t nil)

(defalias 'R #'run-ess-r)

(autoload 'R-mode "ess-r-mode" "\
Major mode for editing R source.  See `ess-mode' for more help.\n\n(fn &optional PROC-NAME)" t nil)

(defalias 'r-mode #'R-mode)

(defalias 'ess-r-mode #'R-mode)

(add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . R-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]profile\\'" . R-mode))

(add-to-list 'auto-mode-alist '("NAMESPACE\\'" . R-mode))

(add-to-list 'auto-mode-alist '("CITATION\\'" . R-mode))

(autoload 'Rnw-mode "ess-r-mode" "\
Major mode for editing Sweave(R) source.\nSee `ess-noweb-mode' and `R-mode' for more help.\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . Rnw-mode))

(add-to-list 'auto-mode-alist '("\\.[sS]nw\\'" . Snw-mode))

(autoload 'R-transcript-mode "ess-r-mode" "\
Does the right thing.\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Rr]out" . R-transcript-mode))

(add-to-list 'interpreter-mode-alist '("Rscript" . R-mode))

(add-to-list 'interpreter-mode-alist '("r" . R-mode))

(add-to-list 'auto-mode-alist '("/Makevars\\(\\.win\\)?$" . makefile-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-mode" '("ess-" "inferior-ess-r-" "R-")))

;;;***

;;;### (autoloads nil "ess-r-package" "ess-r-package.el" (0 0 0 0))
;;; Generated autoloads from ess-r-package.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-package" '("ess-")))

;;;***

;;;### (autoloads nil "ess-r-syntax" "ess-r-syntax.el" (0 0 0 0))
;;; Generated autoloads from ess-r-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-syntax" '("ess-" "backward-ess-r-" "forward-ess-r-")))

;;;***

;;;### (autoloads nil "ess-r-xref" "ess-r-xref.el" (0 0 0 0))
;;; Generated autoloads from ess-r-xref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-r-xref" '("ess-r-xref-")))

;;;***

;;;### (autoloads nil "ess-rd" "ess-rd.el" (0 0 0 0))
;;; Generated autoloads from ess-rd.el

(autoload 'Rd-mode "ess-rd" "\
Major mode for editing R documentation source files.\n\nThis mode makes it easier to write R documentation by helping with\nindentation, doing some of the typing for you (with Abbrev mode) and by\nshowing keywords, strings, etc. in different faces (with Font Lock mode\non terminals that support it).\n\nType \\[list-abbrevs] to display the built-in abbrevs for Rd keywords.\n\nKeybindings\n===========\n\n\\{Rd-mode-map}\n\nVariables you can use to customize Rd mode\n==========================================\n\n`Rd-indent-level'\n  Indentation of Rd code with respect to containing blocks.\n  Default is 2.\n\nTurning on Rd mode runs the hook `Rd-mode-hook'.\n\nTo automatically turn on the abbrev(iate) features, add the\nfollowing lines to your `.emacs' file:\n\n  (add-hook 'Rd-mode-hook\n            (lambda ()\n              (abbrev-mode 1)))\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.Rd\\'" . Rd-mode))

(autoload 'Rd-preview-help "ess-rd" "\
Preview the current Rd buffer contents as help.\nIf optional VIA-SHELL is set, using `Rd-to-help-command'.\nIf the current buffer is not associated with a file, create a\ntemporary one in `temporary-file-directory'.\n\n(fn &optional VIA-SHELL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-rd" '("Rd-" "ess")))

;;;***

;;;### (autoloads nil "ess-rdired" "ess-rdired.el" (0 0 0 0))
;;; Generated autoloads from ess-rdired.el

(autoload 'ess-rdired "ess-rdired" "\
Run dired-like mode on R objects.\nThis is the main function.  See documentation for `ess-rdired-mode' though\nfor more information!\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-rdired" '("ess-rdired-")))

;;;***

;;;### (autoloads nil "ess-roxy" "ess-roxy.el" (0 0 0 0))
;;; Generated autoloads from ess-roxy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-roxy" '("ess-")))

;;;***

;;;### (autoloads nil "ess-rutils" "ess-rutils.el" (0 0 0 0))
;;; Generated autoloads from ess-rutils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-rutils" '("ess-rutils-")))

;;;***

;;;### (autoloads nil "ess-s-lang" "ess-s-lang.el" (0 0 0 0))
;;; Generated autoloads from ess-s-lang.el

(add-to-list 'auto-mode-alist '("\\.[Ss]t\\'" . S-transcript-mode))

(add-to-list 'auto-mode-alist '("\\.Sout" . S-transcript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-s-lang" '("ess-" "S+common-cust-alist" "S-" "inferior-S-language-start")))

;;;***

;;;### (autoloads nil "ess-s3-d" "ess-s3-d.el" (0 0 0 0))
;;; Generated autoloads from ess-s3-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-s3-d" 'nil))

;;;***

;;;### (autoloads nil "ess-s4-d" "ess-s4-d.el" (0 0 0 0))
;;; Generated autoloads from ess-s4-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-s4-d" 'nil))

;;;***

;;;### (autoloads nil "ess-sas-a" "ess-sas-a.el" (0 0 0 0))
;;; Generated autoloads from ess-sas-a.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sas-a" '("ess-" "sas-program")))

;;;***

;;;### (autoloads nil "ess-sas-d" "ess-sas-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sas-d.el

(autoload 'SAS-mode "ess-sas-d" "\
Major mode for editing SAS source.  See ess-mode for more help.\n\n(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . SAS-mode))

(autoload 'SAS-menu "ess-sas-d" "\
Start SAS from the menu.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sas-d" '("ess-" "SAS" "inferior-SAS-args")))

;;;***

;;;### (autoloads nil "ess-sas-l" "ess-sas-l.el" (0 0 0 0))
;;; Generated autoloads from ess-sas-l.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sas-l" '("ess-" "set-sas-file-" "submit-sas" "switch-to-" "sas-" "beginning-of-sas-" "backward-page-top-of-window" "fix-page-breaks" "forward-page-top-of-window" "next-sas-proc" "indent-sas-statement" "SAS-")))

;;;***

;;;### (autoloads nil "ess-site" "ess-site.el" (0 0 0 0))
;;; Generated autoloads from ess-site.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-site" '("ess-")))

;;;***

;;;### (autoloads nil "ess-sp3-d" "ess-sp3-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp3-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sp3-d" '("S+3")))

;;;***

;;;### (autoloads nil "ess-sp4-d" "ess-sp4-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp4-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sp4-d" '("Sqpe+4" "S+4" "inferior-S+4-multipleinstances")))

;;;***

;;;### (autoloads nil "ess-sp5-d" "ess-sp5-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp5-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sp5-d" '("S+5")))

;;;***

;;;### (autoloads nil "ess-sp6-d" "ess-sp6-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp6-d.el

(autoload 'S+-mode "ess-sp6-d" "\
Major mode for editing S+ source.  See `ess-mode' for more help.\n\n(fn &optional PROC-NAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sp6-d" '("ess-" "S+")))

;;;***

;;;### (autoloads nil "ess-sp6w-d" "ess-sp6w-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp6w-d.el

(add-to-list 'auto-mode-alist '("\\.sp\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.[qsS]\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.ssc\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.SSC\\'" . S-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-sp6w-d" '("S+" "Sqpe+" "ess-sqpe-versions-create" "inferior-S+")))

;;;***

;;;### (autoloads nil "ess-stata-lang" "ess-stata-lang.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ess-stata-lang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-stata-lang" '("ess-" "stata-" "STA-" "ado-set-font-lock-keywords")))

;;;***

;;;### (autoloads nil "ess-stata-mode" "ess-stata-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ess-stata-mode.el

(autoload 'STA-mode "ess-stata-mode" "\
Major mode for editing Stata source.  See `ess-mode' for more help.\n\n(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.do\\'" . STA-mode))

(add-to-list 'auto-mode-alist '("\\.ado\\'" . STA-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-stata-mode" '("stata" "ess-" "STA-")))

;;;***

;;;### (autoloads nil "ess-swv" "ess-swv.el" (0 0 0 0))
;;; Generated autoloads from ess-swv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-swv" '("ess-")))

;;;***

;;;### (autoloads nil "ess-toolbar" "ess-toolbar.el" (0 0 0 0))
;;; Generated autoloads from ess-toolbar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-toolbar" '("ess-")))

;;;***

;;;### (autoloads nil "ess-tracebug" "ess-tracebug.el" (0 0 0 0))
;;; Generated autoloads from ess-tracebug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-tracebug" '("ess-" "org-" "inferior-ess-")))

;;;***

;;;### (autoloads nil "ess-trns" "ess-trns.el" (0 0 0 0))
;;; Generated autoloads from ess-trns.el

(autoload 'ess-transcript-mode "ess-trns" "\
Major mode for manipulating {ESS} transcript files.\n\nType \\[ess-transcript-send-command] to send a command in the\ntranscript to the current S process. \\[ess-transcript-copy-command]\ncopies the command but does not execute it, allowing you to edit it in\nthe process buffer first.\n\nType \\[ess-transcript-clean-region] to delete all outputs and prompts\nin the region, leaving only the S commands.  Other keybindings are:\n\n\\{ess-transcript-mode-map}\n\n(fn ALIST &optional PROC)" nil nil)

(autoload 'ess-transcript-clean-region "ess-trns" "\
Strip the transcript in the region, leaving only (R/S/Lsp/..) commands.\nDeletes any lines not beginning with a prompt, and then removes the\nprompt from those lines that remain.  Prefix argument means to\nclean even if the buffer is \\[read-only].\n\n(fn BEG END EVEN-IF-READ-ONLY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-trns" '("ess-transcript-")))

;;;***

;;;### (autoloads nil "ess-utils" "ess-utils.el" (0 0 0 0))
;;; Generated autoloads from ess-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-utils" '("ess-")))

;;;***

;;;### (autoloads nil "ess-vst-d" "ess-vst-d.el" (0 0 0 0))
;;; Generated autoloads from ess-vst-d.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-vst-d" '("VST-" "ViSta")))

;;;***

;;;### (autoloads nil "ess-xls-d" "ess-xls-d.el" (0 0 0 0))
;;; Generated autoloads from ess-xls-d.el

(autoload 'XLS-mode "ess-xls-d" "\
Major mode for editing XLispStat source.  NOT EVEN STARTED.\n\n(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.lsp\\'" . XLS-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ess-xls-d" '("xls-transcript-mode" "XLS" "ess-help-XLS-sec-keys-alist")))

;;;***

;;;### (autoloads nil "essd-els" "essd-els.el" (0 0 0 0))
;;; Generated autoloads from essd-els.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "essd-els" '("ess-" "S+elsewhere" "inferior-ess-remote-pager")))

;;;***

;;;### (autoloads nil "make-regexp" "make-regexp.el" (0 0 0 0))
;;; Generated autoloads from make-regexp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "make-regexp" '("make-regexp" "regexp-span")))

;;;***

;;;### (autoloads nil "mouseme" "mouseme.el" (0 0 0 0))
;;; Generated autoloads from mouseme.el

(autoload 'mouse-me "mouseme" "\
Popup a menu of functions to run on selected string or region.\n\n(fn EVENT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mouseme" '("mouse-me-")))

;;;***

;;;### (autoloads nil nil ("ess-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ess-autoloads.el ends here
