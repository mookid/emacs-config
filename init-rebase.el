;; Put the following to the [core] section of .gitconfig to use this file:
;; editor = emacs -Q -l /path/to/.emacs.d/init-rebase.el
(set-default-coding-systems 'utf-8)
(add-to-list 'default-frame-alist `(font . "Inconsolata 14"))
(add-to-list 'load-path "/home/nmoreau/git-simple-rebase")
(require 'git-simple-rebase)
(add-hook 'find-file-hook 'git-simple-rebase-hook)
