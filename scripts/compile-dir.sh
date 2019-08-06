#!/bin/sh

script=""
script+="(let ((inhibit-message t))"
script+="  (byte-recompile-directory default-directory nil t)"
script+="  (save-buffers-kill-emacs))"

git submodule foreach emacs -Q --eval "$script" --batch
