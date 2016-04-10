EMACSD=~/.emacs.d
EMACSELPA=~/.emacs.d/elpa
EMACSMODULES=~/.emacs.d/modules
EMACSLISP=~/.emacs.d/lisp

EMACS=\
emacs -Q --no-init --batch \
--eval "(package-initialize)" \
--eval "(require 'cl)" \
--eval "(add-to-list 'load-path \"${EMACSELPA}\")" \
--eval "(add-to-list 'load-path \"${EMACSMODULES}\")" \

.PHONY: modules elpa clean all

all: modules elpa

modules:
	$(EMACS) --eval "(byte-recompile-directory \"${EMACSMODULES}\" 0 t)"

elpa:
	find $(EMACSELPA) -name *.elc | xargs rm; \
	$(EMACS) --eval "(byte-recompile-directory \"${EMACSELPA}\" 0 t)"

clean:
	find $(EMACSELPA) -type d -empty | xargs rm;
	find $(EMACSELPA) -name *.elc | xargs rm;
	find $(EMACSLISP) -name *.elc | xargs rm;
	find $(EMACSMODULES) -name *.elc | xargs rm;
