EMACSD=~/.emacs.d
EMACSELPA=~/.emacs.d/elpa
EMACSMODULES=~/.emacs.d/modules

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
	rm -rf $(EMACSELPA) && git reset --hard &&\
	$(EMACS) --eval "(byte-recompile-directory \"${EMACSELPA}\" 0 t)"

clean:
	cd $(EMACSMODULES) && rm -f *.elc; \
	cd $(EMACSELPA) && rm -f *.elc;
