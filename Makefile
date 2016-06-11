EMACSD=~/.emacs.d
EMACSELPA=~/.emacs.d/elpa

EMACS=\
emacs -Q --no-init --batch \
--eval "(package-initialize)" \
--eval "(require 'cl)" \
--eval "(add-to-list 'load-path \"${EMACSELPA}\")" \

.PHONY: modules elpa clean all

all: elpa

elpa:
	find $(EMACSELPA) -name *.elc | xargs rm; \
	$(EMACS) --eval "(byte-recompile-directory \"${EMACSELPA}\" 0 t)"

clean:
	find $(EMACSD) -name *.elc | xargs rm -f
