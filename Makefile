EMACSD=~/.emacs.d
EMACSELPA=~/.emacs.d/elpa

EMACS=\
emacs -Q --no-init --batch \
--eval "(package-initialize)" \
--eval "(require 'cl)" \
--eval "(add-to-list 'load-path \"${EMACSELPA}\")" \

.PHONY: modules elpa clean all

all: elpa cl-hyperspec

elpa:
	find $(EMACSELPA) -name *.elc | xargs -r rm; \
	$(EMACS) --eval "(byte-recompile-directory \"${EMACSELPA}\" 0 t)"

cl-hyperspec:
	wget ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
	tar xvzf HyperSpec-7-0.tar.gz

clean:
	find $(EMACSD) -name *.elc | xargs -r rm -f
	rm -rf HyperSpec*
