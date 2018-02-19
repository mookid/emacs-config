EMACSD=~/.emacs.d
EMACSELPA=~/.emacs.d/elpa

EMACS=\
emacs -Q --no-init --batch \
--eval "(package-initialize)" \
--eval "(require 'cl)" \
--eval "(add-to-list 'load-path \"${EMACSELPA}\")" \

.PHONY: modules elpa clean all

all: elpa cl-hyperspec package-update

elpa:
	find $(EMACSELPA) -name *.elc | xargs rm; \
	$(EMACS) --eval "(byte-recompile-directory \"${EMACSELPA}\" 0 t)"

cl-hyperspec:
	wget ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
	tar xvzf HyperSpec-7-0.tar.gz

clean:
	find $(EMACSD) -name *.elc | xargs rm -f
	rm -rf HyperSpec*

package-update:
	emacs -batch -l $(HOME)/.emacs.d/init.el -f package-utils-upgrade-all -kill && git add elpa && git commit -am"Package update."
