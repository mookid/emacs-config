EMACS=\
emacs -Q --no-init --batch \
--eval "(package-initialize)" \
--eval "(require 'cl)" \
--eval "(add-to-list 'load-path \"~/.emacs.d/elpa/\")" \
--eval "(add-to-list 'load-path \"~/.emacs.d/modules/\")" \

EMACSD=~/.emacs.d
EMACSELPA=~/.emacs.d/elpa
EMACSMODULES=~/.emacs.d/modules

.PHONY: modules elpa clean

modules:
	cd $(EMACSMODULES);\
	$(EMACS) --eval "(byte-recompile-directory \"~/.emacs.d/modules\" 0 t)";\
	cd $(EMACSD);\

elpa:
	$(EMACS) --eval "(byte-recompile-directory \"~/.emacs.d/elpa\" 0 t)";\

clean:
	cd $(EMACSMODULES);\
	rm -f *.elc; \
	cd $(EMACSD);\
