.PHONY: install-init install install-fonts install-gnus help all

EMACS_DIR = ~/.emacs.d
FONTS_DIR = ~/.fonts

help:
	@echo "Available targets:"
	@echo "  install-init   - Copy ONLY init.el to ~/.emacs.d/ (fastest)"
	@echo "  install        - Copy init.el, lisp/ and themes/ to ~/.emacs.d/"
	@echo "  install-fonts  - Copy fonts/ to ~/.fonts/"
	@echo "  install-gnus   - Copy gnus configuration to ~/"
	@echo "  all            - Install everything"

install-init:
	@echo "Installing init.el..."
	mkdir -p $(EMACS_DIR)
	cp init.el $(EMACS_DIR)/init.el
	@echo "Done."

install: install-init
	@echo "Installing lisp/ and themes/..."
	if [ -d "lisp" ]; then cp -R lisp/ $(EMACS_DIR)/lisp/; fi
	if [ -d "themes" ]; then cp -R themes/ $(EMACS_DIR)/themes/; fi
	@echo "Done."

install-fonts:
	@echo "Installing fonts..."
	mkdir -p $(FONTS_DIR)
	if [ -d "fonts" ]; then cp -R fonts/* $(FONTS_DIR)/; fi
	@echo "Done."

install-gnus:
	@echo "Installing Gnus configuration..."
	if [ -d "gnus_mail" ]; then \
		cp gnus_mail/gnus ~/.gnus; \
		cp gnus_mail/authinfo ~/.authinfo; \
		chmod 0600 ~/.authinfo; \
	fi
	@echo "Done. Don't forget to edit ~/.gnus and ~/.authinfo with your credentials."

all: install install-fonts install-gnus
