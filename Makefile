# Copyright (C) 2019-2020 Free Software Foundation, Inc.
#
# License
#
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

include default.mk

%.info: %.texi
	$(info Generating $@)
	@$(MAKEINFO) --no-split $< -o $@

%.elc: %.el
	@printf "Compiling $<\n"
	@$(RUNEMACS) --eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $<

dir: $(INFOPAGES)
	$(info Generating dir)
	@echo $^ | xargs -n 1 $(INSTALL_INFO) --dir=$@

# Remove badges
define org-clean
	@cat $^ | sed -e "s/\[\[.*\.svg\]\]//g"
endef

README: README.org
	$(call org-clean,$^) | \
		$(PANDOC) $(PANDOCLAGS) -t plain | sed -e "s/\[\]//g" > $@

ChangeLog: NEWS
	@cp $^ $@

$(PACKAGE)-pkg.el: $(PACKAGE).el
	@$(CASK) pkg-file

$(ARCHIVE_NAME).tar: $(ARCHIVE_CONTENTS)
	@$(TAR) -c -v -f $(ARCHIVE_NAME).tar \
		--transform "s@^@$(ARCHIVE_NAME)/@" $^

## Public targets

.PHONY: .title
.title:
	@echo "BNF Mode $(VERSION)"

.PHONY: init
init: Cask
	@$(CASK) install

.PHONY: checkdoc
checkdoc:
	@$(EMACSBATCH) --eval '(checkdoc-file "$(SRCS)")'
	$(info Done.)

.PHONY: build
build: $(OBJS)

.PHONY: test
test:
	@$(CASK) exec buttercup $(TESTFLAGS)

.PHONY: clean
clean:
	$(info Remove all byte compiled Elisp files...)
	@$(CASK) clean-elc
	$(info Remove build artefacts...)
	@$(RM) README ChangeLog coverage-final.json
	@$(RM) $(PACKAGE)-pkg.el $(PACKAGE)-*.tar

.PHONY: package
package: $(ARCHIVE_NAME).tar

.PHONY: install
install: $(ARCHIVE_NAME).tar
	@$(EMACS) --batch -l package -f package-initialize --eval \
		"(let ((debug-on-error t))(package-install-file \"$(PWD)/$(ARCHIVE_NAME).tar\"))"

.PHONY: info
info: $(INFOPAGES) dir

.PHONY: help
help: .title
	@echo ''
	@echo 'Run "make init" first to install and update all local dependencies.'
	@echo 'See "default.mk" for variables you might want to set.'
	@echo ''
	@echo 'Available targets:'
	@echo '  help:       Show this help and exit'
	@echo '  init:       Initialize the project (has to be launched first)'
	@echo '  checkdoc:   Checks BNF Mode code for errors in the documentation'
	@echo '  build:      Byte compile BNF Mode package'
	@echo '  test:       Run the non-interactive unit test suite'
	@echo '  clean:      Remove all byte compiled Elisp files, documentation,'
	@echo '              build artefacts and tarball'
	@echo '  package:    Build package'
	@echo '  install:    Install BNF Mode'
	@echo '  info:       Generate info manual'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	@echo ''
	@echo 'You need $(CASK) to develop BNF Mode.'
	@echo 'See https://cask.readthedocs.io/ for more.'
	@echo ''
