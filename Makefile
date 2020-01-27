# Copyright (C) 2019-2020 Serghei Iakovlev
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
# along with this file; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

## Sane defaults
SHELL ?= $(shell which sh)
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS ?= emacs
CASK ?= cask
PANDOC ?= pandoc
TAR ?= tar

EMACSFLAGS ?=
TESTFLAGS ?= --reporter ert+duration
PANDOCLAGS ?= --fail-if-warnings \
	--reference-links \
	--atx-headers \
	-f org+empty_paragraphs

PKGDIR = $(ROOT_DIR)

ARCHIVE_NAME = bnf-mode
VERSION = $(shell sh -c "grep -oh -E 'Version:.*' bnf-mode.el | cut -d' ' -f2")
PACKAGE_NAME = $(ARCHIVE_NAME)-$(VERSION)

.DEFAULT_GOAL = build

.SILENT: ;               # no need for @
.ONESHELL: ;             # recipes execute in same shell
.NOTPARALLEL: ;          # wait for this target to finish
.EXPORT_ALL_VARIABLES: ; # send all vars to shell
Makefile: ;              # skip prerequisite discovery

## File lists

SRCS = bnf-mode.el
OBJS = $(SRCS:.el=.elc)

## Internal variables

EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS =

## Programs availability

HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
RUNEMACS = $(EMACSBATCH)
else
RUNEMACS = $(CASK) exec $(EMACSBATCH)
endif

.PHONY: pkg-dir
pkg-dir:
ifeq ($(PKGDIR),$(ROOT_DIR))
ifndef HAVE_CASK
	$(error "$(CASK) is not available.  Please run make help")
else
	PKGDIR=$(shell EMACS=$(EMACS) $(CASK) package-directory)
endif
endif

%.elc: %.el $(PKGDIR)
	$(RUNEMACS) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

$(PKGDIR): pkg-dir Cask
	$(CASK) install
	touch $(PKGDIR)

## Remove badges

define org-clean
	cat $^ | sed -e "s/\[\[.*\.svg\]\]//g"
endef

$(ARCHIVE_NAME).info: README.org
	$(call org-clean,$^) | $(PANDOC) $(PANDOCLAGS) -t texinfo | makeinfo -o $@

README: README.org
	$(call org-clean,$^) | $(PANDOC) $(PANDOCLAGS) -t plain | sed -e "s/\[\]//g" > $@

ChangeLog: NEWS
	cp $^ $@

$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	$(CASK) pkg-file

$(PACKAGE_NAME).tar: README ChangeLog LICENSE $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir
	$(TAR) -c -v -f $(PACKAGE_NAME).tar --transform "s@^@$(PACKAGE_NAME)/@" $^

## Public targets

.PHONY: .title
.title:
	$(info BNF Mode $(VERSION))

.PHONY: init
init: $(PKGDIR)

.PHONY: checkdoc
checkdoc:
	$(EMACSBATCH) --eval '(checkdoc-file "$(SRCS)")'

.PHONY: build
build: $(OBJS)

.PHONY: test
test:
	$(CASK) exec ert-runner $(TESTFLAGS)

.PHONY: clean
clean:
	$(CASK) clean-elc
	$(RM) -f README ChangeLog $(ARCHIVE_NAME).info coverage-final.json
	$(RM) -f $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME)-*.tar

.PHONY: package
package: $(PACKAGE_NAME).tar

.PHONY: install
install: $(PACKAGE_NAME).tar
	$(EMACS) --batch -l package -f package-initialize --eval \
		"(let ((debug-on-error t))(package-install-file \"$(PWD)/$(PACKAGE_NAME).tar\"))"

.PHONY: help
help: .title
	echo 'Run "make init" first to install and update all local dependencies.'
	echo ''
	echo 'Available targets:'
	echo '  help:     Show this help and exit'
	echo '  init:     Initialize the project (has to be launched first)'
	echo '  checkdoc: Checks BNF Mode code for errors in the documentation'
	echo '  build:    Byte compile BNF Mode package'
	echo '  test:     Run the non-interactive unit test suite'
	echo '  clean:    Remove all byte compiled Elisp files as well as build'
	echo '            artifacts'
	echo '  package:  Build package'
	echo '  install:  Install BNF Mode'
	echo ''
	echo 'Available programs:'
	echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	echo ''
	echo 'You need $(CASK) to develop BNF Mode.'
	echo 'See http://cask.readthedocs.io/ for more.'
	echo ''
