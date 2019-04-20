# Copyright (C) 2019 Serghei Iakovlev
#
# This file is NOT part of GNU Emacs.
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

SHELL := $(shell which bash)
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS ?= emacs
CASK ?= cask
PANDOC ?= pandoc

EMACSFLAGS ?=
TESTFLAGS ?= --reporter ert+duration
PANDOCLAGS ?= --fail-if-warnings \
	--reference-links \
	--atx-headers \
	-f org+empty_paragraphs

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# File lists
SRCS = bnf-mode.el
OBJS = $(SRCS:.el=.elc)

ARCHIVE_NAME=bnf-mode
VERSION ?= $(shell $(CASK) version)

.SILENT: ;               # no need for @
.ONESHELL: ;             # recipes execute in same shell
.NOTPARALLEL: ;          # wait for this target to finish
.EXPORT_ALL_VARIABLES: ; # send all vars to shell
Makefile: ;              # skip prerequisite discovery

# Run make help by default
.DEFAULT_GOAL = build

# Internal variables
EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS =

# Program availability
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
RUNEMACS = $(EMACSBATCH)
else
RUNEMACS = $(CASK) exec $(EMACSBATCH)
endif

%.elc: %.el $(PKGDIR)
	$(RUNEMACS) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

define org-clean
	cat $^ | sed -e "s/\[\[.*\.svg\]\]//g"
endef

$(ARCHIVE_NAME).info: README.org
	$(call org-clean,$^) | $(PANDOC) $(PANDOCLAGS) -t texinfo | makeinfo -o $@

README: README.org
	$(call org-clean,$^) | $(PANDOC) $(PANDOCLAGS) -t plain -o $@

# Public targets

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
	$(RM) -f README

.PHONY: help
help: .title
	echo 'Run `make init` first to install and update all local dependencies.'
	echo ''
	echo 'Available targets:'
	echo '  help:     Show this help and exit'
	echo '  init:     Initialise the project (has to be launched first)'
	echo '  checkdoc: Checks BNF Mode code for errors in documentation'
	echo '  build:    Byte compile BNF Mode package'
	echo '  test:     Run the non-interactive unit test suite'
	echo '  clean:    Remove all byte compiled Elisp files'
	echo ''
	echo 'Available programs:'
	echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	echo ''
	echo 'You need $(CASK) to develop BNF Mode.  See http://cask.readthedocs.io/ for more.'
	echo ''
