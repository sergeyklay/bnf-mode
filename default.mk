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

# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

TOP := $(dir $(lastword $(MAKEFILE_LIST)))

EMACS  ?= emacs
CASK   ?= cask
PANDOC ?= pandoc
TAR    ?= tar

EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS   =

HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
RUNEMACS = $(EMACSBATCH)
else
RUNEMACS = $(CASK) exec $(EMACSBATCH)
endif

EMACSFLAGS ?=
TESTFLAGS  ?= --reporter ert+duration
PANDOCLAGS ?= --fail-if-warnings \
	--reference-links \
	--atx-headers \
	-f org+empty_paragraphs

ARCHIVE_NAME = bnf-mode
PACKAGE_NAME = $(ARCHIVE_NAME)-$(VERSION)

VERSION = 0.4.4

SRCS = bnf-mode.el
OBJS = $(SRCS:.el=.elc)
