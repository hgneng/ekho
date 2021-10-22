#
# Copyright (C) 2002 - 2018 Brailcom, o.p.s.
#
# This is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

## Process this file with automake to produce Makefile.in

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = speech-dispatcher.pc

BUILT_SOURCES = $(top_srcdir)/.version

SUBDIRS= include locale src config doc po

EXTRA_DIST= config.rpath COPYING.LGPL COPYING.GPL-2 COPYING.GPL-3 ANNOUNCE BUGS FAQ README.md README.packagers README.translators README.overview.md README.style.md build.sh git-version-gen split-version.sh speech-dispatcherd.service.in po/Makevars.in po/README

CLEANFILES=

MAINTAINERCLEANFILES=configure

AM_DISTCHECK_CONFIGURE_FLAGS=--without-systemdsystemunitdir

nodist_systemdsystemunit_DATA = \
	speech-dispatcherd.service
CLEANFILES += \
	speech-dispatcherd.service
%.service: %.service.in
	$(AM_V_GEN)sed -e 's,@bindir\@,$(bindir),g' $< > $@

testinstall: install check
	cd src/tests && $(MAKE) $(AM_MAKEFLAGS) testinstall

$(top_srcdir)/.version:
	echo $(VERSION) > $@-t && mv $@-t $@

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version
	echo $(VERSION) > $(distdir)/.version
	rm $(distdir)/po/speech-dispatcher.pot

-include $(top_srcdir)/git.mk

GITIGNOREFILES = $(GITIGNORE_MAINTAINERCLEANFILES_TOPLEVEL) py-compile

ACLOCAL_AMFLAGS = -I m4
