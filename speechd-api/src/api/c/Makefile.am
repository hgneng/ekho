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

spdinclude_HEADERS = libspeechd.h libspeechd_version.h
inc_local = -I$(top_srcdir)/include/
BUILT_SOURCES = libspeechd_version.h

lib_LTLIBRARIES = libspeechd.la
libspeechd_la_SOURCES = libspeechd.c
libspeechd_la_CFLAGS = $(ERROR_CFLAGS)
libspeechd_la_CPPFLAGS = $(inc_local) -D_GNU_SOURCE $(GLIB_CFLAGS) -DSPD_SPAWN_CMD=\""$(prefix)/bin/speech-dispatcher"\"
libspeechd_la_LDFLAGS = -version-info $(LIB_SPD_CURRENT):$(LIB_SPD_REVISION):$(LIB_SPD_AGE)
libspeechd_la_LIBADD = $(GLIB_LIBS)

libspeechd_version.h: $(srcdir)/libspeechd_version.h.in

CLEANFILES = libspeechd_version.h

EXTRA_DIST = libspeechd_version.h.in

libspeechd_version.h: Makefile
	rm -f $@
	srcdir=; \
	test -f ./$@.in || srcdir=$(srcdir)/; \
	$(edit_version) $${srcdir}$@.in > $@

edit_version = sed \
		-e 's:[@]LIBSPEECHD_MAJOR_VERSION[@]:$(MAJOR_VERSION):' \
		-e 's:[@]LIBSPEECHD_MINOR_VERSION[@]:$(MINOR_VERSION):' \
		-e 's:[@]LIBSPEECHD_MICRO_VERSION[@]:$(MICRO_VERSION):'

-include $(top_srcdir)/git.mk
