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

inc_local = -I$(top_srcdir)/include -I$(top_srcdir)/src/api/c
c_api = $(top_builddir)/src/api/c

bin_PROGRAMS = spd-say
spd_say_CPPFLAGS = $(inc_local) $(GLIB_CFLAGS)
spd_say_SOURCES = say.c options.c options.h
spd_say_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS) $(top_builddir)/src/common/libcommon.la

if HAVE_HELP2MAN
spd-say.1: spd-say$(EXEEXT)
	LC_ALL=C help2man -n "send text-to-speech output request to speech-dispatcher" --output=$@ ./$<

dist_man1_MANS = \
	spd-say.1
endif

CLEANFILES = $(dist_man1_MANS)

-include $(top_srcdir)/git.mk
