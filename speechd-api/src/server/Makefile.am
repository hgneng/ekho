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

inc_local = -I$(top_srcdir)/include/ -I$(top_srcdir)/src/common
lib_common = $(top_builddir)/src/common/libcommon.la

bin_PROGRAMS = speech-dispatcher
speech_dispatcher_SOURCES = speechd.c speechd.h server.c server.h \
	history.c history.h module.c module.h configuration.c configuration.h \
	parse.c parse.h set.c set.h msg.h alloc.c alloc.h \
	compare.c compare.h speaking.c speaking.h options.c options.h \
	output.c output.h sem_functions.c sem_functions.h \
	index_marking.c index_marking.h symbols.c symbols.h \
	speak_queue.c speak_queue.h
speech_dispatcher_CFLAGS = $(ERROR_CFLAGS)
speech_dispatcher_CPPFLAGS = $(inc_local) $(DOTCONF_CFLAGS) $(GLIB_CFLAGS) \
	$(GMODULE_CFLAGS) $(GTHREAD_CFLAGS) -DSYS_CONF=\"$(spdconfdir)\" \
	-DSND_DATA=\"$(snddatadir)\" \
	-DMODULEBINDIR=\"$(modulebindir)\" \
	-DOLDMODULEBINDIR=\"$(oldmodulebindir)\" \
	-DLOCALE_DATA=\"$(localedatadir)\" \
	-D_GNU_SOURCE -DDEFAULT_AUDIO_METHOD=\"$(default_audio_method)\"
speech_dispatcher_LDFLAGS = $(RDYNAMIC)
speech_dispatcher_LDADD = $(lib_common) $(DOTCONF_LIBS) $(GLIB_LIBS) \
	$(SNDFILE_LIBS) $(GMODULE_LIBS) $(GTHREAD_LIBS) $(EXTRA_SOCKET_LIBS)

if HAVE_HELP2MAN
speech-dispatcher.1: speech-dispatcher$(EXEEXT)
	LC_ALL=C help2man -n "speech synthesis daemon" --output=$@ ./$<

dist_man1_MANS = \
	speech-dispatcher.1
endif

CLEANFILES = $(dist_man1_MANS)

-include $(top_srcdir)/git.mk
