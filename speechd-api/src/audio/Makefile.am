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

inc_local = -I$(top_srcdir)/include/

audio_LTLIBRARIES =

if alsa_support
audio_LTLIBRARIES +=  spd_alsa.la
spd_alsa_la_SOURCES = alsa.c
spd_alsa_la_CPPFLAGS = $(GLIB_CFLAGS) $(inc_local)  $(ALSA_CFLAGS)
spd_alsa_la_LIBADD = $(ALSA_LIBS) $(GLIB_LIBS)
spd_alsa_la_LDFLAGS = -module -avoid-version
endif

if libao_support
audio_LTLIBRARIES +=  spd_libao.la
spd_libao_la_SOURCES = libao.c
spd_libao_la_CPPFLAGS = $(GLIB_CFLAGS) $(inc_local)  $(LIBAO_CFLAGS)
spd_libao_la_LIBADD = $(LIBAO_LIBS) $(GLIB_LIBS)
spd_libao_la_LDFLAGS = -module -avoid-version
endif

if nas_support
audio_LTLIBRARIES +=  spd_nas.la
spd_nas_la_SOURCES = nas.c
spd_nas_la_CPPFLAGS = $(GLIB_CFLAGS) $(inc_local)
spd_nas_la_LIBADD = $(NAS_LIBS) $(GLIB_LIBS)
spd_nas_la_LDFLAGS = -module -avoid-version
endif

if oss_support
audio_LTLIBRARIES +=  spd_oss.la
spd_oss_la_SOURCES = oss.c
spd_oss_la_CPPFLAGS = $(GLIB_CFLAGS) $(inc_local)
spd_oss_la_LIBADD = $(GLIB_LIBS)
spd_oss_la_LDFLAGS = -module -avoid-version
endif

if pulse_support
audio_LTLIBRARIES +=  spd_pulse.la
spd_pulse_la_SOURCES = pulse.c
spd_pulse_la_CPPFLAGS = $(GLIB_CFLAGS) $(inc_local)  $(PULSE_CFLAGS)
spd_pulse_la_LIBADD = $(PULSE_LIBS) $(GLIB_LIBS)
spd_pulse_la_LDFLAGS = -module -avoid-version
endif

-include $(top_srcdir)/git.mk
