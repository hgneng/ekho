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

EXTRA_DIST = baratinoo_shim.c README.baratinoo
EXTRA_DIST += kali_Kali_shim.cpp kali_KGlobal_shim.cpp kali_KTrans_shim.cpp kali_KParle_shim.cpp kali_KAnalyse_shim.cpp
EXTRA_DIST += ibmtts_shim.c eci.h
EXTRA_DIST += mluvitko.c
CLEANFILES =

inc_local = -I$(top_srcdir)/include
audio_SOURCES = spd_audio.c spd_audio.h
common_SOURCES = module_main.c module_utils.c module_utils.h
common_LDADD = $(SNDFILE_LIBS) $(DOTCONF_LIBS) $(GLIB_LIBS) $(GTHREAD_LIBS)

AM_CFLAGS = $(ERROR_CFLAGS)
AM_CPPFLAGS = $(inc_local) -DDATADIR=\"$(snddatadir)\" -D_GNU_SOURCE \
	-DPLUGIN_DIR="\"$(audiodir)\"" \
	$(DOTCONF_CFLAGS) $(GLIB_CFLAGS) $(GTHREAD_CFLAGS) \
	$(ibmtts_include) $(SNDFILE_CFLAGS)

modulebin_PROGRAMS = sd_dummy sd_generic sd_festival sd_cicero

sd_dummy_SOURCES = dummy.c $(audio_SOURCES) $(common_SOURCES) \
	module_utils_addvoice.c
sd_dummy_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)
dist_snddata_DATA = dummy-message.wav

sd_festival_SOURCES = festival.c festival_client.c festival_client.h \
	$(audio_SOURCES) $(common_SOURCES)
sd_festival_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD) $(EXTRA_SOCKET_LIBS)

sd_generic_SOURCES = generic.c $(audio_SOURCES) $(common_SOURCES) \
	module_utils_addvoice.c
sd_generic_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)

sd_cicero_SOURCES = cicero.c $(audio_SOURCES) $(common_SOURCES)
sd_cicero_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)

if flite_support
modulebin_PROGRAMS += sd_flite
sd_flite_SOURCES = flite.c $(audio_SOURCES) $(common_SOURCES)
sd_flite_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(flite_kal) $(flite_basic) \
	$(common_LDADD)
endif

if ibmtts_support
modulebin_PROGRAMS += sd_ibmtts
sd_ibmtts_SOURCES = ibmtts.c $(audio_SOURCES) $(common_SOURCES) \
	module_utils_addvoice.c
sd_ibmtts_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	-libmeci \
	$(common_LDADD)

if ibmtts_shim
sd_ibmtts_LDADD += -L.

EXTRA_sd_ibmtts_DEPENDENCIES = libibmeci.so

libibmeci.so: $(srcdir)/ibmtts_shim.c
	$(CC) -fPIC -shared $< -o $@

CLEANFILES += $(EXTRA_sd_ibmtts_DEPENDENCIES)
endif
endif

if espeak_support
modulebin_PROGRAMS += sd_espeak
sd_espeak_SOURCES = espeak.c $(audio_SOURCES) $(common_SOURCES) module_utils_speak_queue.c
sd_espeak_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	-lespeak $(EXTRA_ESPEAK_LIBS) \
	$(common_LDADD)
endif

if espeak_ng_support
modulebin_PROGRAMS += sd_espeak-ng
sd_espeak_ng_SOURCES = espeak-ng.c $(audio_SOURCES) $(common_SOURCES) module_utils_speak_queue.c
sd_espeak_ng_CFLAGS = -DESPEAK_NG_INCLUDE $(ESPEAK_NG_CFLAGS)
sd_espeak_ng_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(ESPEAK_NG_LIBS) $(EXTRA_ESPEAK_LIBS) \
	$(common_LDADD)
endif

if ivona_support
modulebin_PROGRAMS += sd_ivona
sd_ivona_SOURCES = ivona.c ivona_client.c ivona_client.h $(audio_SOURCES) \
	$(common_SOURCES)
sd_ivona_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	-ldumbtts \
	$(common_LDADD)
endif

if pico_support
modulebin_PROGRAMS += sd_pico
sd_pico_SOURCES = pico.c $(audio_SOURCES) $(common_SOURCES)
sd_pico_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) -lttspico \
	$(common_LDADD)
endif

if baratinoo_support
modulebin_PROGRAMS += sd_baratinoo
sd_baratinoo_SOURCES = baratinoo.c baratinoo_compat.h $(audio_SOURCES) $(common_SOURCES) module_utils_speak_queue.c
sd_baratinoo_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) -lbaratinoo -lpthread -ldl \
	$(common_LDADD)

if baratinoo_shim
sd_baratinoo_LDADD += -L.

EXTRA_sd_baratinoo_DEPENDENCIES = libbaratinoo.so

libbaratinoo.so: $(srcdir)/baratinoo_shim.c
	$(CC) -fPIC -shared $< -o $@

CLEANFILES += $(EXTRA_sd_baratinoo_DEPENDENCIES)
endif
endif

if kali_support
modulebin_PROGRAMS += sd_kali
KALI_DIR = /usr/lib/kali
sd_kali_SOURCES = kali.cpp $(audio_SOURCES) $(common_SOURCES)
sd_kali_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	-lKali -lKGlobal -lKTrans -lKParle -lKAnalyse \
	$(common_LDADD)
sd_kali_CPPFLAGS = -DLINUX $(AM_CPPFLAGS)

if kali_shim
sd_kali_LDADD += -L.
sd_kali_CPPFLAGS += -I$(srcdir)/kali_shim

noinst_HEADERS = kali_shim/kali/Kali/kali.h module_utils_speak_queue.h

EXTRA_sd_kali_DEPENDENCIES = libKali.so libKGlobal.so libKTrans.so libKParle.so libKAnalyse.so

lib%.so: $(srcdir)/kali_%_shim.cpp
	$(CXX) -fPIC -shared $< -o $@ -I$(srcdir)/kali_shim

CLEANFILES += $(EXTRA_sd_kali_DEPENDENCIES)
endif
endif

-include $(top_srcdir)/git.mk
