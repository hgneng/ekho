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
EXTRA_DIST += voxin_shim.c voxin.h
EXTRA_DIST += module_main.h module_utils.h
EXTRA_DIST += dummy-message-default.wav
EXTRA_DIST += dummy-message.txt
CLEANFILES = dummy-message.wav

inc_local = -I$(top_srcdir)/include -I$(top_srcdir)/src/common
common_SOURCES = module_main.c module_readline.c module_process.c module_config.c module_utils.c module_utils.h
common_LDADD = $(DOTCONF_LIBS) $(GLIB_LIBS) $(audio_dlopen)

AM_CFLAGS = $(ERROR_CFLAGS)
AM_CXXFLAGS = $(ERROR_CFLAGS)
AM_CPPFLAGS = $(inc_local) -DDATADIR=\"$(snddatadir)\" -D_GNU_SOURCE $(GLIB_CFLAGS)

module_utils_CPPFLAGS = $(AM_CPPFLAGS) \
	$(DOTCONF_CFLAGS)

module_utils_play_CPPFLAGS = $(AM_CPPFLAGS) \
	$(SNDFILE_CFLAGS)

noinst_PROGRAMS = sd_skeleton0 sd_skeleton_config

sd_skeleton0_SOURCES = skeleton0.c module_main.c module_readline.c module_process.c

sd_skeleton_config_SOURCES = skeleton_config.c $(common_SOURCES)
sd_skeleton_config_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)

modulebin_PROGRAMS = sd_dummy sd_generic sd_festival sd_cicero

sd_dummy_SOURCES = dummy.c $(common_SOURCES) \
	module_utils_addvoice.c
sd_dummy_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)
dist_snddata_DATA = dummy-message.wav

sd_festival_SOURCES = festival.c festival_client.c festival_client.h \
	$(common_SOURCES)
sd_festival_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(common_LDADD) $(EXTRA_SOCKET_LIBS)

sd_generic_SOURCES = generic.c $(common_SOURCES) \
	module_utils_addvoice.c
sd_generic_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)

sd_cicero_SOURCES = cicero.c $(common_SOURCES)
sd_cicero_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	$(common_LDADD)

if flite_support
modulebin_PROGRAMS += sd_flite
sd_flite_SOURCES = flite.c $(common_SOURCES)
sd_flite_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(flite_kal) $(flite_basic) \
	$(common_LDADD)
endif

if ibmtts_support
modulebin_PROGRAMS += sd_ibmtts
sd_ibmtts_SOURCES = ibmtts.c $(common_SOURCES) \
	module_utils_addvoice.c
sd_ibmtts_LDADD = $(top_builddir)/src/common/libcommon.la \
	-libmeci \
	$(audio_dlopen_modules) \
	$(common_LDADD)
ibmtts_CPPFLAGS = $(AM_CPPFLAGS) \
	$(DOTCONF_CFLAGS) \
	$(ibmtts_include)

if ibmtts_shim
sd_ibmtts_LDADD += -L.
sd_ibmtts_LDFLAGS = -Wl,--disable-new-dtags -Wl,-rpath=/opt/IBM/ibmtts/lib

EXTRA_sd_ibmtts_DEPENDENCIES = libibmeci.so

libibmeci.so: $(srcdir)/ibmtts_shim.c
	$(CC) -fPIC -shared $< -o $@

CLEANFILES += $(EXTRA_sd_ibmtts_DEPENDENCIES)
endif
endif

if espeak_support
modulebin_PROGRAMS += sd_espeak
sd_espeak_SOURCES = espeak.c $(common_SOURCES)
sd_espeak_LDADD = $(top_builddir)/src/common/libcommon.la \
	-lespeak $(EXTRA_ESPEAK_LIBS) \
	$(common_LDADD)
endif

if espeak_ng_support
modulebin_PROGRAMS += sd_espeak-ng
sd_espeak_ng_SOURCES = espeak.c $(common_SOURCES)
sd_espeak_ng_CFLAGS = -DESPEAK_NG_INCLUDE $(ESPEAK_NG_CFLAGS)
sd_espeak_ng_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(ESPEAK_NG_LIBS) $(EXTRA_ESPEAK_LIBS) \
	$(common_LDADD)

noinst_PROGRAMS += sd_skeleton0_espeak-ng-sync sd_skeleton0_espeak-ng-sync-server sd_skeleton0_espeak-ng-async-server
sd_skeleton0_espeak_ng_sync_SOURCES = skeleton0_espeak-ng-sync.c module_main.c module_readline.c module_process.c
sd_skeleton0_espeak_ng_sync_CFLAGS = $(ESPEAK_NG_CFLAGS)
sd_skeleton0_espeak_ng_sync_LDADD = $(ESPEAK_NG_LIBS) $(EXTRA_ESPEAK_LIBS)

noinst_PROGRAMS += sd_skeleton0_espeak-ng-async
sd_skeleton0_espeak_ng_async_SOURCES = skeleton0_espeak-ng-async.c module_main.c module_readline.c module_process.c
sd_skeleton0_espeak_ng_async_CFLAGS = $(ESPEAK_NG_CFLAGS)
sd_skeleton0_espeak_ng_async_LDADD = $(ESPEAK_NG_LIBS) $(EXTRA_ESPEAK_LIBS)

noinst_PROGRAMS += sd_skeleton0_espeak-ng-sync-server
sd_skeleton0_espeak_ng_sync_server_SOURCES = skeleton0_espeak-ng-sync-server.c module_main.c module_readline.c module_process.c
sd_skeleton0_espeak_ng_sync_server_CFLAGS = $(ESPEAK_NG_CFLAGS)
sd_skeleton0_espeak_ng_sync_server_LDADD = $(ESPEAK_NG_LIBS) $(EXTRA_ESPEAK_LIBS)

noinst_PROGRAMS += sd_skeleton0_espeak-ng-async-server
sd_skeleton0_espeak_ng_async_server_SOURCES = skeleton0_espeak-ng-async-server.c module_main.c module_readline.c module_process.c
sd_skeleton0_espeak_ng_async_server_CFLAGS = $(ESPEAK_NG_CFLAGS)
sd_skeleton0_espeak_ng_async_server_LDADD = $(ESPEAK_NG_LIBS) $(EXTRA_ESPEAK_LIBS)
endif

if ivona_support
modulebin_PROGRAMS += sd_ivona
sd_ivona_SOURCES = ivona.c ivona_client.c ivona_client.h \
	$(common_SOURCES) module_utils_play.c
sd_ivona_LDADD = $(top_builddir)/src/common/libcommon.la \
	$(audio_dlopen_modules) \
	-ldumbtts \
	$(SNDFILE_LIBS) \
	$(common_LDADD)
endif

if pico_support
modulebin_PROGRAMS += sd_pico
sd_pico_SOURCES = pico.c $(common_SOURCES)
sd_pico_LDADD = $(top_builddir)/src/common/libcommon.la \
	-lttspico \
	$(common_LDADD)
endif

if baratinoo_support
modulebin_PROGRAMS += sd_baratinoo
sd_baratinoo_SOURCES = baratinoo.c baratinoo_compat.h $(common_SOURCES)
sd_baratinoo_LDADD = $(top_builddir)/src/common/libcommon.la \
	-lbaratinoo \
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
sd_kali_SOURCES = kali.cpp $(common_SOURCES)
sd_kali_LDADD = $(top_builddir)/src/common/libcommon.la \
	-lKali -lKGlobal -lKTrans -lKParle -lKAnalyse \
	$(common_LDADD)
sd_kali_CPPFLAGS = -DLINUX $(AM_CPPFLAGS)

if kali_shim
sd_kali_LDADD += -L.
sd_kali_CPPFLAGS += -I$(srcdir)/kali_shim

noinst_HEADERS = kali_shim/kali/Kali/kali.h

EXTRA_sd_kali_DEPENDENCIES = libKali.so libKGlobal.so libKTrans.so libKParle.so libKAnalyse.so

lib%.so: $(srcdir)/kali_%_shim.cpp
	$(CXX) -fPIC -shared $< -o $@ -I$(srcdir)/kali_shim

CLEANFILES += $(EXTRA_sd_kali_DEPENDENCIES)
endif
endif

if voxin_support
modulebin_PROGRAMS += sd_voxin
sd_voxin_SOURCES = ibmtts.c $(common_SOURCES) \
	module_utils_addvoice.c
sd_voxin_LDADD = $(top_builddir)/src/common/libcommon.la \
	-lvoxin \
	$(common_LDADD)
sd_voxin_CPPFLAGS = -DVOXIN $(AM_CPPFLAGS) \
	$(DOTCONF_CFLAGS) \
	$(ibmtts_include)

if voxin_shim
sd_voxin_LDADD += -L.

EXTRA_sd_voxin_DEPENDENCIES = libvoxin.so

libvoxin.so: $(srcdir)/voxin_shim.c
	$(CC) -fPIC -shared $< -o $@

CLEANFILES += $(EXTRA_sd_voxin_DEPENDENCIES)
endif
endif

if flite_dummy_message
dummy-message.wav: dummy-message.txt
	$(FLITE) -voice slt -w $@ -f $<
else
if espeak_dummy_message
dummy-message.wav: dummy-message.txt
	$(ESPEAK) -v en -w $@ -f $<
else
dummy-message.wav: dummy-message-default.wav
	cp $< $@
endif
endif

-include $(top_srcdir)/git.mk
