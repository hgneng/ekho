
localedir = $(datadir)/locale
inc_local = "-I$(top_srcdir)/intl/"

SNDFILE_CFLAGS = @SNDFILE_CFLAGS@
SNDFILE_LIBS = @SNDFILE_LIBS@

EXTRA_DIST = module_main.c module_utils_addvoice.c festival_client.c festival_client.h ivona_client.c dummy.c dummy-message.wav

AM_CFLAGS = -Wall -DLOCALEDIR=\"$(localedir)\" -DDATADIR=\"$(snddatadir)\" -I/usr/include/ $(inc_local) @glib_include@ -L$(top_srcdir)/src/audio -I$(top_srcdir)/src/audio -I../../intl/ $(ibmtts_include) @SNDFILE_CFLAGS@ -Wl,--rpath -Wl,$(spdlibdir)
AM_CXXFLAGS = -Wall -DLOCALEDIR=\"$(localedir)\" -DDATADIR=\"$(snddatadir)\" -I/usr/include/ $(inc_local) @glib_include@ -L$(top_srcdir)/src/audio -I$(top_srcdir)/src/audio -I../../intl/ $(ibmtts_include) @SNDFILE_CFLAGS@ -Wl,--rpath -Wl,$(spdlibdir)

modulebin_PROGRAMS = sd_dummy sd_generic sd_festival sd_cicero

snddata_DATA = dummy-message.wav

modulebin_PROGRAMS += sd_ekho

if flite_support
modulebin_PROGRAMS += sd_flite
endif

if ibmtts_support
modulebin_PROGRAMS += sd_ibmtts
endif

if espeak_support
modulebin_PROGRAMS += sd_espeak
endif

if ivona_support
modulebin_PROGRAMS += sd_ivona
endif

EKHO_ROOT = /home/hgneng/e-guidedog/eGuideDog_TTS
sd_ekho_SOURCES = ekho.cc module_utils.c module_utils.h
sd_ekho_LDFLAGS = -rpath '$(spdlibdir)'
sd_ekho_LDADD = $(EKHO_ROOT)/build/libekho.a $(EKHO_ROOT)/lib/libFestival.a $(EKHO_ROOT)/lib/libestools.a $(EKHO_ROOT)/lib/libestbase.a $(EKHO_ROOT)/lib/libeststring.a -lsdaudio -lpthread -ldotconf -lsndfile -lportaudio -lm -lvorbisenc -lvorbis -lm -logg -lmp3lame -lcurses $(EKHO_ROOT)/lib/libSoundTouch.a @glib_libs@ @gthread_libs@  

sd_flite_SOURCES = flite.c module_utils.c module_utils.h
sd_flite_LDFLAGS = -rpath '$(spdlibdir)'
sd_flite_LDADD = -lsdaudio $(flite_basic) $(flite_kal) -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_generic_SOURCES = generic.c module_utils.c module_utils.h
sd_generic_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_festival_SOURCES = festival.c module_utils.c
sd_festival_LDFLAGS = -rpath '$(spdlibdir)'
sd_festival_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_ibmtts_SOURCES = ibmtts.c module_utils.c module_utils.h
sd_ibmtts_LDFLAGS = -rpath '$(spdlibdir)'
sd_ibmtts_LDADD = -lsdaudio -lm -lpthread -ldotconf -libmeci @SNDFILE_LIBS@ @glib_libs@ @gthread_libs@

sd_cicero_SOURCES = cicero.c module_utils.c module_utils.h
sd_cicero_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_espeak_SOURCES = espeak.c module_utils.c module_utils.h
sd_espeak_LDFLAGS = -rpath '$(spdlibdir)'
sd_espeak_LDADD = -lsdaudio -lespeak -lpthread -ldotconf @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@

sd_ivona_SOURCES = ivona.c module_utils.c module_utils.h
sd_ivona_LDFLAGS = -rpath '$(spdlibdir)'
sd_ivona_LDADD = -lsdaudio -ldumbtts -lpthread -ldotconf @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@

sd_dummy_SOURCES = dummy.c module_utils.c module_utils.h
sd_dummy_LDFLAGS = -rpath '$(spdlibdir)'
sd_dummy_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@
