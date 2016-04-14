localedir = $(datadir)/locale
inc_local = "-I$(top_srcdir)/intl/"

SNDFILE_CFLAGS = @SNDFILE_CFLAGS@
SNDFILE_LIBS = @SNDFILE_LIBS@

MP3LAME_LIBS = -lpulse-simple -lpulse #-lmp3lame -lm

EKHO_ROOT = ../../..

EXTRA_DIST = module_main.c module_utils_addvoice.c festival_client.c festival_client.h ivona_client.c dummy.c dummy-message.wav

AM_CFLAGS = @ERROR_CFLAGS@ -DLOCALEDIR=\"$(localedir)\" -DDATADIR=\"$(snddatadir)\" -I/usr/include/ $(inc_local) @glib_include@ -L$(top_builddir)/src/audio -I$(top_srcdir)/src/audio -I../../intl/ $(ibmtts_include) @SNDFILE_CFLAGS@ -D_GNU_SOURCE
AM_CXXFLAGS = @ERROR_CFLAGS@ -DLOCALEDIR=\"$(localedir)\" -DDATADIR=\"$(snddatadir)\" -I/usr/include/ $(inc_local) @glib_include@ -L$(top_builddir)/src/audio -I$(top_srcdir)/src/audio -I../../intl/ $(ibmtts_include) @SNDFILE_CFLAGS@ -D_GNU_SOURCE -I$(EKHO_ROOT)/src -I$(EKHO_ROOT)/utfcpp/source -I$(EKHO_ROOT)/sonic -fpermissive

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

sd_ekho_SOURCES = ekho.cc module_utils.c module_utils.h
sd_ekho_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_ekho_LDADD = $(EKHO_ROOT)/libekho.a -lFestival -lestools -lestbase -leststring -lsdaudio -lpthread -ldotconf -lsndfile -lm -lvorbisenc -lvorbis -lm -logg -lcurses @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@ $(MP3LAME_LIBS)

sd_flite_SOURCES = flite.c module_utils.c module_utils.h
sd_flite_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_flite_LDADD = -lsdaudio $(flite_kal) $(flite_basic) -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_generic_SOURCES = generic.c module_utils.c module_utils.h
sd_generic_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_festival_SOURCES = festival.c module_utils.c
sd_festival_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_festival_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @gthread_libs@ @EXTRA_SOCKET_LIBS@

sd_ibmtts_SOURCES = ibmtts.c module_utils.c module_utils.h
sd_ibmtts_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_ibmtts_LDADD = -lsdaudio -lm -lpthread -ldotconf -libmeci @SNDFILE_LIBS@ @glib_libs@ @gthread_libs@

sd_cicero_SOURCES = cicero.c module_utils.c module_utils.h
sd_cicero_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @gthread_libs@

sd_espeak_SOURCES = espeak.c module_utils.c module_utils.h
sd_espeak_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_espeak_LDADD = -lsdaudio -lespeak -ldotconf @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@ @EXTRA_ESPEAK_LIBS@

sd_ivona_SOURCES = ivona.c module_utils.c module_utils.h
sd_ivona_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_ivona_LDADD = -lsdaudio -ldumbtts -lpthread -ldotconf @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@

sd_dummy_SOURCES = dummy.c module_utils.c module_utils.h
sd_dummy_LDFLAGS = @RPATH@ '$(spdlibdir)'
sd_dummy_LDADD = -lsdaudio -lpthread -ldotconf @glib_libs@ @SNDFILE_LIBS@ @gthread_libs@
