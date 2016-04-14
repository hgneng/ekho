
localedir = $(datadir)/locale
inc_local = "-I$(top_srcdir)/intl/"

am_cflags = -DLOCALEDIR=\"$(localedir)\" -I/usr/include/ $(inc_local) @glib_include@

if nas_support
NAS_FLAGS = -DWITH_NAS
NAS_LIBS = -laudio -L/usr/X11R6/lib
endif

if pulse_support
PULSE_FLAGS = -DWITH_PULSE
PULSE_LIBS = -lpulse-simple
endif

if alsa_support
ALSA_FLAGS = -DWITH_ALSA
ALSA_LIBS = -lasound
endif
if libao_support
LIBAO_FLAGS = -DWITH_LIBAO
LIBAO_LIBS = -lao
endif

if oss_support
LIBOSS_FLAGS = -DWITH_OSS
endif


EXTRA_DIST = alsa.c libao.c oss.c nas.c pulse.c

AM_CFLAGS = $(am_cflags) $(NAS_FLAGS) $(PULSE_FLAGS) $(ALSA_FLAGS) $(LIBAO_FLAGS) $(LIBOSS_FLAGS)
libsdaudio_la_LDFLAGS = -version-info @LIB_SDAUDIO_CURRENT@:@LIB_SDAUDIO_REVISION@:@LIB_SDAUDIO_AGE@ -lpthread $(NAS_LIBS) $(PULSE_LIBS) $(ALSA_LIBS) $(LIBAO_LIBS)

spdlib_LTLIBRARIES = libsdaudio.la

libsdaudio_la_SOURCES = spd_audio.c spd_audio.h
