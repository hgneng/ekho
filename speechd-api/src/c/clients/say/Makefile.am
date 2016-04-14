
localedir = $(datadir)/locale
inc_local = "-I$(top_srcdir)/intl/"
c_api = $(top_builddir)/src/c/api

AM_CFLAGS = -DLOCALEDIR=\"$(localedir)\" $(inc_local) @glib_include@ -I$(top_srcdir)/src/c/api -DVERSION=\"@VERSION@\"

bin_PROGRAMS = spd-say
spd_say_SOURCES = say.c options.c options.h
spd_say_LDADD = $(c_api)/libspeechd.la -lpthread @EXTRA_SOCKET_LIBS@
