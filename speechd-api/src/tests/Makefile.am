
localedir = $(datadir)/locale
inc_local = "-I$(top_srcdir)/intl/"
c_api = $(top_builddir)/src/c/api

AM_CFLAGS = -I$(top_srcdir)/src/c/api -DLOCALEDIR=\"$(localedir)\" $(inc_local) @glib_include@ -I../audio/

bin_PROGRAMS = long_message clibrary clibrary2 run_test connection_recovery

long_message_SOURCES = long_message.c
long_message_LDADD = $(c_api)/libspeechd.la -lpthread @EXTRA_SOCKET_LIBS@

clibrary_SOURCES = clibrary.c
clibrary_LDADD = $(c_api)/libspeechd.la -lpthread @EXTRA_SOCKET_LIBS@

clibrary2_SOURCES = clibrary2.c
clibrary2_LDADD = $(c_api)/libspeechd.la -lpthread @EXTRA_SOCKET_LIBS@

connection_recovery_SOURCES = connection-recovery.c
connection_recovery_LDADD = $(c_api)/libspeechd.la -lpthread @EXTRA_SOCKET_LIBS@

run_test_SOURCES = run_test.c
run_test_LDADD = $(c_api)/libspeechd.la -lpthread @EXTRA_SOCKET_LIBS@

EXTRA_DIST= basic.test general.test keys.test priority_progress.test \
            pronunciation.test punctuation.test sound_icons.test spelling.test \
            ssml.test stop_and_pause.test voices.test yo.wav
