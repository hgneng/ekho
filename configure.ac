#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ(2.60)
AC_INIT(ekho, 8.3.1, hgneng at gmail.com)
AC_CONFIG_AUX_DIR([.])
AM_PROG_AR
AM_INIT_AUTOMAKE([-Wall foreign 1.9 tar-pax subdir-objects])
AM_CONFIG_HEADER(config.h)
AC_CONFIG_MACRO_DIR([m4])
AC_PROG_LIBTOOL

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# Checks for libraries.

# FIXME: Replace `main' with a function in `-lpthread':
AC_CHECK_LIB([pthread], [main])

# Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS([locale.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_MALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([memset setlocale strstr])

# Manually added

AC_SUBST([FESTIVAL_HEADER])
AC_SUBST([SPEECH_TOOLS_HEADER])
FESTIVAL_HEADER="" #"`find ${srcdir}/include/festival -name "*.h" -print0 -printf ' '`"
SPEECH_TOOLS_HEADER="" #"`find ${srcdir}/include/speech_tools -name "*.h" -print0 -printf ' '`"

# don't use GTK any more
LIB_GTK2=""
CFLAG_GTK2=""
AC_SUBST([LIB_GTK2])
AC_SUBST([CFLAG_GTK2])
#AC_ARG_WITH([gtk2],
#        [AS_HELP_STRING([--with-gtk2],
#            [support GTK2 interface. @<:@default=check@:>@])],
#        [],
#        [with_gtk2=check])
#AS_IF([test "x$with_gtk2" != xno],
#        [AC_CHECK_LIB([gtk-x11-2.0], [main],
#          [AC_SUBST([LIB_GTK2], ["`pkg-config --libs gtk+-2.0`"])
#           AC_SUBST([CFLAG_GTK2], ["`pkg-config --cflags gtk+-2.0`"])
#           AC_DEFINE([HAVE_GTK2], [1], [Define if you have GTK-2.0])
#          ],
#          [if test "x$with_gtk2" != xcheck; then
#            AC_MSG_FAILURE([gtk2 test failed (with-gtk2=no to disable)])
#          fi
#          ], [])])

AC_CHECK_LIB([sndfile], [main], [],
    [AC_MSG_FAILURE([sndfile test failed])],[]
)

AC_CHECK_LIB([espeak-ng], [main], [], [AC_MSG_FAILURE([espeak-ng test failed])],[])

LIB_PULSEAUDIO=""
CFLAG_PULSEAUDIO=""
AS_IF([test "x$with_pulseaudio" != xno],
        [AC_CHECK_LIB([pulse], [main],
            [AC_SUBST([LIB_PULSEAUDIO], ["`pkg-config --libs libpulse-simple`"])
            AC_SUBST([CFLAG_PULSEAUDIO], ["`pkg-config --cflags libpulse-simple`"])
            AC_DEFINE([HAVE_PULSEAUDIO], [1],
                [Define if you have libpulse])
            ],
            [AC_MSG_FAILURE(
                [pulseaudio test failed (--without-pulseaudio to disable)])],
            [])])

AC_ARG_ENABLE(speechd,
        [AC_HELP_STRING([--enable-speechd],
            [Enable speech-dispatcher API])],
        [AM_CONDITIONAL(ENABLE_SPEECHD, true)],
        [AM_CONDITIONAL(ENABLE_SPEECHD, false)]
        )
        
AC_SUBST([LIB_FESTIVAL])
AC_SUBST([CFLAG_FESTIVAL])
AC_ARG_ENABLE(festival,
        [AC_HELP_STRING([--enable-festival],
            [synthesize English through Festival])],
        [AC_CHECK_LIB([ncurses], [main],
          [
#AC_DEFINE(ENABLE_FESTIVAL,,[Enable Festival])
#LIB_FESTIVAL="${srcdir}/lib/libFestival.a ${srcdir}/lib/libestools.a ${srcdir}/lib/libestbase.a ${srcdir}/lib/libeststring.a -lncurses"
#           CFLAG_FESTIVAL="-I${srcdir}/include/festival -I${srcdir}/include/speech_tools"
          ],
          [AC_MSG_FAILURE([ncurses test failed])],
          [])

          AC_CHECK_LIB([estools], [main], [],
            [AC_MSG_FAILURE([estool test failed])],
            []
          )

          AC_CHECK_LIB([Festival], [main], [
              AC_DEFINE(ENABLE_FESTIVAL,,[Enable Festival])
              LIB_FESTIVAL="-lFestival -lestbase -lestools -leststring -lncurses"
              CFLAG_FESTIVAL="-I/usr/include/festival -I/usr/include/speech_tools"
            ],
            [
              AC_DEFINE(ENABLE_FESTIVAL,,[Enable Festival])
              LIB_FESTIVAL="${srcdir}/lib/libFestival.a ${srcdir}/lib/libestools.a ${srcdir}/lib/libestbase.a ${srcdir}/lib/libeststring.a -lncurses"
              CFLAG_FESTIVAL="-I${srcdir}/include -I${srcdir}/include/festival -I${srcdir}/include/speech_tools"
            ],
            []
          )
        ],
        [LIB_FESTIVAL=""
         CFLAG_FESTIVAL=""]
        )

CFLAG_FRISO=""
LIB_FRISO=""
AC_SUBST([CFLAG_FRISO])
AC_SUBST([LIB_FRISO])
AC_ARG_ENABLE(friso,
        [  --enable-friso],
        [AC_DEFINE(ENABLE_FRISO,,[Enable Friso])
         CFLAG_FRISO="-I${srcdir}/friso-src"
         LIB_FRISO="libfriso.a"
         AM_CONDITIONAL(ENABLE_FRISO, true)
        ],
        [AM_CONDITIONAL(ENABLE_FRISO, false)]
        )

AC_ARG_ENABLE(win32,
        [  --enable-win32],
        [AC_DEFINE(ENABLE_WIN32,,[Enable Windows])
#        LIB_FESTIVAL+=-lws2_32
        ],
        [])

#AC_ARG_WITH([vorbisenc],
#        [AS_HELP_STRING([--with-vorbisenc],
#            [support OGG audio output file format. @<:@default=check@:>@])],
#        [],
#        [with_vorbisenc=check])
#LIB_OGG=""
#AS_IF([test "x$with_vorbisenc" != xno],
#        [AC_CHECK_LIB([vorbisenc], [main],
#            [AC_SUBST([LIB_OGG], ["-lvorbisenc -lvorbis -lm -logg"])
#            AC_DEFINE([HAVE_OGG], [1],
#                [Define if you have libvorbisenc])
#            ],
#            [if test "x$with_vorbisenc" != xcheck; then
#            AC_MSG_FAILURE(
#                [--with-vorbisenc was given, but test for vorbisenc failed])
#            fi
#            ], [])])

AC_ARG_WITH([mp3lame],
        [AS_HELP_STRING([--with-mp3lame],
            [support MP3 audio output file format. @<:@default=no@:>@])],
        [],
        [with_mp3lame=no])
LIB_MP3LAME=""
AS_IF([test "x$with_mp3lame" != xno],
        [AC_CHECK_LIB([mp3lame], [main],
            [AC_SUBST([LIB_MP3LAME], ["-lmp3lame -lm"])
            AC_DEFINE([HAVE_MP3LAME], [1],
                [Define if you have libmp3lame])
            ],
            [if test "x$with_mp3lame" != xcheck; then
            AC_MSG_FAILURE(
                [--with-mp3lame was given, but test for mp3lame failed])
            fi
            ], [])])

AC_ARG_ENABLE(dist-voice,
        [  --disable-dist-voice],
        [dist_voice=false],
        [dist_voice=true])
AM_CONDITIONAL([DIST_VOICE_DATA], [test x$dist_voice = xtrue])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
