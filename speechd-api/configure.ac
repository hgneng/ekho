#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Copyright (C) 2002-2013 Brailcom, o.p.s
# Copyright (C) 2009-2017 Luke Yelavich <luke.yelavich@canonical.com>
# Copyright (C) 2009 Willie Walker <william.walker@sun.com>
# Copyright (C) 2010-2011 William Hubbs <w.d.hubbs@gmail.com>
# Copyright (C) 2010-2011 Andrei Kholodnyi <andrei.kholodnyi@gmail.com>
# Copyright (C) 2010 Boris Dusek <dusek@brailcom.org>
# Copyright (C) 2010 Christopher Brannon <chris@the-brannons.com>
# Copyright (C) 2011 Trevor Saunders <trev.saunders@gmail.com>
# Copyright (C) 2012 William Jon McCann <jmccann@redhat.com>
# Copyright (C) 2017 Colomban Wendling <cwendling@hypra.fr>
# Copyright (C) 2017 Jan Tojnar <jtojnar@gmail.com>
# Copyright (C) 2018 Raphaël POITEVIN <rpoitevin@hypra.fr>
# Copyright (C) 2018-2019 Samuel Thibault <samuel.thibault@ens-lyon.org>
# Copyright (C) 2018 Alan Coopersmith <alan.coopersmith@oracle.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License for more details (file
# COPYING in the root directory).
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

AC_PREREQ([2.63])
AC_INIT([speech-dispatcher], [m4_esyscmd(./git-version-gen .tarball-version)], [speechd-discuss@nongnu.org])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([foreign info-in-builddir])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES(yes)])
LT_PREREQ([2.2])
LT_INIT([dlopen])
AC_CONFIG_SRCDIR([src/server/speechd.c])
AC_CONFIG_HEADERS([config.h])
AM_GNU_GETTEXT_VERSION([0.19.8])
AM_GNU_GETTEXT([external])
if test "$ac_cv_path_MSGFMT" = ":"
then
	AC_MSG_FAILURE([msgfmt missing from the gettext package])
fi

# Split version number
MAJOR_VERSION="$(./split-version.sh -ma)"
MINOR_VERSION="$(./split-version.sh -mi)"
MICRO_VERSION="$(./split-version.sh -mc)"
AC_SUBST([MAJOR_VERSION])
AC_SUBST([MINOR_VERSION])
AC_SUBST([MICRO_VERSION])

# Config test suite
AC_CONFIG_TESTDIR(src/tests)

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_CHECK_PROGS([HELP2MAN], [help2man])
AM_CONDITIONAL([HAVE_HELP2MAN], [test "x$HELP2MAN" != "x"])

# Checks for libraries.
AC_SEARCH_LIBS([sqrt], [m], [],
	[AC_MSG_FAILURE([Math library missing])])
AC_SEARCH_LIBS([pthread_create], [pthread], [],
	[AC_MSG_FAILURE([Threads library missing])])
AC_SEARCH_LIBS([lt_dlopen], [ltdl], [],
	[AC_MSG_FAILURE([ltdl library missing])])

PKG_CHECK_MODULES([GLIB], [glib-2.0 >= 2.36])
AC_SUBST([GLIB_CFLAGS])
AC_SUBST([GLIB_LIBS])

PKG_CHECK_MODULES([GTHREAD], [gthread-2.0])
AC_SUBST([GTHREAD_CFLAGS])
AC_SUBST([GTHREAD_LIBS])

PKG_CHECK_MODULES([GMODULE], [gmodule-2.0])
AC_SUBST([GMODULE_CFLAGS])
AC_SUBST([GMODULE_LIBS])

PKG_CHECK_MODULES([DOTCONF], [dotconf >= 1.3])
AC_SUBST([DOTCONF_CFLAGS])
AC_SUBST([DOTCONF_LIBS])

PKG_CHECK_MODULES([SNDFILE], [sndfile >= 1.0.2])
AC_SUBST([SNDFILE_CFLAGS])
AC_SUBST([SNDFILE_LIBS])

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h fcntl.h langinfo.h limits.h netdb.h])
AC_CHECK_HEADERS([netinet/in.h stddef.h stdlib.h string.h sys/filio.h])
AC_CHECK_HEADERS([sys/ioctl.h sys/socket.h sys/time.h unistd.h wchar.h wctype.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT32_T

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([daemon dup2 gethostbyname getline gettimeofday memmove memset])
AC_CHECK_FUNCS([mkdir select socket strcasecmp strcasestr strchr strcspn strdup])
AC_CHECK_FUNCS([strerror strncasecmp strndup strstr strtol])

# Extra libraries for sockets and espeak added by Willie Walker
# based upon how SunStudio compilers and Solaris libraries work.
# Also conditionally set other compiler/linker flags.
#

# SVR4, including Solaris before 11.4, hides these in libraries other than libc
AC_CHECK_LIB([c], [socket], [],
	[AC_CHECK_LIB([socket], [socket], [EXTRA_SOCKET_LIBS="-lsocket"])])
AC_CHECK_LIB([c], [gethostbyname], [],
	[AC_CHECK_LIB([nsl], [gethostbyname],
		[EXTRA_SOCKET_LIBS="${EXTRA_SOCKET_LIBS} -lnsl"])])

if test "$GCC" = yes; then
	ERROR_CFLAGS="-Wall"
	RDYNAMIC="-rdynamic"
else
	EXTRA_ESPEAK_LIBS="-lCstd -lCrun"
	ERROR_CFLAGS="-errwarn=%all -errtags=yes -erroff=E_STATEMENT_NOT_REACHED"
	RDYNAMIC=""
fi
AC_SUBST([EXTRA_ESPEAK_LIBS])
AC_SUBST([EXTRA_SOCKET_LIBS])
AC_SUBST([ERROR_CFLAGS])
AC_SUBST([RDYNAMIC])

# Check for python support.
# Python 3 or greater required
AC_ARG_ENABLE([python],
	[AS_HELP_STRING([--disable-python], [do not install python bindings])],
	[],
	[enable_python=check])
AS_IF([test $enable_python != "no"],
	[AM_PATH_PYTHON([3.0],
		[enable_python=yes],
		[AS_IF([test $enable_python = "yes"],
			[AC_MSG_FAILURE([python 3 or greater is not available])])])])
AM_CONDITIONAL([HAVE_PYTHON], [test $enable_python = "yes"])

output_modules="cicero dummy festival generic"
# checks for output modules
# check for espeak support
AC_ARG_WITH([espeak],
	[AS_HELP_STRING([--with-espeak], [Compile with espeak support])],
	[],
	[with_espeak=check])
AS_IF([test $with_espeak != "no"],
	[AC_CHECK_LIB([espeak], [espeak_Synth],
		[with_espeak="yes"],
		[AS_IF([test $with_espeak = "yes"],
			[AC_MSG_FAILURE([espeak is not available])])],
		[$EXTRA_ESPEAK_LIBS])])
AM_CONDITIONAL([espeak_support], [test $with_espeak = "yes"])
AS_IF([test $with_espeak = "yes"], [output_modules="${output_modules} espeak"])

# check for espeak-ng support
AC_ARG_WITH([espeak_ng],
	[AS_HELP_STRING([--with-espeak-ng], [Compile with espeak-ng support])],
	[],
	[with_espeak_ng=check])
AS_IF([test $with_espeak_ng != "no"],
	[PKG_CHECK_MODULES([ESPEAK_NG], [espeak-ng],
		[with_espeak_ng="yes"],
		[AS_IF([test $with_espeak_ng = "yes"],
			[AC_MSG_FAILURE([espeak-ng is not available])])])])
AM_CONDITIONAL([espeak_ng_support], [test $with_espeak_ng = "yes"])
AC_SUBST([ESPEAK_NG_CFLAGS])
AC_SUBST([ESPEAK_NG_LIBS])
AS_IF([test $with_espeak_ng = "yes"], [output_modules="${output_modules} espeak-ng"])

# check for flite support
AC_ARG_WITH([flite],
	[AS_HELP_STRING([--with-flite], [include Festival Lite (Flite) support])],
	[],
	[with_flite=check])
AS_IF([test $with_flite != "no"],
	[AC_CHECK_LIB([flite], [flite_init],
		[flite_basic="-lflite"],
		[AS_IF([test $with_flite = "yes"],
			[AC_MSG_FAILURE([libflite is not available])])],
		[-lm])
	AC_CHECK_LIB([flite_usenglish], [usenglish_init],
		[flite_basic="-lflite_usenglish ${flite_basic}"],
		[AS_IF([test $with_flite = "yes"],
			[AC_MSG_FAILURE([libflite_usenglish is not available])])],
		[-lflite -lm])
	AC_CHECK_LIB([flite_cmulex], [cmu_lex_init],
		[flite_basic="-lflite_cmulex ${flite_basic}"],
		[AS_IF([test $with_flite = "yes"],
			[AC_MSG_FAILURE([libflite_cmulex is not available])])],
		[-lflite -lm])
	AC_CHECK_LIB([flite_cmu_us_kal16], [register_cmu_us_kal16],
		[with_flite=yes;
		flite_kal="-lflite_cmu_us_kal16";
		AC_DEFINE([HAVE_REGISTER_CMU_US_KAL16], [1],
			[Use cmu_register_us_kal16 to register the 16-bit voice.])],
		[AC_CHECK_LIB([flite_cmu_us_kal16], [register_cmu_us_kal],
			[with_flite=yes;
			flite_kal="-lflite_cmu_us_kal16"],
			[AC_CHECK_LIB([flite_cmu_us_kal], [register_cmu_us_kal],
				[with_flite=yes;
				flite_kal="-lflite_cmu_us_kal"],
				[AS_IF([test $with_flite = "yes"],
					[AC_MSG_FAILURE([no flite voices found])])],
				[$flite_basic -lm])],
			[$flite_basic -lm])],
		[$flite_basic -lm])])
AM_CONDITIONAL([flite_support], [test $with_flite = "yes"])
AC_SUBST([flite_basic])
AC_SUBST([flite_kal])
AS_IF([test $with_flite = "yes"], [output_modules="${output_modules} flite"])

# check for IBMTTS support
AC_ARG_WITH([ibmtts],
	[AS_HELP_STRING([--with-ibmtts[[=yes|no|shim]]], [include IBM TTS support])],
	[],
	[with_ibmtts=check])
AS_IF([test $with_ibmtts != "no"],
	[AC_CHECK_LIB([ibmeci], [eciStop],
		[with_ibmtts=yes;
		ibmtts_include="-I/opt/IBM/ibmtts/inc/"],
		[AS_IF([test $with_ibmtts = "yes"],
			[AC_MSG_FAILURE([IBMTTS is not available])])
		 with_ibmtts=shim])])
if test "$with_ibmtts" = shim -a $enable_shared = no; then
  with_ibmtts=no
fi
AM_CONDITIONAL([ibmtts_support], [test $with_ibmtts != no])
AM_CONDITIONAL([ibmtts_shim], [test $with_ibmtts = shim])
AC_SUBST([ibmtts_include])
AS_IF([test $with_ibmtts != no], [output_modules="${output_modules} ibmtts"])

# check for ivona support
AC_ARG_WITH([ivona],
	[AS_HELP_STRING([--with-ivona], [include ivona support])],
	[],
	[with_ivona=check])
AS_IF([test $with_ivona != "no"],
	[AC_CHECK_LIB([dumbtts], [dumbtts_TTSInit],
		[with_ivona=yes],
		[AS_IF([test $with_ivona = "yes"],
			[AC_MSG_FAILURE([ivona is not available])])])])
AM_CONDITIONAL([ivona_support], [test $with_ivona = "yes"])
AS_IF([test $with_ivona = "yes"], [output_modules="${output_modules} ivona"])

# check for svox pico support
AC_ARG_WITH([pico],
	[AS_HELP_STRING([--with-pico], [include SVOX pico support])],
	[],
	[with_pico=check])
AS_IF([test $with_pico != "no"],
	[AC_CHECK_LIB([ttspico], [pico_initialize],
		[with_pico=yes],
		[AS_IF([test $with_pico = "yes"],
			[AC_MSG_FAILURE([SVOX pico is not available])])])])
AM_CONDITIONAL([pico_support], [test $with_pico = "yes"])
AS_IF([test $with_pico = "yes"], [output_modules="${output_modules} pico"])

# check for Voxygen Baratinoo support
AC_ARG_WITH([baratinoo],
	[AS_HELP_STRING([--with-baratinoo[[=yes|no|shim]]], [include Voxygen Baratinoo support])],
	[],
	[with_baratinoo=check])
AS_IF([! test "$with_baratinoo" = no],
	[AC_CHECK_LIB([baratinoo], [BCinitlib],
		[with_baratinoo=yes],
		[AS_IF([test "$with_baratinoo" = yes],
			[AC_MSG_FAILURE([Voxygen Baratinoo is not available])])
		 with_baratinoo=shim])])
if test "$with_baratinoo" = shim -a $enable_shared = no; then
  with_baratinoo=no
fi
AM_CONDITIONAL([baratinoo_support], [test "$with_baratinoo" != no])
AM_CONDITIONAL([baratinoo_shim], [test "$with_baratinoo" = shim])
AS_IF([test "$with_baratinoo" != no], [output_modules="${output_modules} baratinoo"])

# check for kali support
AC_ARG_WITH([kali],
	[AS_HELP_STRING([--with-kali[[=yes|no|shim]]], [include Kali support])],
	[],
	[with_kali=check])
AS_IF([test $with_kali != "no"],
	[AC_CHECK_LIB([Kali], [initKali],
		[with_kali=yes],
		[AS_IF([test $with_kali = "yes"],
			[AC_MSG_FAILURE([Kali is not available])])
		 with_kali=shim],
		[-lKGlobal -lKTrans -lKParle -lKAnalyse])])
if test "$with_kali" = shim -a $enable_shared = no; then
  with_kali=no
fi
AM_CONDITIONAL([kali_support], [test $with_kali != no])
AM_CONDITIONAL([kali_shim], [test $with_kali = shim])
AS_IF([test $with_kali != no], [output_modules="${output_modules} kali"])

audio_dlopen_modules='-dlopen force'

# clear default audio method
default_audio_method=""
audio_methods=""

# checks for audio subsystems
# check for pulseaudio support
AC_ARG_WITH([pulse],
	[AS_HELP_STRING([--with-pulse], [include pulseaudio support])],
	[],
	[with_pulse=check])
AS_IF([test $with_pulse != "no"],
	[PKG_CHECK_MODULES([PULSE], [libpulse-simple],
		[with_pulse=yes
		AS_IF([test -z "$default_audio_method"],
			[default_audio_method=pulse])
		audio_dlopen_modules="$audio_dlopen_modules -dlopen ../audio/spd_pulse.la"],
		[AS_IF([test $with_pulse = "yes"],
			[AC_MSG_FAILURE([pulseaudio is not available])])])])
AM_CONDITIONAL([pulse_support], [test $with_pulse = "yes"])
AC_SUBST([PULSE_CFLAGS])
AC_SUBST([PULSE_LIBS])
AS_IF([test $with_pulse = "yes"], [audio_methods="${audio_methods} pulse"])

# check for libao support
AC_ARG_WITH([libao],
	[AS_HELP_STRING([--with-libao], [include libao support])],
	[],
	[with_libao=check])
AS_IF([test $with_libao != "no"],
	[PKG_CHECK_MODULES([LIBAO], [ao],
		[with_libao=yes
		AS_IF([test -z "$default_audio_method"],
			[default_audio_method=libao])
		audio_dlopen_modules="$audio_dlopen_modules -dlopen ../audio/spd_libao.la"],
		[AS_IF([test $with_libao = yes],
			[AC_MSG_FAILURE([libao is not available])])])])
AM_CONDITIONAL([libao_support], [test $with_libao = "yes"])
AC_SUBST([LIBAO_CFLAGS])
AC_SUBST([LIBAO_LIBS])
AS_IF([test $with_libao = "yes"], [audio_methods="${audio_methods} libao"])

# check for alsa support
AC_ARG_WITH([alsa],
	[AS_HELP_STRING([--with-alsa], [include ALSA support])],
	[],
	[with_alsa=check])
AS_IF([test $with_alsa != "no"],
	[PKG_CHECK_MODULES([ALSA], [alsa],
		[with_alsa=yes
		AS_IF([test -z "$default_audio_method"],
			[default_audio_method=alsa])
		audio_dlopen_modules="$audio_dlopen_modules -dlopen ../audio/spd_alsa.la"],
		[AS_IF([test $with_alsa = "yes"],
			[AC_MSG_FAILURE([ALSA is not available])])])])
AM_CONDITIONAL([alsa_support], [test $with_alsa = "yes"])
AC_SUBST([ALSA_CFLAGS])
AC_SUBST([ALSA_LIBS])
AS_IF([test $with_alsa = "yes"], [audio_methods="${audio_methods} alsa"])

#check for OSS support
AC_ARG_WITH([oss],
	[AS_HELP_STRING([--with-oss], [include oss support])],
	[],
	[with_oss=check])
AS_IF([test $with_oss != "no"],
	[AC_CHECK_HEADER([sys/soundcard.h],
		[with_oss=yes
		AS_IF([test -z "$default_audio_method"],
			[default_audio_method=oss])
		audio_dlopen_modules="$audio_dlopen_modules -dlopen ../audio/spd_oss.la"],
		[AS_IF([test $with_oss = "yes"],
			[AC_MSG_FAILURE([oss is not available])])])])
AM_CONDITIONAL([oss_support], [test $with_oss = "yes"])
AS_IF([test $with_oss = "yes"], [audio_methods="${audio_methods} oss"])

# check for NAS support
AC_ARG_WITH([nas],
	[AS_HELP_STRING([--with-nas], [include network audio system (nas) support])],
	[],
	[with_nas=check])
AS_IF([test $with_nas != "no"],
	[AC_CHECK_LIB([audio], [AuOpenServer],
		[with_nas=yes
		AS_IF([test -z "$default_audio_method"],
			[default_audio_method=nas])
		audio_dlopen_modules="$audio_dlopen_modules -dlopen ../audio/spd_nas.la";
		NAS_LIBS="-L/usr/X11R6/lib -lXau -laudio"],
		[AS_IF([test $with_nas = "yes"],
			[AC_MSG_FAILURE([nas is not available])])],
	[-L/usr/X11R6/lib -lXau])])
AM_CONDITIONAL([nas_support], [test $with_nas = "yes"])
AC_SUBST([NAS_LIBS])
AS_IF([test $with_nas = "yes"], [audio_methods="${audio_methods} nas"])

AC_ARG_WITH([default-audio-method],
	[AS_HELP_STRING([--with-default-audio-method=<name>],
		[defines default audio method (default - first discovered)])],
	[default_audio_method="$withval"],
	[])
AC_SUBST([default_audio_method])

# We'll build these modules:
AC_SUBST([audio_dlopen_modules])

# current, age and revision values for libspeechd.
LIB_SPD_CURRENT=8 # Current main version (increment on every API change -- incompatible AND extensions)
LIB_SPD_REVISION=0 # Current minor version (increment on every implementation change)
LIB_SPD_AGE=6 # Number of backward compatible main versions (LIB_SPD_CURRENT incrementations since last incompatible)
AC_SUBST([LIB_SPD_CURRENT])
AC_SUBST([LIB_SPD_REVISION])
AC_SUBST([LIB_SPD_AGE])

# Setup GETTEXT
GETTEXT_PACKAGE=speech-dispatcher
AC_SUBST([GETTEXT_PACKAGE])

# Paths for configuration files:
spdconfdir='${sysconfdir}/speech-dispatcher'
AC_SUBST([spdconfdir])
clientconfdir='${spdconfdir}/clients'
AC_SUBST([clientconfdir])
moduleconfdir='${spdconfdir}/modules'
AC_SUBST([moduleconfdir])

# Paths for read-only copy of configuration files:
spdconforigdir='${datadir}/speech-dispatcher/conf'
AC_SUBST([spdconforigdir])
clientconforigdir='${spdconforigdir}/clients'
AC_SUBST([clientconforigdir])
spddesktopconforigdir='${spdconforigdir}/desktop'
AC_SUBST([spddesktopconforigdir])
moduleconforigdir='${spdconforigdir}/modules'
AC_SUBST([moduleconforigdir])

# Path for sound files:
snddatadir='${datadir}/sounds/speech-dispatcher'
AC_SUBST([snddatadir])

# Path for locale files:
localedatadir='${datadir}/speech-dispatcher/locale'
AC_SUBST([localedatadir])

# Path for module binaries:
AC_ARG_WITH([module-bindir],
	AS_HELP_STRING([--with-module-bindir],[Directory where to install speech-dispatcher modules (defaults to ${libdir}/speech-dispatcher-modules)]),
	[modulebindir=$withval], [modulebindir='${libdir}/speech-dispatcher-modules'])

AC_SUBST([modulebindir])

# Path for speech-dispatcher libraries:
spdlibdir='${libdir}/speech-dispatcher'
AC_SUBST([spdlibdir])

# Path for audio libraries and plugins:
audiodir="$spdlibdir"
AC_SUBST([audiodir])

# Path for speech-dispatcher include files:
spdincludedir=${includedir}/speech-dispatcher
AC_SUBST([spdincludedir])
includedir=${spdincludedir}
AC_SUBST([includedir])

# support for systemd unit files
AC_ARG_WITH([systemdsystemunitdir],
        AS_HELP_STRING([--with-systemdsystemunitdir=DIR], [Directory for systemd service files]),
        [], [with_systemdsystemunitdir=$($PKG_CONFIG --variable=systemdsystemunitdir systemd)])
if test "x$with_systemdsystemunitdir" != xno; then
        AC_SUBST([systemdsystemunitdir], [$with_systemdsystemunitdir])
fi
AM_CONDITIONAL(HAVE_SYSTEMD, [test -n "$with_systemdsystemunitdir" -a "x$with_systemdsystemunitdir" != xno ])

AC_CONFIG_FILES([Makefile
                 speech-dispatcher.pc
                 config/Makefile
                 config/clients/Makefile
                 config/modules/Makefile
                 doc/Makefile
                 include/Makefile
                 locale/Makefile
                 po/Makevars
                 po/Makefile.in
                 src/Makefile
                 src/api/Makefile
                 src/api/c/Makefile
                 src/api/python/Makefile
                 src/api/python/speechd/Makefile
                 src/api/python/speechd_config/Makefile
                 src/audio/Makefile
                 src/clients/Makefile
                 src/clients/say/Makefile
                 src/clients/spdsend/Makefile
                 src/common/Makefile
                 src/modules/Makefile
                 src/server/Makefile
                 src/tests/Makefile])
AC_OUTPUT
AC_MSG_NOTICE([output modules to be built are $output_modules])
AC_MSG_NOTICE([audio methods to be built are $audio_methods])
AC_MSG_NOTICE([Default audio method is $default_audio_method])
