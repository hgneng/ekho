#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT(ffts, 0.7, amb@anthonix.com)
AM_INIT_AUTOMAKE(ffts, 0.7)

AC_CONFIG_MACRO_DIR([m4])

# AC_CONFIG_SRCDIR([include/common.h])
AC_CONFIG_HEADERS([config.h])

AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
#AX_COMPILER_VENDOR
LT_INIT([disable-shared])
AM_PROG_AS
#CXX="clang++"
#CXXFLAGS="$CXXFLAGS -stdlib=libc++"

#SFFT_AR="/usr/bin/ar"
#SFFT_CFLAGS="$CFLAGS"
#SFFT_CC="$CC"
AC_ARG_ENABLE(dynamic-code, [AC_HELP_STRING([--enable-dynamic-code],[dynamically generate code])], sfft_dynamic=$enableval, sfft_dynamic=yes)
if test "$sfft_dynamic" = "no"; then
	AC_DEFINE(DYNAMIC_DISABLED,1,[Define to disable dynamic code generation.])
fi
AM_CONDITIONAL(DYNAMIC_DISABLED, test "$sfft_dynamic" = "no")

AC_ARG_ENABLE(single, [AC_HELP_STRING([--enable-single],[compile single-precision library])], sfft_single=$enableval, sfft_single=no)
if test "$sfft_single" = "yes"; then
	AC_DEFINE(FFTS_PREC_SINGLE,1,[Define to FFT in single precision.])
fi
if test "$sfft_single" = "no"; then
	AC_DEFINE(FFTS_PREC_SINGLE,0,[Define to FFT in single precision.])
fi

AC_ARG_ENABLE(sse, [AC_HELP_STRING([--enable-sse],[enable SSE extensions])], have_sse=$enableval, have_sse=no)
if test "$have_sse" = "yes"; then
	SIMD=sse
	AC_DEFINE(HAVE_SSE,1,[Define to FFT with SSE.])
fi
AM_CONDITIONAL(HAVE_SSE, test "$have_sse" = "yes")

AC_ARG_ENABLE(neon, [AC_HELP_STRING([--enable-neon],[enable NEON extensions])], have_neon=$enableval, have_neon=no)
if test "$have_neon" = "yes"; then
	AC_DEFINE(HAVE_NEON,1,[Define to FFT with ARM NEON.])
fi
AM_CONDITIONAL(HAVE_NEON, test "$have_neon" = "yes")

AC_ARG_ENABLE(vfp, [AC_HELP_STRING([--enable-vfp],[enable VFP extensions])], have_vfp=$enableval, have_vfp=no)
if test "$have_vfp" = "yes"; then
	AC_DEFINE(HAVE_VFP,1,[Define to FFT with ARM VFP.])
fi
AM_CONDITIONAL(HAVE_VFP, test "$have_vfp" = "yes")

AC_ARG_WITH(float-abi, [AS_HELP_STRING([--with-float-abi=ABI],[set float abi for arm, hard or softfp (default is softfp)])],
		       float_abi=$withval, float_abi=softfp)

AC_ARG_ENABLE(jni, [AC_HELP_STRING([--enable-jni],[enable JNI binding])], have_jni=$enableval, have_jni=no)
if test "$have_jni" = "yes"; then
    # Java stuff
    AX_JAVA_OPTIONS
    AC_CHECK_JAVA_HOME
    AC_CHECK_CLASSPATH
    AC_PROG_JAVAC
    # blah this whinges about something
    #AC_PROG_JAVAH
    AC_PROG_JAR
    AX_JNI_INCLUDE_DIR
    for JNI_INCLUDE_DIR in $JNI_INCLUDE_DIRS
    do
          JNI_CPPFLAGS="$JNI_CPPFLAGS -I$JNI_INCLUDE_DIR"
    done
    AC_SUBST(JNI_CPPFLAGS, [$JNI_CPPFLAGS])

    AC_DEFINE(ENABLE_JNI,1,[JNI being built.])
fi
AM_CONDITIONAL(ENABLE_JNI, test "$have_jni" = "yes")

fpu=""
AS_IF([test "$have_vfp" = "yes"],[fpu="-mfpu=vfp"],
      [test "$have_neon" = "yes"],[fpu="-mfpu=neon"],
      [])

AC_MSG_NOTICE([host is "${host}"])
case "${host}" in
	arm* )
		CFLAGS="$CFLAGS -mfloat-abi=${float_abi} ${fpu} -std=c99"
		CCASFLAGS="$CCASFLAGS -mfloat-abi=${float_abi} ${fpu}"
		;;
	*)
		;;
esac

#if test "$ord_sr" = "no"; then
#	AC_DEFINE(SFFT_ORD_SR,0,[Define to enable ordinary split radix.])
#fi

# Checks for libraries.
AC_CHECK_LIB([m], [cos])
AC_CHECK_DECLS([posix_memalign,
                memalign],,,
               [#define _XOPEN_SOURCE 600
                #include <stdlib.h>
                #include <malloc.h>])

# Checks for header files.
AC_CHECK_HEADERS([malloc.h stddef.h stdint.h stdlib.h string.h sys/mman.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT32_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_UINT64_T
AC_PROG_CC_STDC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_LIBTOOL

# Checks for library functions.
#AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday pow])


AC_CONFIG_FILES([Makefile
                 src/Makefile
                 tests/Makefile
		 ffts.pc
		 java/Makefile
                 ])
AC_OUTPUT
