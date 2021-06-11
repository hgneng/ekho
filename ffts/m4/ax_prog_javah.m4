# ===========================================================================
#       http://www.gnu.org/software/autoconf-archive/ax_prog_javah.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_PROG_JAVAH
#
# DESCRIPTION
#
#   AX_PROG_JAVAH tests the availability of the javah header generator and
#   looks for the jni.h header file. If available, JAVAH is set to the full
#   path of javah and CPPFLAGS is updated accordingly.
#
# LICENSE
#
#   Copyright (c) 2008 Luc Maisonobe <luc@spaceroots.org>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 5

AU_ALIAS([AC_PROG_JAVAH], [AX_PROG_JAVAH])
AC_DEFUN([AX_PROG_JAVAH],[
AC_REQUIRE([AC_CANONICAL_SYSTEM])dnl
AC_REQUIRE([AC_PROG_CPP])dnl
AC_PATH_PROG(JAVAH,javah)
if test x"`eval 'echo $ac_cv_path_JAVAH'`" != x ; then
  AC_TRY_CPP([#include <jni.h>],,[
    ac_save_CPPFLAGS="$CPPFLAGS"
changequote(, )dnl
    ac_dir=`echo $ac_cv_path_JAVAH | sed 's,\(.*\)/[^/]*/[^/]*$,\1/include,'`
    ac_machdep=`echo $build_os | sed 's,[-0-9].*,,' | sed 's,cygwin,win32,'`
changequote([, ])dnl
    CPPFLAGS="$ac_save_CPPFLAGS -I$ac_dir -I$ac_dir/$ac_machdep"
    AC_TRY_CPP([#include <jni.h>],
               ac_save_CPPFLAGS="$CPPFLAGS",
               AC_MSG_WARN([unable to include <jni.h>]))
    CPPFLAGS="$ac_save_CPPFLAGS"])
fi])
