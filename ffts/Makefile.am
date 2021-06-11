AUTOMAKE_OPTIONS = foreign
SUBDIRS = src tests 
EXTRA_DIST=COPYRIGHT ffts.pc.in build_iphone.sh build_android.sh
ACLOCAL_AMFLAGS = -Im4 -Iinclude

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ffts.pc

if ENABLE_JNI
SUBDIRS += java
endif
