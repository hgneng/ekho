
# TODO: the ax_prog_javah thing doesn't work so this
# requires javah in the path

if ENABLE_JNI
JAVA_SRC=$(shell find $(srcdir)/src -name '*.java')

BUILT_SOURCES = nz_ac_waikato_ffts_FFTS.h

all: ffts.jar

classes ffts.jar: $(JAVA_SRC)
	-rm -rf classes
	mkdir classes
	$(JAVAC) -d classes -sourcepath src $(JAVA_SRC)
	$(JAR) -cf ffts.jar -C classes .

lib_LTLIBRARIES = libffts_jni.la
libffts_jni_la_SOURCES = jni/ffts_jni.c
nodist_include_HEADERS = nz_ac_waikato_ffts_FFTS.h
libffts_jni_la_LIBADD = $(top_builddir)/src/libffts.la
libffts_jni_la_CFLAGS = @JNI_CPPFLAGS@ $(AM_CFLAGS) -I$(top_srcdir)/include
libffts_jni_la_LDFLAGS = -shared

pkgdata_DATA = ffts.jar

nz_ac_waikato_ffts_FFTS.h: classes
	javah -cp $< nz.ac.waikato.ffts.FFTS

CLEANFILES=ffts.jar nz_ac_waikato_ffts_FFTS.h
clean-local:
	-rm -rf classes
endif
