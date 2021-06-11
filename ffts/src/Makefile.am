

lib_LTLIBRARIES = libffts.la

libffts_la_SOURCES = ffts.c ffts_nd.c ffts_real.c ffts_real_nd.c ffts_transpose.c ffts_trig.c ffts_static.c
libffts_la_SOURCES += codegen.h codegen_arm.h codegen_sse.h ffts.h ffts_nd.h ffts_real.h ffts_real_nd.h ffts_small.h ffts_static.h macros-alpha.h macros-altivec.h macros-neon.h macros-sse.h macros.h neon.h neon_float.h patterns.h types.h vfp.h

if DYNAMIC_DISABLED
libffts_la_SOURCES += ffts_static.c
else
libffts_la_SOURCES += codegen.c
endif

libffts_includedir=$(includedir)/ffts
libffts_include_HEADERS = ../include/ffts.h

AM_CFLAGS = -I$(top_srcdir)/include -DAUTOTOOLS_BUILD=yes

if HAVE_VFP
libffts_la_SOURCES += vfp.s 
else
if HAVE_NEON

libffts_la_SOURCES += neon.s

if DYNAMIC_DISABLED
libffts_la_SOURCES += neon_static_f.s neon_static_i.s
endif

else 
if HAVE_SSE
libffts_la_SOURCES += macros-sse.h
endif
endif
endif
