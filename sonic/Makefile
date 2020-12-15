# This file was written by Bill Cox in 2010, and is licensed under the Apache
# 2.0 license.
#
# Note that -pthread is only included so that older Linux builds will be thread
# safe.  We call malloc, and older Linux versions only linked in the thread-safe
# malloc if -pthread is specified.

# Set this to 0 if you do not want to link in spectrogram generation.
USE_SPECTROGRAM=1
SONAME=soname
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  SONAME=install_name
endif
#CFLAGS=-Wall -Wno-unused-function -g -ansi -fPIC -pthread
CFLAGS=-Wall -Wno-unused-function -O3 -ansi -fPIC -pthread
LIB_TAG=0.3.0
CC=gcc
PREFIX=/usr
LIBDIR=$(PREFIX)/lib
SRC=sonic.c
FFTLIB=
ifeq ($(USE_SPECTROGRAM), 1)
  CFLAGS+= -DSONIC_SPECTROGRAM
  SRC+= spectrogram.c
  FFTLIB=-lfftw3
endif
OBJ=$(SRC:.c=.o)

# Set this to empty if not using spectrograms.

all: sonic libsonic.so.$(LIB_TAG) libsonic.a

sonic: wave.o main.o libsonic.a
	$(CC) $(CFLAGS) -o sonic wave.o main.o libsonic.a -lm $(FFTLIB)

sonic.o: sonic.c sonic.h
	$(CC) $(CFLAGS) -c sonic.c

wave.o: wave.c wave.h
	$(CC) $(CFLAGS) -c wave.c

main.o: main.c sonic.h wave.h
	$(CC) $(CFLAGS) -c main.c

spectrogram.o: spectrogram.c sonic.h
	$(CC) $(CFLAGS) -c spectrogram.c

libsonic.so.$(LIB_TAG): $(OBJ)
	$(CC) $(CFLAGS) -shared -Wl,-$(SONAME),libsonic.so.0 $(OBJ) -o libsonic.so.$(LIB_TAG)
	ln -sf libsonic.so.$(LIB_TAG) libsonic.so
	ln -sf libsonic.so.$(LIB_TAG) libsonic.so.0

libsonic.a: $(OBJ)
	$(AR) cqs libsonic.a $(OBJ)

install: sonic libsonic.so.$(LIB_TAG) sonic.h
	install -d $(DESTDIR)$(PREFIX)/bin $(DESTDIR)$(PREFIX)/include $(DESTDIR)$(PREFIX)/lib
	install sonic $(DESTDIR)$(PREFIX)/bin
	install sonic.h $(DESTDIR)$(PREFIX)/include
	install libsonic.so.$(LIB_TAG) $(DESTDIR)$(PREFIX)/lib
	install libsonic.a $(DESTDIR)$(LIBDIR)
	ln -sf libsonic.so.$(LIB_TAG) $(DESTDIR)$(PREFIX)/lib/libsonic.so
	ln -sf libsonic.so.$(LIB_TAG) $(DESTDIR)$(PREFIX)/lib/libsonic.so.0

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/sonic
	rm -f $(DESTDIR)$(PREFIX)/include/sonic.h
	rm -f $(DESTDIR)$(PREFIX)/lib/libsonic.so.$(LIB_TAG)
	rm -f $(DESTDIR)$(PREFIX)/lib/libsonic.so
	rm -f $(DESTDIR)$(PREFIX)/lib/libsonic.so.0
	rm -f $(DESTDIR)$(LIBDIR)/libsonic.a

clean:
	rm -f *.o sonic libsonic.so* libsonic.a
