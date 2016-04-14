#CFLAGS=-Wall -g -ansi -fPIC -pthread
CFLAGS=-Wall -O2 -ansi -fPIC -pthread
LIB_TAG=0.1.10
CC=gcc
PREFIX=/usr/local

all: sonic libsonic.so.$(LIB_TAG)

sonic: wave.o main.o libsonic.so.$(LIB_TAG)
	$(CC) $(CFLAGS) -lsndfile libsonic.so.$(LIB_TAG) -o sonic wave.o main.o

sonic.o: sonic.c sonic.h
	$(CC) $(CFLAGS) -c sonic.c

wave.o: wave.c wave.h
	$(CC) $(CFLAGS) -c wave.c

main.o: main.c sonic.h wave.h
	$(CC) $(CFLAGS) -c main.c

libsonic.so.$(LIB_TAG): sonic.o
	$(CC) $(CFLAGS) -shared -Wl,-soname,libsonic.so.0 sonic.o -o libsonic.so.$(LIB_TAG)
	ln -sf libsonic.so.$(LIB_TAG) libsonic.so
	ln -sf libsonic.so.$(LIB_TAG) libsonic.so.0

install: sonic libsonic.so.$(LIB_TAG) sonic.h
	install -d $(DESTDIR)$(PREFIX)/bin $(DESTDIR)$(PREFIX)/include $(DESTDIR)$(PREFIX)/lib
	install sonic $(DESTDIR)$(PREFIX)/bin
	install sonic.h $(DESTDIR)$(PREFIX)/include
	install libsonic.so.$(LIB_TAG) $(DESTDIR)$(PREFIX)/lib
	ln -sf libsonic.so.$(LIB_TAG) $(DESTDIR)$(PREFIX)/lib/libsonic.so
	ln -sf libsonic.so.$(LIB_TAG) $(DESTDIR)$(PREFIX)/lib/libsonic.so.0

uninstall: 
	rm -f $(DESTDIR)$(PREFIX)/bin/sonic 
	rm -f $(DESTDIR)$(PREFIX)/include/sonic.h
	rm -f $(DESTDIR)$(PREFIX)/lib/libsonic.so.$(LIB_TAG)
	rm -f $(DESTDIR)$(PREFIX)/lib/libsonic.so
	rm -f $(DESTDIR)$(PREFIX)/lib/libsonic.so.0

clean:
	rm -f *.o sonic libsonic.so* version
