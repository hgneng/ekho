# Makefile for speechd-guile

# Copyright (C) 2004 Brailcom, o.p.s.
#
# Author: Milan Zamazal <pdm@brailcom.org>
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


prefix = /usr/local
INSTALL_PATH = $(prefix)/share/guile/site

CC = gcc
CFLAGS = -g -Wall

INSTALL_PROGRAM = install -m 755
INSTALL_DATA = install -m 644

PROGRAM = gssip

.PHONY: all install install-strip uninstall clean distclean mostlyclean \
	maintainer-clean TAGS info dvi dist check

all: $(PROGRAM).so $(PROGRAM).scm

$(PROGRAM).so: $(PROGRAM).c $(PROGRAM).x $(PROGRAM).h
	$(CC) $(CFLAGS) -shared -fPIC -o $@ $(PROGRAM).c -lguile -lspeechd

%.x: %.c
	guile-snarf -o $@ $<

%.scm: %.scm.in
	sed 's/%%path%%/$(subst /,\/,$(INSTALL_PATH))/' $< > $@

install: all
	mkdir -p $(INSTALL_PATH)
	$(INSTALL_PROGRAM) $(PROGRAM).so $(INSTALL_PATH)
	$(INSTALL_DATA) $(PROGRAM).scm $(INSTALL_PATH)

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

uninstall:

mostlyclean:
	rm -f *.scm *.so *.x

clean: mostlyclean

distclean: clean

maintainer-clean: distclean

TAGS:

info:

dvi:

dist:

check:

