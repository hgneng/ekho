#!/bin/bash

# build.sh - Run all the auto-tools
#
# Copyright (C) 2001,2002,2003 Brailcom, o.p.s, Prague 2,
# Vysehradska 3/255, 128 00, <freesoft@freesoft.cz>
#
# This is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this package; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.
#
# $Id: build.sh,v 1.5 2006-07-11 16:12:25 hanke Exp $

libtoolize=libtoolize
if [ "`uname`" = Darwin ]; then
    libtoolize=glibtoolize
fi

echo "Copying libtool macros (libtoolize)"
if ! $libtoolize --force; then
	echo "libtoolize failed!"
	exit 1
fi

echo "Building user-defined autoconf macros (aclocal)"
if ! aclocal; then
	echo "aclocal failed!"
	exit 1
fi

echo "Creating ./configure (autoconf)"
if ! autoconf; then
	echo "autoconf failed!"
	exit 1
fi

echo "Creating config.h.in (autoheader)"
if ! autoheader; then
	echo "autoheader failed!"
	exit 1
fi

echo "Checking for missing scripts (automake -a)"
if ! automake -a; then
	echo "automake -a failed!"
	exit 1
fi

echo "Creating makefiles.in (automake)"
if ! automake; then
	echo "automake failed!"
	exit 1
fi

echo 
echo "You can continue configuring and compiling Speech Dispatcher with"
echo "       ./configure && make all && make install"
