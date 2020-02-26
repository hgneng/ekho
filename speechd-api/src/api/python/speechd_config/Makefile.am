#
# Copyright (C) 2002 - 2018 Brailcom, o.p.s.
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
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

## Process this file with automake to produce Makefile.in

dist_snddata_DATA = test.wav
DESKTOP_FILES = speechd.desktop.in
desktop_DATA = $(DESKTOP_FILES:.desktop.in=.desktop)
desktopdir = $(spddesktopconforigdir)
CLEANFILES = $(desktop_DATA)

speechd.desktop: speechd.desktop.in
	$(AM_V_GEN)$(MSGFMT) --desktop --template $< -d $(top_srcdir)/po -o $@

dist_bin_SCRIPTS = spd-conf

speechd_pythondir = $(pyexecdir)/speechd_config
speechd_python_PYTHON = __init__.py
nodist_speechd_python_PYTHON = config.py paths.py

paths_edit = sed \
	-e "s:[@]spdconforigdir[@]:$(spdconforigdir):" \
	-e "s:[@]spdconfdir[@]:$(spdconfdir):" \
	-e "s:[@]snddatadir[@]:$(snddatadir):" \
	-e "s:[@]spddesktopconforigdir[@]:$(spddesktopconforigdir):"

config_edit = sed \
	-e "s:[@]GETTEXT_PACKAGE[@]:$(GETTEXT_PACKAGE):" \
	-e "s:[@]VERSION[@]:$(VERSION):" \
	-e "s:[@]localedir[@]:$(localedir):"

paths.py: Makefile
	rm -f $@
	srcdir=; \
	test -f ./$@.in || srcdir=$(srcdir)/; \
	$(paths_edit) $${srcdir}$@.in > $@

config.py: Makefile
	rm -f $@
	srcdir=; \
	test -f ./$@.in || srcdir=$(srcdir)/; \
	$(config_edit) $${srcdir}$@.in > $@

$(abs_builddir)/__init__.py:
	ln -s $(srcdir)/__init__.py .

paths.py: $(srcdir)/paths.py.in

CLEANFILES += paths.py config.py speechd.desktop

EXTRA_DIST = paths.py.in speechd.desktop.in config.py.in

if HAVE_HELP2MAN
spd-conf.1: config.py paths.py $(abs_builddir)/__init__.py
	LC_ALL=C PYTHONPATH=$(builddir)/.. PYTHONDONTWRITEBYTECODE=1 help2man -N -n "configure Speech Dispatcher and diagnose problems" --output=$@ $(srcdir)/spd-conf

dist_man1_MANS = \
	spd-conf.1
endif

CLEANFILES += $(dist_man1_MANS)

-include $(top_srcdir)/git.mk
