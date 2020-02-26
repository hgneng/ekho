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

speechd_pythondir = $(pyexecdir)/speechd
speechd_python_PYTHON = __init__.py _test.py client.py

nodist_speechd_python_PYTHON = paths.py

edit = sed  \
	-e 's:@bindir[@]:$(bindir):g'

paths.py: Makefile
	rm -f $@
	srcdir=; \
	test -f ./$@.in || srcdir=$(srcdir)/; \
	$(edit) $${srcdir}$@.in > $@

paths.py: $(srcdir)/paths.py.in

CLEANFILES = paths.py

EXTRA_DIST = paths.py.in

-include $(top_srcdir)/git.mk
