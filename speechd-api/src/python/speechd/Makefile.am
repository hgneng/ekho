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
