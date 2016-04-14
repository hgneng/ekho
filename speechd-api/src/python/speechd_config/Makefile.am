## Process this file with automake to produce Makefile.in

dist_snddata_DATA = test.wav

dist_bin_SCRIPTS = spd-conf

speechd_pythondir = $(pyexecdir)/speechd_config
speechd_python_PYTHON = __init__.py config.py
nodist_speechd_python_PYTHON = paths.py

paths_edit = sed \
	-e "s:[@]spdconforigdir[@]:$(spdconforigdir):" \
	-e "s:[@]spdconfdir[@]:$(spdconfdir):" \
	-e "s:[@]snddatadir[@]:$(snddatadir):" \
	-e "s:[@]spddesktopconforigdir[@]:$(spddesktopconforigdir):"

paths.py: Makefile
	rm -f $@
	srcdir=; \
	test -f ./$@.in || srcdir=$(srcdir)/; \
	$(paths_edit) $${srcdir}$@.in > $@

paths.py: $(srcdir)/paths.py.in

CLEANFILES = paths.py

EXTRA_DIST = paths.py.in
