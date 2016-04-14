## Process this file with automake to produce Makefile.in

# APIs in languages other than C must be included here
# (either directly listing all files or indirectly
# in SUBDIRS if they contain another Makefile)
# otherwise they won't be included in distribution packages

EXTRA_DIST = cl/ChangeLog cl/README cl/configuration.lisp cl/elisp.lisp \
             cl/package.lisp cl/ssip.lisp cl/sysdep.lisp cl/ssip.asd \
             guile/ChangeLog guile/Makefile guile/README guile/gssip.scm.in \
             guile/gssip.c guile/gssip.h

SUBDIRS=server audio c modules tests

if HAVE_PYTHON
SUBDIRS += python
endif

DIST_SUBDIRS=server audio c modules tests python


