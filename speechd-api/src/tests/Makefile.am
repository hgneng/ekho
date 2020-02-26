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

DISTCLEANFILES = atconfig $(TESTSUITE)

c_api = $(top_builddir)/src/api/c

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/src/api/c $(GLIB_CFLAGS)

# The `:;' works around a redirected compound command bash exit status bug.
package.m4: Makefile
	:;{ \
	  echo '# Signature of the current package.' && \
	  echo 'm4_define([AT_PACKAGE_NAME],      [$(PACKAGE_NAME)])' && \
	  echo 'm4_define([AT_PACKAGE_TARNAME],   [$(PACKAGE_TARNAME)])' && \
	  echo 'm4_define([AT_PACKAGE_VERSION],   [$(PACKAGE_VERSION)])' && \
	  echo 'm4_define([AT_PACKAGE_STRING],    [$(PACKAGE_STRING)])' && \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [$(PACKAGE_BUGREPORT)])' && \
	  echo 'm4_define([AT_PACKAGE_URL],       [$(PACKAGE_URL)])'; \
	} > $@-t
	mv $@-t $@

atconfig: $(top_builddir)/config.status
	cd $(top_builddir) && ./config.status src/tests/$@

AUTOM4TE = autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest

TESTSUITE_AT = c_api.at
TESTSUITE = ./testsuite
$(TESTSUITE): package.m4 testsuite.at $(TESTSUITE_AT)
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

check_PROGRAMS = long_message clibrary clibrary2 run_test connection_recovery \
               spd_cancel_long_message spd_set_notifications_all

long_message_SOURCES = long_message.c
long_message_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS)

clibrary_SOURCES = clibrary.c
clibrary_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS)

clibrary2_SOURCES = clibrary2.c
clibrary2_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS)

connection_recovery_SOURCES = connection-recovery.c
connection_recovery_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS)

spd_cancel_long_message_SOURCES = spd_cancel_long_message.c
spd_cancel_long_message_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS)

spd_set_notifications_all_SOURCES = spd_set_notifications_all.c
spd_set_notifications_all_LDADD = $(c_api)/libspeechd.la $(EXTRA_SOCKET_LIBS)

run_test_SOURCES = run_test.c
run_test_LDADD = $(c_api)/libspeechd.la $(GLIB_LIBS) $(EXTRA_SOCKET_LIBS)

EXTRA_DIST= basic.test general.test keys.test priority_progress.test \
            pronunciation.test punctuation.test sound_icons.test spelling.test \
            ssml.test stop_and_pause.test voices.test yo.wav \
            testsuite.at $(TESTSUITE_AT) sayfortune.sh

clean-local:
	test ! -f $(TESTSUITE) || $(SHELL) $(TESTSUITE) --clean
	rm -f *.tmp
	rm -f -r autom4te.cache

# Run the test suite on the *installed* tree.
testinstall: atconfig $(TESTSUITE)
	$(SHELL) $(TESTSUITE) AUTOTEST_PATH="$(bindir)" $(TESTSUITEFLAGS)

CLEANFILES = package.m4

-include $(top_srcdir)/git.mk
