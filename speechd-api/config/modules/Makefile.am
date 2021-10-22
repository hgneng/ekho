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

dist_moduleconf_DATA = cicero.conf espeak.conf espeak-ng.conf festival.conf \
		       flite.conf dtk-generic.conf \
                       epos-generic.conf \
                       espeak-ng-mbrola-generic.conf espeak-mbrola-generic.conf \
                       llia_phon-generic.conf \
                       swift-generic.conf mary-generic.conf

dist_moduleconforig_DATA = cicero.conf espeak.conf festival.conf espeak-ng.conf
	                   flite.conf dtk-generic.conf \
                           epos-generic.conf \
                           espeak-ng-mbrola-generic.conf espeak-mbrola-generic.conf \
                           llia_phon-generic.conf \
                           swift-generic.conf mary-generic.conf

if ivona_support
dist_moduleconf_DATA += ivona.conf
dist_moduleconforig_DATA += ivona.conf
endif

if pico_support
dist_moduleconf_DATA += pico.conf
dist_moduleconforig_DATA += pico.conf
endif

if baratinoo_support
dist_moduleconf_DATA += baratinoo.conf
dist_moduleconforig_DATA += baratinoo.conf
endif

if kali_support
dist_moduleconf_DATA += kali.conf
dist_moduleconforig_DATA += kali.conf
endif

if ibmtts_support
dist_moduleconf_DATA += ibmtts.conf
dist_moduleconforig_DATA += ibmtts.conf
endif

if voxin_support
dist_moduleconf_DATA += voxin.conf
dist_moduleconforig_DATA += voxin.conf
endif

-include $(top_srcdir)/git.mk
