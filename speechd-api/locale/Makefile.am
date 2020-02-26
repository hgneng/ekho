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

nobase_localedata_DATA = \
	en/font-variants.dic \
	af/emojis.dic \
	am/emojis.dic \
	an/symbols.dic \
	ar/emojis.dic \
	ar/symbols.dic \
	ar_SA/emojis.dic \
	as/emojis.dic \
	ast/emojis.dic \
	az/emojis.dic \
	be/emojis.dic \
	bg/emojis.dic \
	bg/symbols.dic \
	bn/emojis.dic \
	br/emojis.dic \
	bs/emojis.dic \
	ca/emojis.dic \
	ca/symbols.dic \
	ccp/emojis.dic \
	ceb/emojis.dic \
	chr/emojis.dic \
	ckb/symbols.dic \
	cs/emojis.dic \
	cs/symbols.dic \
	cy/emojis.dic \
	da/emojis.dic \
	da/symbols.dic \
	de/emojis.dic \
	de/symbols.dic \
	de_CH/emojis.dic \
	de_CH/symbols.dic \
	el/emojis.dic \
	el/symbols.dic \
	en/emojis.dic \
	en/symbols.dic \
	en_AU/emojis.dic \
	en_CA/emojis.dic \
	en_GB/emojis.dic \
	en_IN/emojis.dic \
	es/emojis.dic \
	es/symbols.dic \
	es_419/emojis.dic \
	es_CO/symbols.dic \
	es_MX/emojis.dic \
	es_US/emojis.dic \
	et/emojis.dic \
	eu/emojis.dic \
	fa/emojis.dic \
	fa/symbols.dic \
	fi/emojis.dic \
	fi/symbols.dic \
	fil/emojis.dic \
	fo/emojis.dic \
	fr/emojis.dic \
	fr/symbols.dic \
	fr_CA/emojis.dic \
	ga/emojis.dic \
	ga/symbols.dic \
	gd/emojis.dic \
	gl/emojis.dic \
	gl/symbols.dic \
	gu/emojis.dic \
	he/emojis.dic \
	he/symbols.dic \
	hi/emojis.dic \
	hi/symbols.dic \
	hr/emojis.dic \
	hr/symbols.dic \
	hu/emojis.dic \
	hu/symbols.dic \
	hy/emojis.dic \
	ia/emojis.dic \
	id/emojis.dic \
	is/emojis.dic \
	is/symbols.dic \
	it/emojis.dic \
	it/symbols.dic \
	ja/emojis.dic \
	ja/symbols.dic \
	jv/emojis.dic \
	ka/emojis.dic \
	ka/symbols.dic \
	kab/emojis.dic \
	kk/emojis.dic \
	km/emojis.dic \
	kmr/symbols.dic \
	kn/emojis.dic \
	kn/symbols.dic \
	ko/emojis.dic \
	ko/symbols.dic \
	kok/emojis.dic \
	ku/emojis.dic \
	ky/emojis.dic \
	ky/symbols.dic \
	lo/emojis.dic \
	lt/emojis.dic \
	lt/symbols.dic \
	lv/emojis.dic \
	mk/emojis.dic \
	mk/symbols.dic \
	ml/emojis.dic \
	mn/emojis.dic \
	mn/symbols.dic \
	mr/emojis.dic \
	ms/emojis.dic \
	my/emojis.dic \
	my/symbols.dic \
	nb/emojis.dic \
	nb_NO/symbols.dic \
	ne/emojis.dic \
	nl/emojis.dic \
	nl/symbols.dic \
	nn/emojis.dic \
	nn_NO/symbols.dic \
	or/emojis.dic \
	pa/emojis.dic \
	pa/symbols.dic \
	pl/emojis.dic \
	pl/symbols.dic \
	ps/emojis.dic \
	pt/emojis.dic \
	pt_BR/symbols.dic \
	pt_PT/emojis.dic \
	pt_PT/symbols.dic \
	qu/emojis.dic \
	ro/emojis.dic \
	ro/symbols.dic \
	ru/emojis.dic \
	ru/symbols.dic \
	sd/emojis.dic \
	si/emojis.dic \
	sk/emojis.dic \
	sk/symbols.dic \
	sl/emojis.dic \
	sl/symbols.dic \
	so/emojis.dic \
	so/symbols.dic \
	sq/emojis.dic \
	sq/symbols.dic \
	sr/emojis.dic \
	sr/symbols.dic \
	sr_BA/emojis.dic \
	sv/emojis.dic \
	sv/symbols.dic \
	sw/emojis.dic \
	sw_KE/emojis.dic \
	ta/emojis.dic \
	ta/symbols.dic \
	te/emojis.dic \
	th/emojis.dic \
	tk/emojis.dic \
	to/emojis.dic \
	tr/emojis.dic \
	tr/symbols.dic \
	uk/emojis.dic \
	uk/symbols.dic \
	ur/emojis.dic \
	uz/emojis.dic \
	vi/emojis.dic \
	vi/symbols.dic \
	yue/emojis.dic \
	zh/emojis.dic \
	zh/symbols.dic \
	zh_CN/symbols.dic \
	zh_HK/emojis.dic \
	zh_HK/symbols.dic \
	zh_TW/emojis.dic \
	zh_TW/symbols.dic \
	zu/emojis.dic

EXTRA_DIST = ${nobase_localedata_DATA}

# List of latin American Spanish languages
ES_419_CLDR = \
	es_AR \
	es_BO \
	es_CL \
	es_CO \
	es_CR \
	es_CU \
	es_DO \
	es_EC \
	es_GT \
	es_HN \
	es_NI \
	es_PA \
	es_PE \
	es_PR \
	es_PY \
	es_SV \
	es_UY \
	es_VE

import-symbols:
	./import-symbols.py

install-data-hook:
	for lang419 in $(ES_419_CLDR) ; do \
		$(MKDIR_P) "$(DESTDIR)$(localedatadir)/$$lang419" ;\
		$(LN_S) "../es_419/emojis.dic" "$(DESTDIR)$(localedatadir)/$$lang419/emojis.dic" || true ;\
	done

uninstall-hook:
	for lang419 in $(ES_419_CLDR) ; do \
		rm "$(DESTDIR)$(localedatadir)/$$lang419/emojis.dic" ;\
	done

-include $(top_srcdir)/git.mk
GITIGNOREFILES = symbolsrc
