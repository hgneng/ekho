/*
 * i18n.c - internationalization support for Speech-dispatcher
 *
 * Copyright (C) 2010 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <i18n.h>
#include <locale.h>		/* For setlocale. */
#include <stdio.h>
#include <stdlib.h>

void i18n_init(void)
{
	if (setlocale(LC_ALL, "") == NULL) {
		perror("setlocale");
		exit(1);
	}

	if (bindtextdomain(GETTEXT_PACKAGE, LOCALEDIR) == NULL) {
		perror("bindtextdomain");
		exit(1);
	}

	if (textdomain(GETTEXT_PACKAGE) == NULL) {
		perror("textdomain");
		exit(1);
	}
}
