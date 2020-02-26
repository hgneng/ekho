/*
 * mluvitko.c - Speechd module for mluvitko (czech software synthetizer)
 *
 * Copyright (C) 2001 Brailcom, o.p.s.
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
 * Author: Tomas Cerha <cerha@brailcom.cz>
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define VERSION "0.0.1"

#include <stdio.h>
#include <glib.h>

#include "module.h"

gint mluvitko_write(const gchar * data, gint len);
gint mluvitko_stop(void);
gint mluvitko_pause(void);
gint mluvitko_release(void);

/* fill the module_info structure with pointers to this modules functions */
OutputModule modinfo = {
	"mluvitko",
	"Czech software synthesizer",
	NULL,			/* filename */
	mluvitko_write,
	mluvitko_stop,
	mluvitko_pause,
	mluvitko_release
};

/* entry point of this module */
OutputModule *module_init(void)
{
	printf("mluvitko: init_module()\n");

	/*modinfo.name = g_strdup("mluvitko"),
	   modinfo.description = g_strdup_printf("Czech software synthesizer, version %s",VERSION); */
	return &modinfo;
}

/* module operations */
gint mluvitko_write(const gchar * data, gint len)
{
	int i;

	printf("mluvitko: write()\n");

	for (i = 0; i < len; i++) {
		printf("%c ", data[i]);
	}
	printf("\n");

	return len;
}

gint mluvitko_stop(void)
{
	printf("mluvitko: stop()\n");
	return 1;
}

gint mluvitko_pause(void)
{
	printf("mluvitko: pause()\n");
	return 1;
}

gint mluvitko_release(void)
{
	printf("mluvitko: release()\n");
	return 1;
}
