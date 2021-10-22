/*
 * module_config.c - Manage configuration of output module.
 *
 * Copyright (C) 2001, 2002, 2003, 2006 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1, or (at your option) any later
 * version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: module_main_loop.c,v 1.17 2008-10-15 17:05:37 hanke Exp $
 */

#include <dotconf.h>
#include <ltdl.h>

#include "module_main.h"
#include "module_utils.h"

int module_config(const char *configfilename) {
	int ret;

	/* Initialize ltdl's list of preloaded audio backends. */
	LTDL_SET_PRELOADED_SYMBOLS();

	module_num_dc_options = 0;
	module_audio_id = 0;

	ret = module_load();
	if (ret == -1)
		return -1;

	if (configfilename == NULL) {
		DBG("No config file specified, using defaults...\n");
		return 0;
	}

	/* Add the LAST option */
	module_dc_options = module_add_config_option(module_dc_options,
						     &module_num_dc_options,
						     "", 0, NULL, NULL,
						     0);

	configfile =
	    dotconf_create((char*) configfilename, module_dc_options, 0,
			   CASE_INSENSITIVE);

	if (!configfile) {
		DBG("Can't read specified config file!\n");
		return -1;
	}

	if (dotconf_command_loop(configfile) == 0) {
		DBG("Error reading config file\n");
		return -1;
	}
	dotconf_cleanup(configfile);
	DBG("Configuration (pre) has been read from \"%s\"\n",
	    configfilename);

	return 0;
}
