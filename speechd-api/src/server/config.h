
/*
 * config.h - Configuration for Speech Dispatcher
 *
 * Copyright (C) 2001, 2002, 2003, 2006 Brailcom, o.p.s.
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: config.h,v 1.4 2007-02-17 18:58:27 hanke Exp $
 */

#ifndef CONFIG_H
#define CONFIG_H

#include <stdlib.h>
#include <dotconf.h>

/* Loading options from DotConf */
configoption_t *spd_options;
int spd_num_options;

configoption_t* load_config_options(int *num_options);
void free_config_options(configoption_t *opts, int *num);
configoption_t *
add_config_option(configoption_t *options, int *num_config_options, char *name, int type,
                  dotconf_callback_t callback, info_t *info,
                  unsigned long context);

void load_default_global_set_options();

#endif
