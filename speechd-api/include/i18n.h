/*
 * i18n.h - prototypes for internationalization functions for Speech Dispatcher
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
 */

#ifndef I18N_H
#define I18N_H

#include <glib/gi18n.h>

/*
* Initialize internationalization support for Speech Dispatcher components
* This function must be called before any component outputs messages
* that should be translated to the user.
* It obtains the current locale from environment variables,
* See setlocale (3) for more information.
* If the initialization support fails, this function will exit the
* program after printing an error message.
*/

void i18n_init(void);

#endif
