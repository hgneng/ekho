
/*
 * compare.c - Auxiliary functions for comparing elements in lists
 *
 * Copyright (C) 2001, 2002, 2003, 2007 Brailcom, o.p.s.
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
 * $Id: compare.c,v 1.5 2007-02-17 18:58:53 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>
#include "speechd.h"
#include "compare.h"

/* Pointer to compare_message_uid */
GCompareFunc p_msg_lc;
GCompareFunc p_msg_uid_lc = compare_message_uid;

gint compare_message_uid(gconstpointer element, gconstpointer value)
{
	int *uid_val;
	TSpeechDMessage *message;

	uid_val = (int *)value;

	message = ((TSpeechDMessage *) element);
	assert(message != NULL);
	//assert(message->settings.fd!=0);

	return (message->settings.uid - *uid_val);
}
