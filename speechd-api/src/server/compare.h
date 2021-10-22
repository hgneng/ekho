
/*
 * compare.h - Auxiliary functions for comparing elements in lists
 *
 * Copyright (C) 2001, 2002, 2003 Brailcom, o.p.s.
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
 * $Id: compare.h,v 1.3 2006-07-11 16:12:27 hanke Exp $
 */

#ifndef COMPARE_H
#define COMPARE_H

gint compare_message_uid(gconstpointer element, gconstpointer value);

/* Pointer to function compare_message_uid */
extern GCompareFunc p_msg_lc;
extern GCompareFunc p_msg_uid_lc;

#endif /* COMPARE_H */
