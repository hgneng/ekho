
/*
 * history.h - History functions for Speech Dispatcher header
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
 * $Id: history.h,v 1.11 2006-07-11 16:12:27 hanke Exp $
 */

#ifndef HISTORY_H
#define HISTORY_H

#include "speechd.h"

char *history_get_client_list(void);
char *history_get_message_list(guint client_id, int from, int num);
char *history_get_last(int fd);
char *history_cursor_set_last(int fd, guint client_id);
char *history_cursor_set_first(int fd, guint client_id);
char *history_cursor_set_pos(int fd, guint client_id, int pos);
char *history_cursor_next(int fd);
char *history_cursor_prev(int fd);
char *history_cursor_get(int fd);
char *history_cursor_forward(int fd);
char *history_cursor_backward(int fd);
char *history_say_id(int fd, int id);
char *history_get_client_id(int fd);
char *history_get_message(int uid);
int history_add_message(TSpeechDMessage * msg);

/* Internal functions */
GList *get_messages_by_client(int uid);
gint message_compare_id(gconstpointer element, gconstpointer value);

#endif /* HISTORY_H */
