
/*
 * server.h - The server body
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: server.h,v 1.3 2006-07-11 16:12:27 hanke Exp $
 */

#ifndef SERVER_H
#define SERVER_H

/* serve() reads data from clients and sends it to parse() */
int serve(int fd);

/* Switches `receiving data' mode on and off for specified client */
int server_data_on(int fd);
void server_data_off(int fd);

/* Put a message into Dispatcher's queue */
int queue_message(TSpeechDMessage *new, int fd, int history_flag,
		  EMessageType type, int reparted);


#endif

