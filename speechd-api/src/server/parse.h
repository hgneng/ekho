/*
 * parse.h - Parses commands Speech Dispatcher got from client
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef PARSE_H
#define PARSE_H

char *parse(const char *buf, const int bytes, const int fd);

char *parse_history(const char *buf, const int bytes, const int fd,
		    const TSpeechDSock * speechd_socket);
char *parse_set(const char *buf, const int bytes, const int fd,
		const TSpeechDSock * speechd_socket);
char *parse_stop(const char *buf, const int bytes, const int fd,
		 const TSpeechDSock * speechd_socket);
char *parse_cancel(const char *buf, const int bytes, const int fd,
		   const TSpeechDSock * speechd_socket);
char *parse_pause(const char *buf, const int bytes, const int fd,
		  const TSpeechDSock * speechd_socket);
char *parse_resume(const char *buf, const int bytes, const int fd,
		   const TSpeechDSock * speechd_socket);
char *parse_snd_icon(const char *buf, const int bytes, const int fd,
		     const TSpeechDSock * speechd_socket);
char *parse_char(const char *buf, const int bytes, const int fd,
		 const TSpeechDSock * speechd_socket);
char *parse_key(const char *buf, const int bytes, const int fd,
		const TSpeechDSock * speechd_socket);
char *parse_list(const char *buf, const int bytes, const int fd,
		 const TSpeechDSock * speechd_socket);
char *parse_get(const char *buf, const int bytes, const int fd,
		const TSpeechDSock * speechd_socket);
char *parse_help(const char *buf, const int bytes, const int fd,
		 const TSpeechDSock * speechd_socket);
char *parse_block(const char *buf, const int bytes, const int fd,
		  TSpeechDSock * speechd_socket);

char *deescape_dot(const char *orig_text, size_t orig_len);

/* Function for parsing the input from clients */
char *get_param(const char *buf, const int n, const int bytes,
		const int lower_case);

/* Other internal functions */
char *parse_general_event(const char *buf, const int bytes, const int fd,
			  const TSpeechDSock * speechd_socket,
			  SPDMessageType type);
int spd_utf8_read_char(char *pointer, char *character);

#endif
