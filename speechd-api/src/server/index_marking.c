
/*
 * index_marking.c -- Implements functions handling index marking
 *                    for Speech Dispatcher
 *
 * Copyright (C) 2001,2002,2003, 2007 Brailcom, o.p.s
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
 * $Id: index_marking.c,v 1.17 2008-06-11 12:11:25 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "index_marking.h"

void insert_index_marks(TSpeechDMessage * msg, SPDDataMode ssml_mode)
{
	GString *marked_text;
	char *pos;
	char character[6];
	char character2[6];
	gunichar u_char;
	int n = 0;
	int ret;
	int inside_tag = 0;

	marked_text = g_string_new("");

	assert(msg != NULL);
	assert(msg->buf != NULL);

	MSG2(5, "index_marking", "MSG before index marking: |%s|, ssml_mode=%d",
	     msg->buf, ssml_mode);

	if (ssml_mode == SPD_DATA_TEXT)
		g_string_printf(marked_text, "<speak>");

	pos = msg->buf;
	while (pos) {
		ret = spd_utf8_read_char(pos, character);
		if (ret == 0 || (strlen(character) == 0))
			break;
		u_char = g_utf8_get_char(character);

		if (u_char == '<') {
			if (ssml_mode == SPD_DATA_SSML) {
				inside_tag = 1;
				g_string_append_printf(marked_text, "%s",
						       character);
			} else
				g_string_append_printf(marked_text, "&lt;");
		} else if (u_char == '>') {
			if (ssml_mode == SPD_DATA_SSML) {
				inside_tag = 0;
				g_string_append_printf(marked_text, "%s",
						       character);
			} else
				g_string_append_printf(marked_text, "&gt;");
		} else if (u_char == '&') {
			if (ssml_mode == SPD_DATA_SSML) {
				g_string_append_printf(marked_text, "%s",
						       character);
			} else {
				if (!inside_tag)
					g_string_append_printf(marked_text,
							       "&amp;");
			}
		} else
		    if (((u_char == '.') || (u_char == '?') || (u_char == '!'))
			&& !inside_tag) {
			pos = g_utf8_find_next_char(pos, NULL);
			ret = spd_utf8_read_char(pos, character2);
			if ((ret == 0) || (strlen(character2) == 0)) {
				g_string_append_printf(marked_text, "%s",
						       character);
				MSG2(6, "index_marking", "MSG altering 1: |%s|",
				     marked_text->str);
				break;
			}
			u_char = g_utf8_get_char(character2);
			if ((g_unichar_isspace(u_char)) || (u_char == '<')
			    || (u_char == '&')) {
				g_string_append_printf(marked_text,
						       "%s" SD_MARK_HEAD "%d"
						       SD_MARK_TAIL,
						       character, n);
				n++;
				MSG2(6, "index_marking", "MSG altering 2: |%s|",
				     marked_text->str);
				continue;
			} else {
				g_string_append_printf(marked_text, "%s",
						       character);
				MSG2(6, "index_marking", "MSG altering 3: |%s|",
				     marked_text->str);
				continue;
			}
		} else {
			g_string_append_printf(marked_text, "%s", character);
		}

		pos = g_utf8_find_next_char(pos, NULL);
	}

	if (ssml_mode == SPD_DATA_TEXT)
		g_string_append_printf(marked_text, "</speak>");

	g_free(msg->buf);
	msg->buf = marked_text->str;

	g_string_free(marked_text, 0);

	MSG2(5, "index_marking", "MSG after index marking: |%s|", msg->buf);
}

/* Finds the index mark specified in _mark_ . */
char *find_index_mark(TSpeechDMessage * msg, int mark)
{
	char str_mark[64];
	char *pos;
	char *p;

	MSG(5, "Trying to find index mark %d", mark);

	/* Fix this for variable space number */
	sprintf(str_mark, SD_MARK_HEAD "%d" SD_MARK_TAIL, mark);

	p = strstr(msg->buf, str_mark);
	if (p == 0)
		return NULL;

	pos = p + strlen(str_mark);

	MSG(5, "Search for index mark sucessfull");

	return pos;
}

/* Deletes all index marks from the given text */
char *strip_index_marks(char *buf, SPDDataMode ssml_mode)
{
	GString *str;
	char *strret;

	char str_mark[] = SD_MARK_HEAD;

	char *p;
	char *p_old;

	if (ssml_mode == SPD_DATA_SSML)
		str = g_string_new("<speak>");
	else
		str = g_string_new("");

	MSG2(5, "index_marking", "Message before stripping index marks: |%s|",
	     buf);

	p = buf;

	while (1) {
		if (*p == '\0')
			break;
		p_old = p;
		p = strstr(p, str_mark);
		if (p != NULL) {
			g_string_append_len(str, p_old, (int)(p - p_old));
		} else {
			g_string_append(str, p_old);
			break;
		}
		do {
			p++;
		} while (*p != '>' && *p != '\0');
		if (*p == '>')
			p++;
	}

	if (ssml_mode == SPD_DATA_TEXT) {
		p = strstr(str->str, "</speak>");
		if (p != NULL)
			*p = 0;
	}

	strret = str->str;
	g_string_free(str, 0);

	MSG2(5, "index_marking", "Message after stripping index marks: |%s|",
	     strret);

	return strret;
}
