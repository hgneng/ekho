
/*
 * history.c - History functions for Speech Dispatcher
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
 * $Id: history.c,v 1.24 2008-02-08 10:01:09 hanke Exp $
 */

#include "speechd.h"
#include "msg.h"
#include "set.h"
#include "server.h"

#include "history.h"

/* Compares TSpeechDMessage data structure elements
   with given ID */
int
message_compare_id (gconstpointer element, gconstpointer value, gpointer n)
{
   int ret;
   ret = ((TSpeechDMessage*) element)->id - (int) value;
   return ret;
}

gint (*p_msg_comp_id)() = message_compare_id;

char*
history_get_client_list()
{
    TFDSetElement *client;
    GString *clist;
    int i;

    clist = g_string_new("");

    for(i=1; i<=SpeechdStatus.max_uid; i++){
        MSG(4,"Getting settings for client %d of %d", i, SpeechdStatus.max_uid-1);
        client = get_client_settings_by_uid(i);
        assert(client!=NULL);
        g_string_append_printf(clist, C_OK_CLIENTS"-");
        g_string_append_printf(clist, "%d ", client->uid);
        g_string_append(clist, client->client_name);
        g_string_append_printf(clist, " %d", client->active);
        g_string_append(clist,"\r\n");
    }
    g_string_append_printf(clist, OK_CLIENT_LIST_SENT);

    return clist->str;                
}

char*
history_get_client_id(int fd)
{
    GString *cid;
    int uid;

    cid = g_string_new("");

    uid = get_client_uid_by_fd(fd);
    if (uid == 0) return strdup(ERR_INTERNAL);

    g_string_append_printf(cid, C_OK_CLIENT_ID"-%d\r\n", uid);
    g_string_append_printf(cid, OK_CLIENT_ID_SENT);

    return cid->str;                
}

char*
history_get_message(int uid)
{
    /* TODO: Rework. */
#if 0
    GString *mtext;
    GList *gl;
    TSpeechDMessage *msg;
    int i, pos;
    char c;
    char helper[1024];

    mtext = g_string_new("");

    gl = g_list_find_custom(message_history, &uid, compare_message_uid);
    if (gl == NULL) return strdup(ERR_ID_NOT_EXIST);
    if (gl->data == NULL) return strdup(ERR_INTERNAL);
    msg = (TSpeechDMessage*) gl->data;

    i = 0;
    pos = 0;
    while(c = msg->buf[i]){
        helper[pos] = msg->buf[i];
        pos++; i++;
        if (c == '\n'){
            helper[pos-2] = 0;
            g_string_append_printf(mtext, C_OK_MSG_TEXT"-%s\r\n", helper);
            pos = 0;
        }
    }
    helper[pos] = 0;
    g_string_append_printf(mtext, C_OK_MSG_TEXT"-%s\r\n", helper);
    g_string_append_printf(mtext, OK_MSG_TEXT_SENT);

#endif

    return strdup(ERR_NOT_IMPLEMENTED);                
}


char*
history_get_message_list(guint client_id, int from, int num)
{
	TSpeechDMessage *message;
	GString *mlist;
	GList *gl;
	TFDSetElement *client_settings;
	GList *client_msgs;
	int i;

	MSG(4, "message_list: from %d num %d, client %d\n", from, num, client_id);
   
	mlist = g_string_new("");

	client_settings = get_client_settings_by_uid(client_id);
        if (client_settings == NULL) return strdup(ERR_NO_SUCH_CLIENT);
	
	client_msgs = get_messages_by_client(client_id);
	
	for (i=from; i<=from+num-1; i++){
		gl = g_list_nth(client_msgs, i);
		if (gl == NULL){
			g_string_append_printf(mlist, OK_MSGS_LIST_SENT);
			return mlist->str;
		}
		message = gl->data;

		if (message == NULL){
			if(SPEECHD_DEBUG) FATAL("Internal error.\n");
			return strdup(ERR_INTERNAL);
		}

		g_string_append_printf(mlist, C_OK_MSGS"-");
		g_string_append_printf(mlist, "%d %s\r\n", message->id, client_settings->client_name);
   }
 	
	g_string_append_printf(mlist, OK_MSGS_LIST_SENT);
	return mlist->str;
}

char*
history_get_last(int fd)
{
	TSpeechDMessage *message;
	GString *lastm;
	GList *gl;

	lastm = g_string_new("");

	gl = g_list_last(message_history);
	if (gl == NULL) return strdup(ERR_NO_MESSAGE);
	message = gl->data;
      
	g_string_append_printf(lastm, C_OK_LAST_MSG"-%d\r\n", 
		message->id);
	g_string_append_printf(lastm, OK_LAST_MSG);
	return lastm->str;
}

char*
history_cursor_set_last(int fd, guint client_id)
{
	GList *client_msgs;
	TFDSetElement *settings;

	settings = get_client_settings_by_fd(fd);
	if (settings == NULL) FATAL("Couldn't find settings for active client");

	client_msgs = get_messages_by_client(client_id);
	settings->hist_cur_pos = g_list_length(client_msgs) - 1;
	settings->hist_cur_uid = client_id;

	return strdup(OK_CUR_SET_LAST);
}

char*
history_cursor_set_first(int fd, guint client_id)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_fd(fd);
	if (settings == NULL) FATAL("Couldn't find settings for active client");

	settings->hist_cur_pos = 0;
	settings->hist_cur_uid = client_id;
	return strdup(OK_CUR_SET_FIRST);
}

char*
history_cursor_set_pos(int fd, guint client_id, int pos)
{
	TFDSetElement *settings;
	GList *client_msgs;

	if (pos < 0) return strdup(ERR_POS_LOW);

	client_msgs = get_messages_by_client(client_id);
	if (pos > g_list_length(client_msgs)-1) return strdup(ERR_POS_HIGH);

	settings = get_client_settings_by_fd(fd);
	if (settings == NULL) FATAL("Couldn't find settings for active client");
   
	settings->hist_cur_pos = pos;
	settings->hist_cur_uid = client_id;
	MSG(4,"cursor pos:%d\n", settings->hist_cur_pos);
	return strdup(OK_CUR_SET_POS);
}

char*
history_cursor_forward(int fd)
{
	TFDSetElement *settings;
	GList *client_msgs;
	
	settings = get_client_settings_by_fd(fd);
	if (settings == NULL) FATAL("Couldn't find settings for active client");
   
	client_msgs = get_messages_by_client(settings->hist_cur_uid);
	if ((settings->hist_cur_pos + 1) > g_list_length(client_msgs)-1) return strdup(ERR_POS_HIGH);
	settings->hist_cur_pos++; 

	return strdup(OK_CUR_MOV_FOR);
}

char*
history_cursor_backward(int fd)
{
	TFDSetElement *settings;

	settings = get_client_settings_by_fd(fd);
	if (settings == NULL) FATAL("Couldn't find settings for active client");
   
	if ((settings->hist_cur_pos - 1) < 0) return strdup(ERR_POS_LOW);
	settings->hist_cur_pos--; 
   
	return strdup(OK_CUR_MOV_BACK);
}

char*
history_cursor_get(int fd)
{
	TFDSetElement *settings;
	TSpeechDMessage *new;
	GString *reply;
	GList *gl, *client_msgs;

	reply = g_string_new("");

	settings = get_client_settings_by_fd(fd);
	if (settings == NULL) FATAL("Couldn't find settings for active client");
 	 
	client_msgs = get_messages_by_client(settings->hist_cur_uid);
	gl = g_list_nth(client_msgs, (int) settings->hist_cur_pos);
	if (gl == NULL) return strdup(ERR_NO_MESSAGE);
	new = gl->data;

	g_string_printf(reply, C_OK_CUR_POS"-%d\r\n"OK_CUR_POS_RET, new->id);
	return reply->str;
}

char*
history_say_id(int fd, int id)
{
	TSpeechDMessage *msg, *new;
	GList *gl; 

	gl = g_list_find_custom(message_history,  &id, p_msg_comp_id);
	if (gl == NULL) return strdup(ERR_ID_NOT_EXIST);
	msg = gl->data;
	if (msg == NULL) return strdup(ERR_INTERNAL);

	MSG(4,"putting history message into queue\n");
	new = (TSpeechDMessage*) spd_message_copy(msg);
	//	queue_message(new, fd, 0, 0);

	return strdup(OK_MESSAGE_QUEUED);
}

GList*
get_messages_by_client(int uid){
	GList *list = NULL;
	GList *gl;
	TSpeechDMessage* msg;
	guint i;
	guint history_length = g_list_length(message_history);

	for (i = 0; i < history_length; i++) {
		gl = g_list_nth(message_history, i);
		assert(gl!=NULL);
		msg = gl->data;
		if (msg->settings.uid == uid){
                    list = g_list_append(list, msg);
		}
	}
	
	return list;
}

