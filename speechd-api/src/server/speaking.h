
/*
 * speaking.h - Speech Dispatcher speech output functions header
 *
 * Copyright (C) 2001,2002,2003 Brailcom, o.p.s, Prague 2,
 * Vysehradska 3/255, 128 00, <freesoft@freesoft.cz>
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
 * $Id: speaking.h,v 1.11 2006-07-11 16:12:27 hanke Exp $
 */

#ifndef SPEAKING_H
#define SPEAKING_H

#include <speechd_types.h>

extern OutputModule *speaking_module;
extern int speaking_uid;
extern int speaking_gid;

/* Pause and resume handling */
extern int pause_requested;
extern int pause_requested_fd;
extern int pause_requested_uid;
extern int resume_requested;

/* Speak() is responsible for getting right text from right
 * queue in right time and saying it loud through corresponding
 * synthetiser. (Note that there can be a big problem with synchronization).
 * This runs in a separate thread. */
void *speak(void *data);

/* Put this message into queue again, stripping index marks etc. */
int reload_message(TSpeechDMessage * msg);

/* Speech flow control functions */
void speaking_stop(int uid);
void speaking_stop_all(void);

void speaking_cancel(int uid);
void speaking_cancel_all(void);

int speaking_pause(int fd, int uid);
int speaking_pause_all(int fd);

int speaking_resume(int uid);
int speaking_resume_all(void);

/* Internal speech flow control functions */

/* If there is someone speaking on some output
 * module, return 1, otherwise 0. */
int is_sb_speaking(void);

/* Stops speaking and cancels currently spoken message.*/
void stop_speaking_active_module(void);

int stop_priority(SPDPriority priority);

void stop_from_uid(int uid);

/* Decides if the message should (not) be spoken now */
gint message_nto_speak(gconstpointer, gconstpointer);

void set_speak_thread_attributes(void);

/* Do priority interaction */
void resolve_priorities(SPDPriority priority);

/* Queue interaction helper functions */
TSpeechDMessage *get_message_from_queues(void);
GList *speaking_get_queue(SPDPriority priority);
void speaking_set_queue(SPDPriority priority, GList * queue);
gint sortbyuid(gconstpointer a, gconstpointer b);
int client_has_messages(int uid);

/* Get the unique id of the client who is speaking
 * on some output module */
int get_speaking_client_uid(void);

int socket_send_msg(int fd, const char *msg);
int report_index_mark(TSpeechDMessage * msg, const char *index_mark);
int report_begin(TSpeechDMessage * msg);
int report_end(TSpeechDMessage * msg);
int report_pause(TSpeechDMessage * msg);
int report_resume(TSpeechDMessage * msg);
int report_cancel(TSpeechDMessage * msg);

GList *empty_queue(GList * queue);
GList *empty_queue_by_time(GList * queue, unsigned int uid);

int stop_priority_older_than(SPDPriority priority, unsigned int uid);
GList *stop_priority_from_uid(GList * queue, const int uid);
void stop_priority_except_first(SPDPriority priority);

#endif /* SPEAKING_H */
