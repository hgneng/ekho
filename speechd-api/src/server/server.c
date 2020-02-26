
/*
 * server.c - Speech Dispatcher server core
 *
 * Copyright (C) 2001,2002,2003, 2004, 2006, 2007 Brailcom, o.p.s
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
 * $Id: server.c,v 1.85 2008-06-27 12:28:58 hanke Exp $
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "speechd.h"
#include "server.h"
#include "set.h"
#include "speaking.h"
#include "sem_functions.h"
#include "history.h"

int last_message_id = 0;

/* Put a message into its queue.
 *
 * Parameters:
 *   new -- the (allocated) message structure to queue, it must contain
 *          the text to be queued in new->buf
 *   fd  -- file descriptor of the calling client (positive)
 *          unique id if the client is gone (negative) -- in this
 *          case it means we are reloading the message and the
 *          behavior is slightly different
 *   history_flag -- should this message be included in history?
 *   type -- type of the message (see ../../include/speechd_types.h)
 *   reparted -- if this is a preprocessed message reparted
 *             in more pieces
 * It returns 0 on success, -1 otherwise.
 */

#define COPY_SET_STR(name) \
	new->settings.name = (char*) g_strdup(settings->name);

/* Queue a message _new_. When fd is a positive number,
it means we have a new message from the client on connection
fd and we should fill in the proper settings. When fd is
negative, it's absolute value is the client uid and the
message _new_ already contains a fully filled in settings
structure which should not be overwritten (on must be cautius
that the original client might not be still connected
to speechd). _history_flag_ indicates if inclusion into
history is desired and _reparted_ flag indicates whether
this message is a part of a reparted message (one of a block
of messages). */
int
queue_message(TSpeechDMessage * new, int fd, int history_flag,
	      SPDMessageType type, int reparted)
{
	TFDSetElement *settings;
	TSpeechDMessage *message_copy;
	int id;
	GList *element;

	/* Check function parameters */
	if (new == NULL)
		return -1;
	if (new->buf == NULL)
		return -1;
	if (strlen(new->buf) < 1)
		return -1;

	/* Find settings for this particular client */
	if (fd > 0) {
		settings = get_client_settings_by_fd(fd);
		if (settings == NULL)
			FATAL
			    ("Couldn't find settings for active client, internal error.");
	} else if (fd < 0) {
		settings = get_client_settings_by_uid(-fd);
	} else {
		if (SPEECHD_DEBUG)
			FATAL("fd == 0, this shouldn't happen...");
		return -1;
	}

	MSG(5, "In queue_message desired output module is %s",
	    settings->output_module);

	if (fd > 0) {
		/* Copy the settings to the new to-be-queued element */
		new->settings = *settings;
		new->settings.type = type;
		new->settings.index_mark = NULL;
		COPY_SET_STR(client_name);
		COPY_SET_STR(output_module);
		COPY_SET_STR(msg_settings.voice.language);
		COPY_SET_STR(msg_settings.voice.name);

		COPY_SET_STR(index_mark);
		COPY_SET_STR(audio_output_method);
		COPY_SET_STR(audio_oss_device);
		COPY_SET_STR(audio_alsa_device);
		COPY_SET_STR(audio_nas_server);
		COPY_SET_STR(audio_pulse_server);

		/* And we set the global id (note that this is really global, not
		 * depending on the particular client, but unique) */
		last_message_id++;
		new->id = last_message_id;
		new->time = time(NULL);

		new->settings.paused_while_speaking = 0;
	}
	id = new->id;

	new->settings.reparted = reparted;

	MSG(5, "Queueing message |%s| with priority %d", new->buf,
	    settings->priority);

	/* If desired, put the message also into history */
	/* NOTE: This should be before we put it into queues() to
	   avoid conflicts with the other thread (it could delete
	   the message before we woud copy it) */
	//    if (history_flag){
	if (0) {
		pthread_mutex_lock(&element_free_mutex);
		history_add_message(new);
		pthread_mutex_unlock(&element_free_mutex);
	}

	pthread_mutex_lock(&element_free_mutex);
	/* Put the element new to queue according to it's priority. */
	check_locked(&element_free_mutex);
	switch (settings->priority) {
	case SPD_IMPORTANT:
		MessageQueue->p1 = g_list_append(MessageQueue->p1, new);
		break;
	case SPD_MESSAGE:
		MessageQueue->p2 = g_list_append(MessageQueue->p2, new);
		break;
	case SPD_TEXT:
		MessageQueue->p3 = g_list_append(MessageQueue->p3, new);
		break;
	case SPD_NOTIFICATION:
		MessageQueue->p4 = g_list_append(MessageQueue->p4, new);
		break;
	case SPD_PROGRESS:
		MessageQueue->p5 = g_list_append(MessageQueue->p5, new);
		//clear last_p5_block if we get new block or no block message
		element = g_list_last(last_p5_block);
		if (!element || !element->data
		    || ((TSpeechDMessage *) (element->data))->
		    settings.reparted != new->settings.reparted) {
			g_list_foreach(last_p5_block, (GFunc) mem_free_message,
				       NULL);
			g_list_free(last_p5_block);
			last_p5_block = NULL;
		}
		// insert message
		message_copy = spd_message_copy(new);
		if (message_copy != NULL)
			last_p5_block =
			    g_list_append(last_p5_block, message_copy);

		break;
	default:
		FATAL("Nonexistent priority given");
	}

	/* Look what is the highest priority of waiting
	 * messages and take the desired actions on other
	 * messages */
	/* TODO: Do the entire resolve_priorities() here is certainly
	   not the best approach possible. Especially the part that
	   calls output_stop() should be moved to speaking.c speak()
	   function in future */
	resolve_priorities(settings->priority);
	pthread_mutex_unlock(&element_free_mutex);

	speaking_semaphore_post();

	MSG(5, "Message inserted into queue.");

	return id;
}

#undef COPY_SET_STR

/* Switch data mode on for the particular client. */
void server_data_on(int fd)
{
	TSpeechDSock *speechd_socket = speechd_socket_get_by_fd(fd);
	assert(speechd_socket);
	/* Mark this client as ,,sending data'' */
	speechd_socket->awaiting_data = 1;
	/* Create new output buffer */
	speechd_socket->o_buf = g_string_new("");
	MSG(4, "Switching to data mode...");
	return;
}

/* Switch data mode off for the particular client. */
void server_data_off(int fd)
{
	TSpeechDSock *speechd_socket = speechd_socket_get_by_fd(fd);
	assert(speechd_socket);
	assert(speechd_socket->o_buf);
	speechd_socket->o_bytes = 0;
	g_string_free(speechd_socket->o_buf, 1);
	speechd_socket->o_buf = NULL;
	return;
}

/* Serve the client on _fd_ if we got some activity. */
int serve(int fd)
{
	char *reply;		/* Reply to the client */
	int ret;

	{
		size_t bytes = 0;	/* Number of bytes we got */
		int buflen = BUF_SIZE;
		char *buf = (char *)g_malloc(buflen + 1);

		/* Read data from socket */
		/* Read exactly one complete line, the `parse' routine relies on it */
		{
			while (1) {
				int n = read(fd, buf + bytes, 1);
				if (n <= 0) {
					g_free(buf);
					return -1;
				}
				/* Note, bytes is a 0-based index into buf. */
				if ((buf[bytes] == '\n')
				    && (bytes >= 1) && (buf[bytes - 1] == '\r')) {
					buf[++bytes] = '\0';
					break;
				}
				if (buf[bytes] == '\0')
					buf[bytes] = '?';
				if ((++bytes) == buflen) {
					buflen *= 2;
					buf = g_realloc(buf, buflen + 1);
				}
			}
		}

		/* Parse the data and read the reply */
		MSG2(5, "protocol", "%d:DATA:|%s| (%lu)", fd, buf, (unsigned long) bytes);
		reply = parse(buf, bytes, fd);
		g_free(buf);
	}

	if (reply == NULL)
		FATAL("Internal error, reply from parse() is NULL!");

	/* Send the reply to the socket */
	if (strlen(reply) == 0) {
		g_free(reply);
		return 0;
	}
	if (reply[0] != '9') {	/* Don't reply to data etc. */
		pthread_mutex_lock(&socket_com_mutex);
		MSG2(5, "protocol", "%d:REPLY:|%s|", fd, reply);
		ret = write(fd, reply, strlen(reply));
		g_free(reply);
		pthread_mutex_unlock(&socket_com_mutex);
		if (ret == -1) {
			MSG(5, "write() error: %s", strerror(errno));
			return -1;
		}
	} else {
		g_free(reply);
	}

	return 0;
}
