/*
 * parse.c - Parses commands Speech Dispatcher got from client
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: parse.c,v 1.73 2008-07-01 08:52:31 hanke Exp $
 */

#include <ctype.h>

#include "speechd.h"

#include "set.h"
#include "history.h"
#include "msg.h"
#include "server.h"
#include "sem_functions.h"
#include "output.h"

/*
  Parse() receives input data and parses them. It can
  be either command or data to speak. If it's command, it
  is immadetaily executed (eg. parameters are set). If it's
  data to speak, they are queued in corresponding queues
  with corresponding parameters for synthesis.
*/
/* SSIP command allowed inside block? */
#define BLOCK_NO 0
#define BLOCK_OK 1

#define CHECK_SSIP_COMMAND(cmd_name, parse_function, allowed_in_block)\
    if(!strcmp(command, cmd_name)){ \
        spd_free(command); \
        if ((allowed_in_block == BLOCK_NO) && speechd_socket->inside_block) \
            return strdup(ERR_NOT_ALLOWED_INSIDE_BLOCK); \
        return (char*) (parse_function) (buf, bytes, fd, speechd_socket);	\
    }

#define NOT_ALLOWED_INSIDE_BLOCK() \
    if(speechd_socket->inside_block > 0) \
        return strdup(ERR_NOT_ALLOWED_INSIDE_BLOCK);

#define ALLOWED_INSIDE_BLOCK() ;

char* 
parse(const char *buf, const int bytes, const int fd)
{
    TSpeechDMessage *new;
    char *command;
    int r;
    int end_data;
    char *pos;
    int reparted;
    int msg_uid;
    GString* ok_queued_reply;
    char *reply;
    TSpeechDSock *speechd_socket = speechd_socket_get_by_fd(fd);
    assert(speechd_socket);       

    end_data = 0;
    if ((buf == NULL) || (bytes == 0)){
        if(SPEECHD_DEBUG) FATAL("invalid buffer for parse()\n");
        return strdup(ERR_INTERNAL);
    }
 
    /* First the condition that we are not in data mode and we
     * are awaiting commands */
    if (speechd_socket->awaiting_data == 0){
        /* Read the command */
        command = get_param(buf, 0, bytes, 1);

        MSG(5, "Command caught: \"%s\"", command);

        /* Here we will check which command we got and process
         * it with its parameters. */

        if (command == NULL){
            if(SPEECHD_DEBUG) FATAL("Invalid buffer for parse()\n");
            return strdup(ERR_INTERNAL); 
        }		

        CHECK_SSIP_COMMAND("set", parse_set, BLOCK_OK);
        CHECK_SSIP_COMMAND("history", parse_history, BLOCK_NO);
        CHECK_SSIP_COMMAND("stop", parse_stop, BLOCK_NO);
        CHECK_SSIP_COMMAND("cancel", parse_cancel, BLOCK_NO);
        CHECK_SSIP_COMMAND("pause", parse_pause, BLOCK_NO);
        CHECK_SSIP_COMMAND("resume", parse_resume, BLOCK_NO);
        CHECK_SSIP_COMMAND("sound_icon", parse_snd_icon, BLOCK_OK);
        CHECK_SSIP_COMMAND("char", parse_char, BLOCK_OK);
        CHECK_SSIP_COMMAND("key", parse_key, BLOCK_OK)
        CHECK_SSIP_COMMAND("list", parse_list, BLOCK_NO);
        CHECK_SSIP_COMMAND("get", parse_get, BLOCK_NO);
        CHECK_SSIP_COMMAND("help", parse_help, BLOCK_NO);		
        CHECK_SSIP_COMMAND("block", parse_block, BLOCK_OK);

        if (!strcmp(command,"bye") || !strcmp(command,"quit")){
            MSG(4, "Bye received.");
            /* Send a reply to the socket */
            if (write(fd, OK_BYE, strlen(OK_BYE))){
		MSG(2, "ERROR: Can't write OK_BYE message to client socket: %s", strerror(errno));
	    }
		
            speechd_connection_destroy(fd);
            /* This is internal Speech Dispatcher message, see serve() */
            spd_free(command);
            return strdup("999 CLIENT GONE"); /* This is an internal message, not part of SSIP */
        }
	
        if (!strcmp(command,"speak")){
	    spd_free(command);
            /* Ckeck if we have enough space in awaiting_data table for
             * this client, that can have higher file descriptor that
             * everything we got before */
            r = server_data_on(fd);
            if (r!=0){
                if(SPEECHD_DEBUG) FATAL("Can't switch to data on mode\n");
                return strdup(ERR_INTERNAL);
            }
            return strdup(OK_RECEIVE_DATA);
        }
        spd_free(command);
        return strdup(ERR_INVALID_COMMAND);

        /* The other case is that we are in awaiting_data mode and
         * we are waiting for text that is comming through the chanel */
    }else{
         enddata:
        /* In the end of the data flow we got a "\r\n.\r\n" command. */
        MSG(5,"Buffer: |%s| bytes:", buf, bytes);

        if(((bytes >= 5) && ((!strncmp(buf, "\r\n.\r\n", bytes))))||(end_data == 1)
           ||((bytes == 3) && (!strncmp(buf, ".\r\n", bytes)))){

            MSG(5,"Finishing data");
            end_data = 0;

            /* Set the flag to command mode */
            MSG(5, "Switching back to command mode...");
            speechd_socket->awaiting_data = 0;

            /* Prepare element (text+settings commands) to be queued. */

	    /* TODO: Remove? */
            if ((bytes == 3) && (speechd_socket->o_bytes > 2)) speechd_socket->o_bytes -= 2;

	    /* Check if message contains any data */
            if (speechd_socket->o_bytes == 0) return strdup(OK_MSG_CANCELED);

	    /* Check buffer for proper UTF-8 encoding */
	    if (!g_utf8_validate(speechd_socket->o_buf->str, speechd_socket->o_bytes, NULL))
	      {
		MSG(4, "ERROR: Invalid character encoding on input (failed UTF-8 validation)");
		MSG(4, "Rejecting this message.");
		return strdup(ERR_INVALID_ENCODING);
	      }

            new = (TSpeechDMessage*) spd_malloc(sizeof(TSpeechDMessage));
            new->bytes = speechd_socket->o_bytes;
            assert(speechd_socket->o_buf != NULL);
            new->buf = deescape_dot(speechd_socket->o_buf->str, new->bytes);
            reparted = speechd_socket->inside_block; 
            MSG(5, "New buf is now: |%s|", new->buf);		
            if((msg_uid = queue_message(new, fd, 1, MSGTYPE_TEXT, reparted)) == 0){
                if(SPEECHD_DEBUG) FATAL("Can't queue message\n");
                free(new->buf);
                free(new);
                return strdup(ERR_INTERNAL);
            }			       

            /* Clear the counter of bytes in the output buffer. */
            server_data_off(fd);
	    ok_queued_reply = g_string_new("");
	    g_string_printf(ok_queued_reply, C_OK_MESSAGE_QUEUED"-%d\r\n"OK_MESSAGE_QUEUED, msg_uid);
	    reply = ok_queued_reply->str;
	    g_string_free(ok_queued_reply, 0);
            return reply;
        }

        {
            int real_bytes;
            if(bytes>=5){
	      if ( (pos = strstr(buf,"\r\n.\r\n")) ){	
                    real_bytes=pos-buf;
                    end_data=1;		
                    MSG(5,"Command in data caught");
                }else{
                    real_bytes = bytes;
                }
            }else{
                real_bytes = bytes;
            }      
            /* Get the number of bytes read before, sum it with the number of bytes read
             * now and store again in the counter */        
            speechd_socket->o_bytes += real_bytes;       

            g_string_insert_len(speechd_socket->o_buf, -1, buf, real_bytes);
        }
    }

    if (end_data == 1) goto enddata;

    /* Don't reply on data */
    return strdup("999 DATA");

}
#undef CHECK_SSIP_COMMAND

#define CHECK_PARAM(param) \
    if (param == NULL){ \
       MSG(4, "Missing parameter from client"); \
       return strdup(ERR_MISSING_PARAMETER); \
    } 

#define GET_PARAM_INT(name, pos) \
   { \
       char *helper; \
       helper = get_param(buf, pos, bytes, 0); \
       CHECK_PARAM(helper); \
       if (!isanum(helper)){ \
	   spd_free(helper); \
	   return strdup(ERR_NOT_A_NUMBER); \
       } \
       name = atoi(helper); \
       spd_free(helper); \
   }

#define CONV_DOWN 1
#define NO_CONV 0

#define GET_PARAM_STR(name, pos, up_lo_case) \
       name = get_param(buf, pos, bytes, up_lo_case); \
       CHECK_PARAM(name);

/* Tests if cmd is the same as str AND deallocates cmd if
   the test is succesful */
#define TEST_CMD(cmd, str) \
    (!strcmp(cmd, str) ? spd_free(cmd), 1 : 0 )

/* Parses @history commands and calls the appropriate history_ functions. */
char*
parse_history(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    char *cmd_main;
    GET_PARAM_STR(cmd_main, 1, CONV_DOWN);

    if (TEST_CMD(cmd_main, "get")){
        char *hist_get_sub;
        GET_PARAM_STR(hist_get_sub, 2, CONV_DOWN);

        if (TEST_CMD(hist_get_sub, "client_list")){
            return (char*) history_get_client_list();
        }  
        else if (TEST_CMD(hist_get_sub, "client_id")){
            return (char*) history_get_client_id(fd);
        }  
        else if (TEST_CMD(hist_get_sub, "client_messages")){
            int start, num;
            char *who;
	    int who_id;

            /* TODO: This needs to be (sim || am)-plified */
            who = get_param(buf,3,bytes, 1);
            CHECK_PARAM(who);
            if (!strcmp(who, "self")) return strdup(ERR_NOT_IMPLEMENTED);
            if (!strcmp(who, "all")) return strdup(ERR_NOT_IMPLEMENTED);                         
            if (!isanum(who)) return strdup(ERR_NOT_A_NUMBER);           
	    who_id = atoi(who);
	    spd_free(who);	    
            GET_PARAM_INT(start, 4);
            GET_PARAM_INT(num, 5);
            return (char*) history_get_message_list(who_id, start, num);
        }  
        else if (TEST_CMD(hist_get_sub, "last")){
            return (char*) history_get_last(fd);
        }
        else if (TEST_CMD(hist_get_sub, "message")){
            int msg_id;
            GET_PARAM_INT(msg_id, 3);
            return (char*) history_get_message(msg_id);
        }else{
            return strdup(ERR_MISSING_PARAMETER);
        }
    }
    else if (TEST_CMD(cmd_main, "cursor")){       
        char *hist_cur_sub;
        GET_PARAM_STR(hist_cur_sub, 2, CONV_DOWN);

        if (TEST_CMD(hist_cur_sub, "set")){
            int who;
            char *location;

            GET_PARAM_INT(who, 3);
            GET_PARAM_STR(location, 4, CONV_DOWN);            

            if (TEST_CMD(location,"last")){
                return (char*) history_cursor_set_last(fd, who);
            }
            else if (TEST_CMD(location,"first")){
                return (char*) history_cursor_set_first(fd, who);
            }
            else if (TEST_CMD(location,"pos")){
                int pos;
                GET_PARAM_INT(pos, 5);
                return (char*) history_cursor_set_pos(fd, who, pos);
            }
            else{
                spd_free(location);
                return strdup(ERR_MISSING_PARAMETER);
            }
        }
        else if (TEST_CMD(hist_cur_sub, "forward")){
            return (char*) history_cursor_forward(fd);
        }
        else if (TEST_CMD(hist_cur_sub,"backward")){
            return (char*) history_cursor_backward(fd);
        }
        else if (TEST_CMD(hist_cur_sub,"get")){
            return (char*) history_cursor_get(fd);
        }else{
            spd_free(hist_cur_sub);
            return strdup(ERR_MISSING_PARAMETER);
        }
            
    }
    else if (TEST_CMD(cmd_main,"say")){
        int msg_id;
        GET_PARAM_INT(msg_id, 2);
        return (char*) history_say_id(fd, msg_id);
    }
    else if (TEST_CMD(cmd_main,"sort")){
        // TODO: everything :)
        return strdup(ERR_NOT_IMPLEMENTED);
    }
    else{
        spd_free(cmd_main);
        return strdup(ERR_MISSING_PARAMETER);
    }

 
    return strdup(ERR_INVALID_COMMAND);
}

#define SSIP_SET_COMMAND(param) \
        if (who == 0) ret = set_ ## param ## _self(fd, param); \
        else if (who == 1) ret = set_ ## param ## _uid(uid, param); \
        else if (who == 2) ret = set_ ## param ## _all(param); \

#define SSIP_ON_OFF_PARAM(param, ok_message, err_message, inside_block) \
    if (!strcmp(set_sub, #param)){ \
	char *helper_s; \
	int param; \
\
        inside_block \
\
        GET_PARAM_STR(helper_s, 3, CONV_DOWN); \
\
        if(TEST_CMD(helper_s, "on")) param = 1; \
        else if(TEST_CMD(helper_s, "off")) param = 0; \
        else{ \
            spd_free(helper_s); \
            return strdup(ERR_PARAMETER_NOT_ON_OFF); \
        } \
        SSIP_SET_COMMAND(param); \
        if (ret) return strdup(err_message); \
        return strdup(ok_message); \
    }

char*
parse_set(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    int who;                    /* 0 - self, 1 - uid specified, 2 - all */
    int uid;                    /* uid of the client (only if who == 1) */
    int ret = -1; // =-1 has no effect but avoids gcc warning  
    char *set_sub;
    char *who_s;

    GET_PARAM_STR(who_s, 1, CONV_DOWN);

    if (TEST_CMD(who_s, "self")) who = 0;
    else if (TEST_CMD(who_s, "all")) who = 2;
    else if (isanum(who_s)){
        who = 1;
        uid = atoi(who_s);
	spd_free(who_s);
    }else{
        spd_free(who_s);
        return strdup(ERR_PARAMETER_INVALID);
    }

    GET_PARAM_STR(set_sub, 2, CONV_DOWN);

    if (TEST_CMD(set_sub, "priority")){
        char *priority_s;
        int priority;
        NOT_ALLOWED_INSIDE_BLOCK();
        
        /* Setting priority only allowed for "self" */
        if (who != 0) return strdup(ERR_COULDNT_SET_PRIORITY); 
        GET_PARAM_STR(priority_s, 3, CONV_DOWN);

        if (TEST_CMD(priority_s, "important")) priority = 1;
        else if (TEST_CMD(priority_s, "text")) priority = 2;
        else if (TEST_CMD(priority_s, "message")) priority = 3;
        else if (TEST_CMD(priority_s, "notification")) priority = 4;
        else if (TEST_CMD(priority_s, "progress")) priority = 5;
        else{
            spd_free(priority_s);
            return strdup(ERR_UNKNOWN_PRIORITY);
        }

        ret = set_priority_self(fd, priority);
        if (ret) return strdup(ERR_COULDNT_SET_PRIORITY);	
        return strdup(OK_PRIORITY_SET);
    }
    else if (TEST_CMD(set_sub, "language")){
        char *language;

        GET_PARAM_STR(language, 3, CONV_DOWN);

        SSIP_SET_COMMAND(language);
	spd_free(language);

        if (ret) return strdup(ERR_COULDNT_SET_LANGUAGE);
        return strdup(OK_LANGUAGE_SET);
    }
    else if (TEST_CMD(set_sub, "synthesis_voice")){
        char *synthesis_voice;

        GET_PARAM_STR(synthesis_voice, 3, CONV_DOWN);

        SSIP_SET_COMMAND(synthesis_voice);
	spd_free(synthesis_voice);

        if (ret) return strdup(ERR_COULDNT_SET_VOICE);
        return strdup(OK_VOICE_SET);
    }
    else if (TEST_CMD(set_sub, "client_name")){
        char *client_name;
        NOT_ALLOWED_INSIDE_BLOCK();

        /* Setting client name only allowed for "self" */
        if (who != 0) return strdup(ERR_PARAMETER_INVALID);
      
        GET_PARAM_STR(client_name, 3, CONV_DOWN);

        ret = set_client_name_self(fd, client_name);       
        spd_free(client_name);

        if (ret) return strdup(ERR_COULDNT_SET_CLIENT_NAME);
        return strdup(OK_CLIENT_NAME_SET);
    }
    else if (TEST_CMD(set_sub, "rate")){
        signed int rate;
        GET_PARAM_INT(rate, 3);

        if(rate < -100) return strdup(ERR_RATE_TOO_LOW);
        if(rate > +100) return strdup(ERR_RATE_TOO_HIGH);

        SSIP_SET_COMMAND(rate);
        if (ret) return strdup(ERR_COULDNT_SET_RATE);
        return strdup(OK_RATE_SET);
    }
    else if (TEST_CMD(set_sub, "pitch")){
        signed int pitch;
        GET_PARAM_INT(pitch, 3);

        if(pitch < -100) return strdup(ERR_PITCH_TOO_LOW);
        if(pitch > +100) return strdup(ERR_PITCH_TOO_HIGH);

        SSIP_SET_COMMAND(pitch);
        if (ret) return strdup(ERR_COULDNT_SET_PITCH);
        return strdup(OK_PITCH_SET);
    }
    else if (TEST_CMD(set_sub, "volume")){
        signed int volume;
        GET_PARAM_INT(volume, 3);

        if(volume < -100) return strdup(ERR_VOLUME_TOO_LOW);
        if(volume > +100) return strdup(ERR_VOLUME_TOO_HIGH);

        SSIP_SET_COMMAND(volume);
        if (ret) return strdup(ERR_COULDNT_SET_VOLUME);
        return strdup(OK_VOLUME_SET);
    }
    else if (TEST_CMD(set_sub, "voice")){
        char *voice;
        GET_PARAM_STR(voice, 3, CONV_DOWN);

        SSIP_SET_COMMAND(voice);
        spd_free(voice);

        if (ret) return strdup(ERR_COULDNT_SET_VOICE);
        return strdup(OK_VOICE_SET);
    }
    else if (TEST_CMD(set_sub, "punctuation")){
        char *punct_s;
        EPunctMode punctuation_mode;

        NOT_ALLOWED_INSIDE_BLOCK();
        GET_PARAM_STR(punct_s, 3, CONV_DOWN);

        if(TEST_CMD(punct_s,"all")) punctuation_mode = PUNCT_ALL;
        else if(TEST_CMD(punct_s,"some")) punctuation_mode = PUNCT_SOME;        
        else if(TEST_CMD(punct_s,"none")) punctuation_mode = PUNCT_NONE;        
        else{
            spd_free(punct_s);
            return strdup(ERR_PARAMETER_INVALID);
        }

        SSIP_SET_COMMAND(punctuation_mode);

        if (ret) return strdup(ERR_COULDNT_SET_PUNCT_MODE);
        return strdup(OK_PUNCT_MODE_SET);
    }
    else if (TEST_CMD(set_sub, "output_module")){
        char *output_module;
        NOT_ALLOWED_INSIDE_BLOCK();
        GET_PARAM_STR(output_module, 3, CONV_DOWN);

        SSIP_SET_COMMAND(output_module);
        spd_free(output_module);

        if (ret) return strdup(ERR_COULDNT_SET_OUTPUT_MODULE);
        return strdup(OK_OUTPUT_MODULE_SET);
    }
    else if (TEST_CMD(set_sub, "cap_let_recogn")){
        int capital_letter_recognition;
        char *recognition;
        NOT_ALLOWED_INSIDE_BLOCK();
        GET_PARAM_STR(recognition, 3, CONV_DOWN);

        if(TEST_CMD(recognition, "none")) capital_letter_recognition = RECOGN_NONE;
        else if(TEST_CMD(recognition, "spell")) capital_letter_recognition = RECOGN_SPELL;        
        else if(TEST_CMD(recognition, "icon")) capital_letter_recognition = RECOGN_ICON;        
        else{
            spd_free(recognition);
            return strdup(ERR_PARAMETER_INVALID);
        }

        SSIP_SET_COMMAND(capital_letter_recognition);

        if (ret) return strdup(ERR_COULDNT_SET_CAP_LET_RECOG);
        return strdup(OK_CAP_LET_RECOGN_SET);
    }
    else if (TEST_CMD(set_sub, "pause_context")){
        int pause_context;
        GET_PARAM_INT(pause_context, 3);

        SSIP_SET_COMMAND(pause_context);
        if (ret) return strdup(ERR_COULDNT_SET_PAUSE_CONTEXT);
        return strdup(OK_PAUSE_CONTEXT_SET);
    }
    else SSIP_ON_OFF_PARAM(spelling,
	    OK_SPELLING_SET, ERR_COULDNT_SET_SPELLING,
	    NOT_ALLOWED_INSIDE_BLOCK())
    else SSIP_ON_OFF_PARAM(ssml_mode,
	    OK_SSML_MODE_SET, ERR_COULDNT_SET_SSML_MODE,
	    ALLOWED_INSIDE_BLOCK())
    else SSIP_ON_OFF_PARAM(debug, g_strdup_printf("262-%s\r\n"OK_DEBUGGING, SpeechdOptions.debug_destination),
            ERR_COULDNT_SET_DEBUGGING, ;)
    else if (TEST_CMD(set_sub, "notification")){
	char *scope;
        char *par_s;
	int par;

	if (who != 0) return strdup(ERR_PARAMETER_INVALID);

        GET_PARAM_STR(scope, 3, CONV_DOWN);
        GET_PARAM_STR(par_s, 4, CONV_DOWN);

        if(TEST_CMD(par_s,"on")) par = 1;
        else if(TEST_CMD(par_s,"off")) par = 0;        
        else{	    
            spd_free(par_s);
            return strdup(ERR_PARAMETER_INVALID);
        }

	ret = set_notification_self(fd, scope, par);
	spd_free(scope);
	           
        if (ret) return strdup(ERR_COULDNT_SET_NOTIFICATION);
        return strdup(OK_NOTIFICATION_SET);
    }
    else{
	spd_free(set_sub);
        return strdup(ERR_PARAMETER_INVALID);
    }

    return strdup(ERR_INVALID_COMMAND);
}
#undef SSIP_SET_COMMAND

char*
parse_stop(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    int uid = 0;
    char *who_s;

    MSG(5, "Stop received from fd %d.", fd);

    GET_PARAM_STR(who_s, 1, CONV_DOWN);

    if (TEST_CMD(who_s, "all")){
	pthread_mutex_lock(&element_free_mutex);
        speaking_stop_all();
	pthread_mutex_unlock(&element_free_mutex);	
    }
    else if (TEST_CMD(who_s, "self")){
        uid = get_client_uid_by_fd(fd);
        if(uid == 0) return strdup(ERR_INTERNAL);
	pthread_mutex_lock(&element_free_mutex);    
        speaking_stop(uid);
	pthread_mutex_unlock(&element_free_mutex);    
    }
    else if (isanum(who_s)){
        uid = atoi(who_s);
        spd_free(who_s);

        if (uid <= 0) return strdup(ERR_ID_NOT_EXIST);
	pthread_mutex_lock(&element_free_mutex);    
        speaking_stop(uid);
	pthread_mutex_unlock(&element_free_mutex);    
    }else{
        spd_free(who_s);
        return strdup(ERR_PARAMETER_INVALID);
    }

    return strdup(OK_STOPPED);
}

char*
parse_cancel(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    int uid = 0;
    char *who_s;

    MSG(4, "Cancel received from fd %d.", fd);

    GET_PARAM_STR(who_s, 1, CONV_DOWN);

    if (TEST_CMD(who_s,"all")){
        speaking_cancel_all();
    }
    else if (TEST_CMD(who_s, "self")){
        uid = get_client_uid_by_fd(fd);
        if(uid == 0) return strdup(ERR_INTERNAL);
        speaking_cancel(uid);
    }
    else if (isanum(who_s)){
        uid = atoi(who_s);
        spd_free(who_s);

        if (uid <= 0) return strdup(ERR_ID_NOT_EXIST);
        speaking_cancel(uid);
    }else{
        spd_free(who_s);
        return strdup(ERR_PARAMETER_INVALID);
    }

    return strdup(OK_CANCELED);
}

char*
parse_pause(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    int uid = 0;
    char *who_s;

    MSG(4, "Pause received from fd %d.", fd);

    GET_PARAM_STR(who_s, 1, CONV_DOWN);

    /* Note: In this case, the semaphore has a special meaning
       to allow the speaking loop detect the request for pause */

    if (TEST_CMD(who_s, "all")){
        pause_requested = 1;
        pause_requested_fd = fd;
        speaking_semaphore_post();
    }
    else if (TEST_CMD(who_s, "self")){
        uid = get_client_uid_by_fd(fd);
        if(uid == 0) return strdup(ERR_INTERNAL);
        pause_requested = 2;
        pause_requested_fd = fd;
        pause_requested_uid = uid;
        speaking_semaphore_post();
    }
    else if (isanum(who_s)){
        uid = atoi(who_s);
        spd_free(who_s);
        if (uid <= 0) return strdup(ERR_ID_NOT_EXIST);
        pause_requested = 2;
        pause_requested_fd = fd;
        pause_requested_uid = uid;
        speaking_semaphore_post();
    }else{
        spd_free(who_s);
        return strdup(ERR_PARAMETER_INVALID);
    }

    return strdup(OK_PAUSED);
}

char*
parse_resume(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    int uid = 0;
    char *who_s;

    MSG(4, "Resume received from fd %d.", fd);

    GET_PARAM_STR(who_s, 1, CONV_DOWN);

    if (TEST_CMD(who_s, "all")){
        speaking_resume_all();
    }
    else if (TEST_CMD(who_s, "self")){
        uid = get_client_uid_by_fd(fd);
        if(uid == 0) return strdup(ERR_INTERNAL);
        speaking_resume(uid);
    }
    else if (isanum(who_s)){
        uid = atoi(who_s);
        spd_free(who_s);
        if (uid <= 0) return strdup(ERR_ID_NOT_EXIST);
        speaking_resume(uid);
    }else{
        spd_free(who_s);
        return strdup(ERR_PARAMETER_INVALID);
    }
    
    return strdup(OK_RESUMED);
}

char*
parse_general_event(const char *buf, const int bytes, const int fd, const TSpeechDSock* speechd_socket, EMessageType type)
{
    char *param;
    TSpeechDMessage *msg;

    GET_PARAM_STR(param, 1, NO_CONV);

    if (param == NULL)	return strdup(ERR_MISSING_PARAMETER);
    
    if (param[0] == 0){
	spd_free(param);
	return strdup(ERR_MISSING_PARAMETER);
    }

    /* Check for proper UTF-8 */
    /* Check buffer for proper UTF-8 encoding */
    if (!g_utf8_validate(buf, bytes, NULL))
      {
	MSG(4, "ERROR: Invalid character encoding on event input (failed UTF-8 validation)");
	MSG(4, "Rejecting this event (char/key/sound_icon).");
	return strdup(ERR_INVALID_ENCODING);
      }


    msg = (TSpeechDMessage*) spd_malloc(sizeof(TSpeechDMessage));
    msg->bytes = strlen(param);
    msg->buf = strdup(param);

    if(queue_message(msg, fd, 1, type, speechd_socket->inside_block) == 0){
        if (SPEECHD_DEBUG) FATAL("Couldn't queue message\n");
        MSG(2, "Error: Couldn't queue message!\n");            
    }   

    spd_free(param);

    return strdup(OK_MESSAGE_QUEUED);
}

char*
parse_snd_icon(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    return parse_general_event(buf, bytes, fd, speechd_socket, MSGTYPE_SOUND_ICON);
}				 

char*
parse_char(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    return parse_general_event(buf, bytes, fd, speechd_socket, MSGTYPE_CHAR);
}

char*
parse_key(const char* buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    return parse_general_event(buf, bytes, fd, speechd_socket, MSGTYPE_KEY);
}

char*
parse_list(const char* buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    char *list_type;
    char *voice_list;

    GET_PARAM_STR(list_type, 1, CONV_DOWN);

    if(TEST_CMD(list_type, "voices")){
        voice_list = (char*) spd_malloc(1024);
        sprintf(voice_list,
                C_OK_VOICES"-MALE1\r\n"
                C_OK_VOICES"-MALE2\r\n"
                C_OK_VOICES"-MALE3\r\n"
                C_OK_VOICES"-FEMALE1\r\n"
                C_OK_VOICES"-FEMALE2\r\n"
                C_OK_VOICES"-FEMALE3\r\n"
                C_OK_VOICES"-CHILD_MALE\r\n"
                C_OK_VOICES"-CHILD_FEMALE\r\n"
                OK_VOICE_LIST_SENT);
        return voice_list;
    }else if(TEST_CMD(list_type, "output_modules")){
        GString *result = g_string_new("");
	char *helper;
	GList *cur_ptr = output_modules_list;

	while(cur_ptr){
	    if (cur_ptr->data != NULL){
	        g_string_append_printf(result, C_OK_MODULES"-%s\r\n", (char*) cur_ptr->data);
	    }else{
                MSG(2, "null entry in output modules list");
	    }

            cur_ptr = cur_ptr->next;
        }

	g_string_append(result, OK_MODULES_LIST_SENT);
	helper = result->str;
	g_string_free(result, 0);

        return helper;
    }else if(TEST_CMD(list_type, "synthesis_voices")){
      char *module_name;
      int uid;
      TFDSetElement *settings;
      VoiceDescription **voices;
      GString *result;
      int i;
      char *helper;
      
      uid = get_client_uid_by_fd(fd);		       
      settings = get_client_settings_by_uid(uid);
      if (settings == NULL) return strdup(ERR_INTERNAL);
      module_name = settings->output_module;
      if (module_name == NULL) return strdup(ERR_NO_OUTPUT_MODULE);
      voices = output_list_voices(module_name);
      if (voices == NULL)  return strdup(ERR_CANT_REPORT_VOICES);

      result = g_string_new("");
      for (i=0; ; i++){
	if (voices[i] == NULL) break;
	g_string_append_printf(result, C_OK_VOICES"-%s %s %s\r\n",
			       voices[i]->name, voices[i]->language, voices[i]->dialect);	
      }
      g_string_append(result, OK_VOICE_LIST_SENT);
      helper = result->str;
      g_string_free(result, 0);	
      return helper;      
    }else{
      spd_free(list_type);
      return strdup(ERR_PARAMETER_INVALID);
    }
}

char*
parse_get(const char *buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    char *get_type;
    GString *result;
    char *helper;

    TFDSetElement *settings;

    settings = get_client_settings_by_fd(fd);
    if (settings == NULL) 
        return strdup(ERR_INTERNAL);

    GET_PARAM_STR(get_type, 1, CONV_DOWN);
    if(TEST_CMD(get_type, "voice")) {
        result = g_string_new("");

        switch (settings->voice)
        {
            case MALE1:
                g_string_append_printf(result, C_OK_GET"-MALE1\r\n");
                break;
            case MALE2:
                g_string_append_printf(result, C_OK_GET"-MALE2\r\n");
                break;
            case MALE3:
                g_string_append_printf(result, C_OK_GET"-MALE3\r\n");
                break;
            case FEMALE1:
                g_string_append_printf(result, C_OK_GET"-FEMALE1\r\n");
                break;
            case FEMALE2:
                g_string_append_printf(result, C_OK_GET"-FEMALE2\r\n");
                break;
            case FEMALE3:
                g_string_append_printf(result, C_OK_GET"-FEMALE3\r\n");
                break;
            case CHILD_MALE:
                g_string_append_printf(result, C_OK_GET"-CHILD_MALE\r\n");
                break;
            case CHILD_FEMALE:
                g_string_append_printf(result, C_OK_GET"-CHILD_FEMALE\r\n");
                break;
            case NO_VOICE:
            default:
                g_string_append_printf(result, C_OK_GET"-NO_VOICE\r\n");
                break;
        }
        helper = result->str;
        g_string_free(result, 0);
        return helper;
    }
    else if (TEST_CMD(get_type, "output_module")) {
        result = g_string_new("");
        g_string_append_printf(result, C_OK_GET"-%s\r\n",
                       settings->output_module);
        helper = result->str;
        g_string_free(result, 0);
        return helper;      
    }
    else{
      spd_free(get_type);
      return strdup(ERR_PARAMETER_INVALID);
    }
}

char*
parse_help(const char* buf, const int bytes, const int fd, const TSpeechDSock *speechd_socket)
{
    char *help;

    help = (char*) spd_malloc(1024 * sizeof(char));

    sprintf(help, 
            C_OK_HELP"-  SPEAK           -- say text \r\n"
            C_OK_HELP"-  KEY             -- say a combination of keys \r\n"
            C_OK_HELP"-  CHAR            -- say a character \r\n"
            C_OK_HELP"-  SOUND_ICON      -- execute a sound icon \r\n"
            C_OK_HELP"-  SET             -- set a parameter \r\n"
            C_OK_HELP"-  GET             -- get a current parameter \r\n"
            C_OK_HELP"-  LIST            -- list available arguments \r\n"
            C_OK_HELP"-  HISTORY         -- commands related to history \r\n"
            C_OK_HELP"-  QUIT            -- close the connection \r\n"
            OK_HELP_SENT);

    return help;
}

char*
parse_block(const char *buf, const int bytes, const int fd, TSpeechDSock* speechd_socket)
{
    char *cmd_main;
    GET_PARAM_STR(cmd_main, 1, CONV_DOWN);

    if (TEST_CMD(cmd_main, "begin")){
        assert(speechd_socket->inside_block >= 0);
        if (speechd_socket->inside_block == 0){
            speechd_socket->inside_block = ++SpeechdStatus.max_gid;
            return strdup(OK_INSIDE_BLOCK);
        }else{
            return strdup(ERR_ALREADY_INSIDE_BLOCK);
        }        
    }
    else if (TEST_CMD(cmd_main, "end")){
        assert(speechd_socket->inside_block >= 0);
        if (speechd_socket->inside_block > 0){
            speechd_socket->inside_block = 0;
            return strdup(OK_OUTSIDE_BLOCK);
        }else{
            return strdup(ERR_ALREADY_OUTSIDE_BLOCK);
        }
    }
    else{
	spd_free(cmd_main);
	return strdup(ERR_PARAMETER_INVALID);
    }
}
   
/*
     * deescape_dot: Replace .. with . at the start of lines or at the
     * start of the string.
 * @orig_text: text to be unescaped.
  * @orig_len: length of the text.
 * Returns: a freshly allocated string, containing the unescaped data.
 *
 * In SSIP, the message terminator is \r\n.\r\n, just as it is in SMTP
 * and similar protocols.  Thus, period needs to be escaped when it
 * is the only character on a line.  deescape_dot reverts that
 * transformation, after the message is received.
 * This function deserves further examination.
 */

char*
deescape_dot(const char *orig_text, size_t orig_len)
{
    /* Constants.  DOTLINE is CRLF followed by a period.
     * DOTLINELEN is the length of DOTLINE.
     * ESCAPED_DOTLINELEN is the length of the sequence \r\n..,
     * which is used in the original (unescaped) text.
     */
    static const char *DOTLINE = "\r\n.";
    static const size_t DOTLINELEN = 3;
    static const size_t ESCAPED_DOTLINELEN = 4;	/* \r\n.. */

    char *out_text = NULL;
    char *out_ptr;
    const char *orig_end = orig_text + orig_len;

    if (orig_text == NULL)
        return NULL;

    out_text = spd_malloc(orig_len + 1);
    /* We may have allocated more than we need.  In any case, out_text
     * can be no longer than orig_text.
     * Note: spd_malloc aborts the program on failure to allocate. */

    out_ptr = out_text;
    if (orig_len >= 2) {
        /* De-escape .. at start of text. */
        if ((orig_text[0] == '.') && (orig_text[1] == '.')) {
            *(out_ptr++) = '.';
            orig_text = orig_text+2;
        }
    }

    while (orig_text < orig_end) {
        if ((orig_text[0] == '\r') && (orig_text[1] == '\n')
                && (orig_text[2] == '.') && (orig_text[3] == '.')) {
            /* We just found \r\n.., the sequence we want to unescape. */
            memcpy(out_ptr, DOTLINE, DOTLINELEN);
            out_ptr += DOTLINELEN;
            orig_text += ESCAPED_DOTLINELEN;
        } else {
            /* Just copy the character from source to destination... */
            *(out_ptr++) = *(orig_text++);
        }
    }

    *out_ptr = '\0';	/* NUL-terminate. */
    return out_text;
}

/* isanum() tests if the given string is a number,
 * returns 1 if yes, 0 otherwise. */
int
isanum(const char *str){
   int i;
   if (!isdigit(str[0]) && !( (str[0]=='+') || (str[0]=='-'))) return 0;
   for(i=1;i<=strlen(str)-1;i++){
       if (!isdigit(str[i]))   return 0;
    }
    return 1;
}

/* Gets command parameter _n_ from the text buffer _buf_
 * which has _bytes_ bytes. Note that the parameter with
 * index 0 is the command itself. */
char* 
get_param(const char *buf, const int n, const int bytes, const int lower_case)
{
    char* param;
    char* par;
    int i, y, z = 0;

    assert (bytes != 0);
    param = (char*) spd_malloc(bytes);
    assert(param != NULL);
	
    strcpy(param,"");
    i = 0;
        
    /* Read all the parameters one by one,
     * but stop after the one with index n,
     * while maintaining it's value in _param_ */
    for(y=0; y<=n; y++){
        z=0;
        for(; i<bytes; i++){
            if (buf[i] == ' ') break;
            param[z] = buf[i];
            z++;
        }
        i++;
    }

    if(z <= 0){
        spd_free(param);
        return NULL;   
    }

    /* Write the trailing zero */
    if (i >= bytes){
        param[z>1?z-2:0] = 0;
    }else{
        param[z] = 0;
    }

    if(lower_case){
        par = g_ascii_strdown(param, strlen(param));
	spd_free(param);
    }else{
	par = param;
    }   

    return par;
}

/* Read one char  (which _pointer_ is pointing to) from an UTF-8 string
 * and store it into _character_. _character_ must have space for
 * at least  7 bytes (6 bytes character + 1 byte trailing 0). This
 * function doesn't validate if the string is valid UTF-8.
 */
int
spd_utf8_read_char(char* pointer, char* character)
{
    int bytes;
    gunichar u_char;

    u_char = g_utf8_get_char(pointer);
    bytes = g_unichar_to_utf8(u_char, character);
    character[bytes]=0;

    return bytes;
}
