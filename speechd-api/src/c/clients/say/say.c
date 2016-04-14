
/*
 * say.c - Super-simple Speech Dispatcher client
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
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: say.c,v 1.16 2007-05-03 09:43:12 hanke Exp $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <semaphore.h>
#include <errno.h>
#include <getopt.h>
#include "libspeechd.h"
#include "options.h"


#define MAX_LINELEN 16384

sem_t semaphore;

/* Callback for Speech Dispatcher notifications */
void end_of_speech(size_t msg_id, size_t client_id, SPDNotificationType type)
{
    sem_post(&semaphore);
}

int main(int argc, char **argv) {
    SPDConnection *conn;
    SPDPriority spd_priority;
    int err;
    char *error;
    int msg_arg_required = 0;
    int ret;
    int option_ret;
    char *line;

    rate = -101;
    pitch = -101;
    volume = -101;
    language = NULL;
    voice_type = NULL;
    punctuation_mode = NULL;
    spelling = -2;
    ssml_mode = 0;
    wait_till_end = 0;
    stop_previous = 0;
    cancel_previous = 0;
    pipe_mode = 0;
    priority = NULL;
    application_name = NULL;
    connection_name = NULL;

    option_ret = options_parse(argc, argv);

    /* Check if the text to say or options are specified in the argument */
    msg_arg_required = (pipe_mode != 1) && (stop_previous != 1)
                       && (cancel_previous != 1);
    if ((optind >= argc) && msg_arg_required) {
        options_print_help(argv);
        return 1;
    }

    /* Open a connection to Speech Dispatcher */
    conn = spd_open2(application_name ? application_name : "spd-say",
                    connection_name ? connection_name : "main",
		     NULL, SPD_MODE_THREADED, NULL, 1, &error);
    if (conn == NULL){
      fprintf(stderr, "Failed to connect to Speech Dispatcher:\n%s\n", error);
      exit(1);
    }

    if (stop_previous) spd_stop_all(conn);
    if (cancel_previous) spd_cancel_all(conn);

    /* Set the desired parameters */

    if (language != NULL)
        if(spd_set_language(conn, language))
            printf("Invalid language!\n");

    if (output_module != NULL)
        if(spd_set_output_module(conn, output_module))
            printf("Invalid output module!\n");

    if (voice_type != NULL){
        if (!strcmp(voice_type, "male1")){
            if(spd_set_voice_type(conn, SPD_MALE1))
                printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "male2")){
            if(spd_set_voice_type(conn, SPD_MALE2))
                printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "male3")){
            if(spd_set_voice_type(conn, SPD_MALE3))
            printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "female1")){
            if(spd_set_voice_type(conn, SPD_FEMALE1))
                printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "female2")){
            if(spd_set_voice_type(conn, SPD_FEMALE2))
                printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "female3")){
            if(spd_set_voice_type(conn, SPD_FEMALE3))
                printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "child_male")){
            if(spd_set_voice_type(conn, SPD_CHILD_MALE))
                printf("Can't set this voice!\n");
        }
        else if(!strcmp(voice_type, "child_female")){
            if(spd_set_voice_type(conn, SPD_CHILD_FEMALE))
                printf("Can't set this voice!\n");
        }else{
            printf("Invalid voice\n");
        }
    }

    if (ssml_mode)
        if(spd_execute_command(conn, "SET SELF SSML_MODE ON"))
            printf("Failed to set SSML mode.\n");

    if (rate != -101)
        if(spd_set_voice_rate(conn, rate))
            printf("Invalid rate!\n");

    if (pitch != -101)
        if(spd_set_voice_pitch(conn, pitch))
            printf("Invalid pitch!\n");

    if (volume != -101)
        if(spd_set_volume(conn, volume))
            printf("Invalid volume!\n");

    if (spelling == 1)
        if(spd_set_spelling(conn, SPD_SPELL_ON))
            printf("Can't set spelling to on!\n");

    if (punctuation_mode != NULL){
        if (!strcmp(punctuation_mode, "none")){
            if(spd_set_punctuation(conn, SPD_PUNCT_NONE))
                printf("Can't set this punctuation mode!\n");
        }
        else if(!strcmp(punctuation_mode, "some")){
            if(spd_set_punctuation(conn, SPD_PUNCT_SOME))
                printf("Can't set this punctuation mode!\n");
        }
        else if(!strcmp(punctuation_mode, "all")){
            if(spd_set_punctuation(conn, SPD_PUNCT_ALL))
                printf("Can't set this punctuation mode!\n");
        }else{
            printf("Invalid punctuation mode.\n");
        }
    }

    /* Set default priority... */
    if (1 == pipe_mode)
        spd_priority = SPD_MESSAGE;
    else
        spd_priority = SPD_TEXT;
    /* ...and look if it wasn't overriden */
    if (priority != NULL){
        if (!strcmp(priority, "important")) spd_priority = SPD_IMPORTANT;
        else if (!strcmp(priority, "message")) spd_priority = SPD_MESSAGE;
        else if (!strcmp(priority, "text")) spd_priority = SPD_TEXT;
        else if (!strcmp(priority, "notification")) spd_priority = SPD_NOTIFICATION;
        else if (!strcmp(priority, "progress")) spd_priority = SPD_PROGRESS;
        else{
            printf("Invalid priority.\n");
        }
    }

    if (wait_till_end){
        ret = sem_init(&semaphore, 0, 0);
        if (ret < 0){
            fprintf(stderr, "Can't initialize semaphore: %s", strerror(errno));
            return 0;
        }

        /* Notify when the message is canceled or the speech terminates */
        conn->callback_end = end_of_speech;
        conn->callback_cancel = end_of_speech;
        spd_set_notification_on(conn, SPD_END);
        spd_set_notification_on(conn, SPD_CANCEL);
    }

    /* In pipe mode, read from stdin, write to stdout, and also to Speech Dispatcher. */
    if (pipe_mode == 1) {
        line = (char *) malloc( MAX_LINELEN );
        while (NULL != fgets(line, MAX_LINELEN, stdin)) {
            fputs(line, stdout);
            if (0 == strncmp(line, "!-!", 3)) {
                /* Remove EOL */
                line[strlen(line)-1] = 0;
                spd_execute_command(conn, line + 3);
            } else {
                spd_say(conn, spd_priority, line);
                if (wait_till_end) sem_wait(&semaphore);
            }
        }
        free(line);

    } else {
        /* Say the message with priority "text" */
        /* Or do nothing in case of -C or -S with no message. */
        if (optind < argc) {
	  err = spd_sayf(conn, spd_priority, (char*) argv[optind]);
	  if (err == -1){
	    fprintf(stderr, "Speech Dispatcher failed to say message");
	    exit(1);
	  }

          /* Wait till the callback is called */
          if (wait_till_end) sem_wait(&semaphore);
        }
    }

    /* Close the connection */
    spd_close(conn);

    return 0;
}
