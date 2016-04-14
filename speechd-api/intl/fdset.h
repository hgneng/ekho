
/*
 * fdset.h - Settings for Speech Dispatcher
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
 * $Id: fdset.h,v 1.33 2008-06-09 10:28:08 hanke Exp $
 */

#ifndef FDSET_H
#define FDSET_H

typedef enum 
    {                  /* Type of voice */
	NO_VOICE = 0,
	MALE1 = 1,
	MALE2 = 2,
	MALE3 = 3,
	FEMALE1 = 4,
	FEMALE2 = 5,
	FEMALE3 = 6,
	CHILD_MALE = 7,
	CHILD_FEMALE = 8
    }EVoiceType;

typedef enum
    {
	SORT_BY_TIME = 0,
	SORT_BY_ALPHABET = 1
    }ESort;

typedef enum
    {
	MSGTYPE_TEXT = 0,
	MSGTYPE_SOUND_ICON = 1,
	MSGTYPE_CHAR = 2,
	MSGTYPE_KEY = 3,
	MSGTYPE_SPELL = 99
    }EMessageType;

typedef enum
    {
	RECOGN_NONE = 0,
	RECOGN_SPELL = 1,
	RECOGN_ICON = 2
    }ECapLetRecogn;

typedef enum
    {
	PUNCT_NONE = 0,
	PUNCT_ALL = 1,
	PUNCT_SOME = 2
    }EPunctMode;

typedef enum
    {
	SPELLING_OFF = 0,
	SPELLING_ON = 1
    }ESpellMode;

typedef enum
    {
	NOTIFY_NOTHING = 0,
	NOTIFY_BEGIN = 1,
	NOTIFY_END = 2,
	NOTIFY_IM = 4,
	NOTIFY_CANCEL = 8,
	NOTIFY_PAUSE = 16,
	NOTIFY_RESUME = 32
    }ENotification;

typedef struct {
  char* name;
  char* language;
  char* dialect;
}VoiceDescription;


typedef struct{
    unsigned int uid;		/* Unique ID of the client */
    int fd;                     /* File descriptor the client is on. */
    int active;                 /* Is this client still active on socket or gone?*/
    int paused;                 /* Internal flag, 1 for paused client or 0 for normal. */
    int paused_while_speaking;  
    EMessageType type;          /* Type of the message (1=text, 2=icon, 3=char, 4=key) */
    int ssml_mode;		/* SSML mode on (1)/off (0) */
    int priority;               /* Priority between 1 and 3 (1 - highest, 3 - lowest) */
    signed int rate; 		/* Speed of voice from <-100;+100>, 0 is the default */
    signed int pitch;		/* Pitch of voice from <-100;+100>, 0 is the default */
    signed int volume;		/* Volume of voice from <-100;+100), 0 is the default */
    EPunctMode punctuation_mode;	/* Punctuation mode: 0, 1 or 2
                                   0	-	no punctuation
                                   1 	-	all punctuation
                                   2	-	only user-selected punctuation */
    ESpellMode spelling_mode;   /* Spelling mode: 0 or 1 (0 - off, 1 - on) */
    char *client_name;		/* Name of the client. */
    char *language;             /* Selected language name. (e.g. "en", "cz", "fr", ...) */
    char *output_module;        /* Output module name. (e.g. "festival", "flite", "apollo", ...) */
    EVoiceType voice;           /* see EVoiceType definition above */
    char *synthesis_voice;
    ECapLetRecogn cap_let_recogn;         /* Capital letters recognition: (0 - off, 1 - on) */

    ENotification notification;	/* Notification about start and stop of messages, about reached 
				   index marks and state (canceled, paused, resumed). */

    int reparted;
    unsigned int min_delay_progress;
    int pause_context;          /* Number of words that should be repeated after a pause */
    char* index_mark;           /* Current index mark for the message (only if paused) */

    char* audio_output_method;
    char* audio_oss_device;
    char* audio_alsa_device;
    char* audio_nas_server;
    char* audio_pulse_server;
    int audio_pulse_min_length;
    int log_level;

    /* TODO: Should be moved out */
    unsigned int hist_cur_uid;
    int hist_cur_pos;
    ESort hist_sorted;

}TFDSetElement;

typedef struct{
    char *pattern;
    TFDSetElement val;
}TFDSetClientSpecific;

typedef struct{
    signed int rate;
    signed int pitch;
    signed int volume;
    
    EPunctMode punctuation_mode;
    ESpellMode spelling_mode;
    ECapLetRecogn cap_let_recogn;

    char* language;

    EVoiceType voice;
    char *synthesis_voice;
}SPDMsgSettings;

#endif /* not ifndef FDSET */
