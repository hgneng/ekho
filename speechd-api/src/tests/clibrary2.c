
/*
 * clibrary2.c - Testing LIST and associated set functions in Speech Dispatcher
 *
 * Copyright (C) 2008 Brailcom, o.p.s.
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
 * $Id: clibrary2.c,v 1.1 2008-04-09 11:41:52 hanke Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "libspeechd.h"
#include "def.h"

int main() {
   SPDConnection* conn;
   int i,j ;   
   char **modules;
   char **voices;
   SPDVoice **synth_voices;

   printf("Start of the test.\n");
   
   printf("Trying to initialize Speech Deamon...");
   conn = spd_open("say", NULL, NULL, SPD_MODE_SINGLE);
   if (conn == 0){
     printf("Speech Deamon failed");
     exit(1);
   }
   printf("OK\n");

   modules = spd_list_modules(conn);
   if (modules == NULL){
     printf("Can't list modules\n");
     exit(0);
   }

   printf("Available output modules:\n");
   for (i=0;;i++){
     if (modules[i] == NULL) break;
     printf("     %s\n", modules[i]);
   }

   voices = spd_list_voices(conn);
   if (voices == NULL){
     printf("Can't list voices\n");
     exit(0);
   }

   printf("Available symbolic voices:\n");
   for (i=0;;i++){
     if (voices[i] == NULL) break;
     printf("     %s\n", voices[i]);
   }

   for (j=0; ;j++){
     if (modules[j] == NULL) break;
     spd_set_output_module(conn, modules[j]);
     printf("\nListing voices for %s\n", modules[j]);
     synth_voices = spd_list_synthesis_voices(conn);
     if (synth_voices == NULL){
       printf("Can't list voices\n");
       exit(0);
     }
     printf("Available synthesis voices:\n");
     for (i=0;;i++){
       if (synth_voices[i] == NULL)
	 break;
       printf("     name: %s language: %s variant: %s\n", 
	      synth_voices[i]->name, synth_voices[i]->language,
	      synth_voices[i]->variant);
       spd_set_synthesis_voice(conn, synth_voices[i]->name);
       spd_say(conn, SPD_TEXT, "test");
       sleep(1);
     }
   }

   printf("Trying to close Speech Dispatcher connection...");
   spd_close(conn);
   printf("OK\n");

   printf("End of the test.\n");
   exit(0);
}

