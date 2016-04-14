
/*
 * dummy.c - Speech Dispatcher dummy output module
 *
 * A simplific output module that just tries to play an
 * an error message in various ways.
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
 * $Id: dummy.c,v 1.3 2008-06-09 10:32:00 hanke Exp $
 */

#include <glib.h>

#include "fdset.h"

#include "module_utils.h"
#include "module_utils_addvoice.c"

#define MODULE_NAME     "dummy"
#define MODULE_VERSION  "0.1"

//#define Debug 0

#if !(defined(__GLIBC__) && defined(_GNU_SOURCE))
/* Added by Willie Walker - getline is a gcc-ism */
ssize_t getline (char **lineptr, size_t *n, FILE *f);
#endif

/* Thread and process control */
static int dummy_speaking = 0;

static pthread_t dummy_speak_thread;
static pid_t dummy_pid;
static sem_t *dummy_semaphore;


/* Internal functions prototypes */
static void* _dummy_speak(void*);
static void _dummy_child();

/* Fill the module_info structure with pointers to this modules functions */

/* Public functions */
int
module_load(void)
{

    INIT_SETTINGS_TABLES();
    
    return 0;
}

int
module_init(char **status_info)
{
    int ret;

    *status_info = NULL;

    dummy_semaphore = module_semaphore_init();

    DBG("Dummy: creating new thread for dummy_speak\n");
    dummy_speaking = 0;
    ret = pthread_create(&dummy_speak_thread, NULL, _dummy_speak, NULL);
    if(ret != 0){
        DBG("Dummy: thread failed\n");
	*status_info = strdup("The module couldn't initialize threads"
			      "This can be either an internal problem or an"
			      "architecture problem. If you are sure your architecture"
			      "supports threads, please report a bug.");
        return -1;
    }
								
    *status_info = strdup("Everything ok so far.");

    DBG("Ok, now debugging");

    return 0;
}

int
module_audio_init(char **status_info){
  status_info = NULL;
  return 0;
}

VoiceDescription**
module_list_voices(void)
{
  return NULL;
}


int
module_speak(gchar *data, size_t bytes, EMessageType msgtype)
{

    DBG("speak()\n");

    if (dummy_speaking){
        DBG("Speaking when requested to write");
        return 0;
    }

    if(module_write_data_ok(data) != 0) return -1;

    DBG("Requested data: |%s|\n", data);
	
    /* Send semaphore signal to the speaking thread */
    dummy_speaking = 1;    
    sem_post(dummy_semaphore);    
		
    DBG("Dummy: leaving write() normaly\n\r");
    return bytes;
}

int
module_stop(void) 
{
  DBG("dummy: stop(), dummy_speaking=%d, dummy_pid=%d\n", dummy_speaking, dummy_pid);

    if(dummy_speaking && dummy_pid){
        DBG("dummy: stopping process group pid %d\n", dummy_pid);
        kill(-dummy_pid, SIGKILL);
    }
    DBG("Already stopped, no action");
    return 0;
}

size_t
module_pause(void)
{
    DBG("pause requested\n");
    if(dummy_speaking){
        DBG("Dummy module can't pause\n");
        return 0;        
    }else{
        return -1;
    }
}

char*
module_is_speaking(void)
{
    return NULL ; 
}

void
module_close(int status)
{
    DBG("dummy: close()\n");

    if(dummy_speaking){
        module_stop();
    }

    if (module_terminate_thread(dummy_speak_thread) != 0)
        exit(1);

    exit(status);
}


/* Internal functions */


void*
_dummy_speak(void* nothing)
{      
    int status;

    DBG("dummy: speaking thread starting.......\n");

    set_speaking_thread_parameters();

    while(1){        
        sem_wait(dummy_semaphore);
        DBG("Semaphore on\n");
	module_report_event_begin();

        /* Create a new process so that we could send it signals */
        dummy_pid = fork();

        switch(dummy_pid){
        case -1:	
            DBG("Can't say the message. fork() failed!\n");
            dummy_speaking = 0;
            continue;

        case 0:
            {	      
	      
	      /* Set this process as a process group leader (so that SIGKILL
		 is also delivered to the child processes created by system()) */
	      if (setpgid(0,0) == -1) DBG("Can't set myself as project group leader!");
	      
	      DBG("Starting child...\n");
	      _dummy_child();
            }
            break;

        default:
	  /* This is the parent. Send data to the child. */
	  
	  DBG("Waiting for child...");
	  waitpid(dummy_pid, &status, 0); 
	  dummy_speaking = 0;
	  

	  DBG("Child exited");

	  // Report CANCEL if the process was signal-terminated
	  // and END if it terminated normally
	  if (WIFSIGNALED(status)) module_report_event_stop();
	  else module_report_event_end();
	  
            DBG("child terminated -: status:%d signal?:%d signal number:%d.\n",
                WIFEXITED(status), WIFSIGNALED(status), WTERMSIG(status));
        }        
    }

    dummy_speaking = 0;

    DBG("dummy: speaking thread ended.......\n");    

    pthread_exit(NULL);
}	

void
_dummy_child()
{
  sigset_t some_signals;
    
  int ret;
  char *try1, *try2, *try3;

  sigfillset(&some_signals);
  module_sigunblockusr(&some_signals);

  DBG("Entering child loop\n");
  /* Read the waiting data */

  try1 = strdup("play "DATADIR"/dummy-message.wav > /dev/null 2> /dev/null");
  try2 = strdup("aplay  "DATADIR"/dummy-message.wav > /dev/null 2> /dev/null");
  try3 = strdup("paplay "DATADIR"/dummy-message.wav > /dev/null 2> /dev/null");
  
  DBG("child: synth commands = |%s|%s|%s|", try1, try2, try3);
  DBG("Speaking in child...");
  module_sigblockusr(&some_signals);        
  
  ret = system(try1);
  DBG("Executed shell command '%s' returned with %d", try1, ret);
  if ((ret != 0)){
    DBG("Execution failed, trying seccond command");
    ret = system(try2);
    DBG("Executed shell command '%s' returned with %d", try1, ret);
    if ((ret != 0)){
      DBG("Execution failed, trying third command");
      ret = system(try3);
      DBG("Executed shell command '%s' returned with %d", try1, ret);
      if ((ret != 0) && (ret != 256)){
	DBG("Failed, giving up.");
      }
    }
  }
    
  module_sigunblockusr(&some_signals);        
    
  xfree(try1); xfree(try2); xfree(try3);

  DBG("Done, exiting from child.");
  exit(0);
}



#include "module_main.c"
