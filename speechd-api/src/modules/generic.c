
/*
 * generic.c - Speech Dispatcher generic output module
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
 * $Id: generic.c,v 1.30 2008-07-30 09:15:51 hanke Exp $
 */

#include <glib.h>

#include "fdset.h"

#include "module_utils.h"
#include "module_utils_addvoice.c"

#define MODULE_NAME     "generic"
#define MODULE_VERSION  "0.2"

DECLARE_DEBUG()

/* Thread and process control */
static int generic_speaking = 0;

static pthread_t generic_speak_thread;
static pid_t generic_pid;
static sem_t *generic_semaphore;

static char **generic_message;
static EMessageType generic_message_type;

static int generic_position = 0;
static int generic_pause_requested = 0;

static char *execute_synth_str1;
static char *execute_synth_str2;

/* Internal functions prototypes */
static void* _generic_speak(void*);
static void _generic_child(TModuleDoublePipe dpipe, const size_t maxlen);
static void generic_child_close(TModuleDoublePipe dpipe);

void generic_set_rate(signed int rate);
void generic_set_pitch(signed int pitch);
void generic_set_voice(EVoiceType voice);
void generic_set_language(char* language);
void generic_set_volume(signed int volume);
void generic_set_punct(EPunctMode punct);

/* Fill the module_info structure with pointers to this modules functions */

MOD_OPTION_1_STR(GenericExecuteSynth)
MOD_OPTION_1_INT(GenericMaxChunkLength)
MOD_OPTION_1_STR(GenericDelimiters)
MOD_OPTION_1_STR(GenericPunctNone)
MOD_OPTION_1_STR(GenericPunctSome)
MOD_OPTION_1_STR(GenericPunctAll)
MOD_OPTION_1_STR(GenericStripPunctChars)
MOD_OPTION_1_STR(GenericRecodeFallback)

MOD_OPTION_1_INT(GenericRateAdd)
MOD_OPTION_1_FLOAT(GenericRateMultiply)
MOD_OPTION_1_INT(GenericRateForceInteger)
MOD_OPTION_1_INT(GenericPitchAdd)
MOD_OPTION_1_FLOAT(GenericPitchMultiply)
MOD_OPTION_1_INT(GenericPitchForceInteger)
MOD_OPTION_1_INT(GenericVolumeAdd)
MOD_OPTION_1_FLOAT(GenericVolumeMultiply)
MOD_OPTION_1_INT(GenericVolumeForceInteger)
MOD_OPTION_3_HT(GenericLanguage, code, name, charset)

static char generic_msg_pitch_str[16];
static char generic_msg_rate_str[16];
static char generic_msg_volume_str[16];
static char* generic_msg_voice_str = NULL;
static TGenericLanguage* generic_msg_language = NULL;
static char* generic_msg_punct_str;

/* Public functions */
int
module_load(void)
{

    INIT_SETTINGS_TABLES();

    MOD_OPTION_1_STR_REG(GenericExecuteSynth, "");

    REGISTER_DEBUG();

    MOD_OPTION_1_INT_REG(GenericMaxChunkLength, 300);
    MOD_OPTION_1_STR_REG(GenericDelimiters, ".");
    MOD_OPTION_1_STR_REG(GenericStripPunctChars, "");
    MOD_OPTION_1_STR_REG(GenericRecodeFallback, "?");

    MOD_OPTION_1_INT_REG(GenericRateAdd, 0);
    MOD_OPTION_1_FLOAT_REG(GenericRateMultiply, 1);
    MOD_OPTION_1_INT_REG(GenericRateForceInteger, 0);

    MOD_OPTION_1_INT_REG(GenericPitchAdd, 0);
    MOD_OPTION_1_FLOAT_REG(GenericPitchMultiply, 1);
    MOD_OPTION_1_INT_REG(GenericPitchForceInteger, 0);

    MOD_OPTION_1_INT_REG(GenericVolumeAdd, 0);
    MOD_OPTION_1_FLOAT_REG(GenericVolumeMultiply, 1);
    MOD_OPTION_1_INT_REG(GenericVolumeForceInteger, 0);


    MOD_OPTION_HT_REG(GenericLanguage);

    MOD_OPTION_1_STR_REG(GenericPunctNone, "--punct-none");
    MOD_OPTION_1_STR_REG(GenericPunctSome, "--punct-some");
    MOD_OPTION_1_STR_REG(GenericPunctAll, "--punct-all");


    module_register_settings_voices();

    return 0;
}

int
module_init(char **status_info)
{
    int ret;

    *status_info = NULL;

    DBG("GenericMaxChunkLength = %d\n", GenericMaxChunkLength);
    DBG("GenericDelimiters = %s\n", GenericDelimiters);
    DBG("GenericExecuteSynth = %s\n", GenericExecuteSynth);

    generic_msg_language = (TGenericLanguage*) xmalloc(sizeof(TGenericLanguage));
    generic_msg_language->code = strdup("en");
    generic_msg_language->charset = strdup("iso-8859-1");
    generic_msg_language->name = strdup("english");    

    generic_message = malloc (sizeof (char*));    
    generic_semaphore = module_semaphore_init();

    DBG("Generic: creating new thread for generic_speak\n");
    generic_speaking = 0;
    ret = pthread_create(&generic_speak_thread, NULL, _generic_speak, NULL);
    if(ret != 0){
        DBG("Generic: thread failed\n");
	*status_info = strdup("The module couldn't initialize threads"
			      "This can be either an internal problem or an"
			      "architecture problem. If you are sure your architecture"
			      "supports threads, please report a bug.");
        return -1;
    }
								
    *status_info = strdup("Everything ok so far.");
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
    char *tmp;

    DBG("speak()\n");

    if (generic_speaking){
        DBG("Speaking when requested to write");
        return 0;
    }


    if(module_write_data_ok(data) != 0) return -1;

    UPDATE_STRING_PARAMETER(language, generic_set_language);
    UPDATE_PARAMETER(voice, generic_set_voice);
    UPDATE_PARAMETER(punctuation_mode, generic_set_punct);
    UPDATE_PARAMETER(pitch, generic_set_pitch);
    UPDATE_PARAMETER(rate, generic_set_rate);
    UPDATE_PARAMETER(volume, generic_set_volume);

    /* Set the appropriate charset */
    assert(generic_msg_language != NULL);
    if (generic_msg_language->charset != NULL){
	DBG("Recoding from UTF-8 to %s...", generic_msg_language->charset);
	tmp = 
	    (char*) g_convert_with_fallback(data, bytes, generic_msg_language->charset,
					    "UTF-8", GenericRecodeFallback, NULL, NULL,
					    NULL);
    }else{
	DBG("Warning: Prefered charset not specified, recoding to iso-8859-1");
	tmp = 
	    (char*) g_convert_with_fallback(data, bytes, "iso-8859-2",
					    "UTF-8", GenericRecodeFallback, NULL, NULL,
					    NULL);
    }

    if (tmp == NULL) return -1;

    if (msgtype == MSGTYPE_TEXT)
      *generic_message = module_strip_ssml(tmp);
    else
      *generic_message = strdup(tmp);
    xfree(tmp);

    module_strip_punctuation_some(*generic_message, GenericStripPunctChars);

    generic_message_type = MSGTYPE_TEXT;

    DBG("Requested data: |%s|\n", data);
	
    /* Send semaphore signal to the speaking thread */
    generic_speaking = 1;    
    sem_post(generic_semaphore);    
		
    DBG("Generic: leaving write() normaly\n\r");
    return bytes;
}

int
module_stop(void) 
{
    DBG("generic: stop()\n");

    if(generic_speaking && generic_pid != 0){
        DBG("generic: stopping process group pid %d\n", generic_pid);
        kill(-generic_pid, SIGKILL);
    }
    return 0;
}

size_t
module_pause(void)
{
    DBG("pause requested\n");
    if(generic_speaking){
        DBG("Sending request to pause to child\n");
        generic_pause_requested = 1;

        DBG("paused at byte: %d", generic_position);
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
    DBG("generic: close()\n");

    if(generic_speaking){
        module_stop();
    }

    if (module_terminate_thread(generic_speak_thread) != 0)
        exit(1);

    exit(status);
}


/* Internal functions */

/* Replace all occurances of 'token' in 'sting'
   with 'data' */
char*
string_replace(char *string, char* token, char* data)
{
    char *p;
    char *str1;
    char *str2;
    char *new;
    char *mstring;

    mstring = g_strdup(string);
    while (1){
      /* Split the string in two parts, ommit the token */
      p = strstr(mstring, token);
      if (p == NULL){
	return mstring;
      }
      *p = 0;

      str1 = mstring;
      str2 = p + (strlen(token));        

      /* Put it together, replacing token with data */
      new = g_strdup_printf("%s%s%s", str1, data, str2);
      xfree(mstring);
      mstring = new;
    }

}

void*
_generic_speak(void* nothing)
{	
    TModuleDoublePipe module_pipe;
    int ret;
    int status;

    DBG("generic: speaking thread starting.......\n");

    set_speaking_thread_parameters();

    while(1){        
        sem_wait(generic_semaphore);
        DBG("Semaphore on\n");

        ret = pipe(module_pipe.pc);
        if (ret != 0){
            DBG("Can't create pipe pc\n");
            generic_speaking = 0;
            continue;
        }

        ret = pipe(module_pipe.cp);
        if (ret != 0){
            DBG("Can't create pipe cp\n");
            close(module_pipe.pc[0]);     close(module_pipe.pc[1]);
            generic_speaking = 0;
            continue;
        }

	module_report_event_begin();

        /* Create a new process so that we could send it signals */
        generic_pid = fork();

        switch(generic_pid){
        case -1:	
            DBG("Can't say the message. fork() failed!\n");
            close(module_pipe.pc[0]);     close(module_pipe.pc[1]);
            close(module_pipe.cp[0]);     close(module_pipe.cp[1]);
            generic_speaking = 0;
            continue;

        case 0:
            {
		char *e_string;
		char *p;	       
		char *tmpdir, *homedir;
		const char *helper;
		char *play_command;
		char *next_method;
		int amlen;

		helper = getenv("TMPDIR");
		if (helper)
		  tmpdir = strdup(helper);
		else
		  tmpdir = strdup("/tmp");

		helper = g_get_home_dir();
		if (helper)
		  homedir = strdup(helper);
		else
		  homedir = strdup("UNKNOWN_HOME_DIRECTORY");

		
		/* Generic will always use the first audio method,
		 it doesn't currently support fallback*/
		next_method = strchr(audio_settings.audio_output_method, ',');
		amlen = (next_method ? ((size_t)(next_method-audio_settings.audio_output_method)) : strlen(audio_settings.audio_output_method));

		DBG("Requested audio output methods are: %s (only first taken into account", audio_settings.audio_output_method);
		if (!strncmp(audio_settings.audio_output_method, "oss", amlen)){
		  play_command = strdup("play");
		}else if (!strncmp(audio_settings.audio_output_method, "alsa", amlen)){
		  play_command = strdup("aplay");
		}else if (!strncmp(audio_settings.audio_output_method, "libao", amlen)){
		  play_command = strdup("play");
		}else if (!strncmp(audio_settings.audio_output_method, "pulse", amlen)){
		  play_command = strdup("paplay");
		}else{
		  /* This should not happen */
		  DBG("ERROR: Unsupported audio output method");
		  exit(1);
		}

		/* Set this process as a process group leader (so that SIGKILL
		   is also delivered to the child processes created by system()) */
		if (setpgid(0,0) == -1) DBG("Can't set myself as project group leader!");

		e_string = strdup(GenericExecuteSynth);

		e_string = string_replace(e_string, "$PLAY_COMMAND", play_command);
		xfree(play_command);
		e_string = string_replace(e_string, "$TMPDIR", tmpdir);
		xfree(tmpdir);
		e_string = string_replace(e_string, "$HOMEDIR", homedir);
		xfree(homedir);
		e_string = string_replace(e_string, "$PITCH", generic_msg_pitch_str);
		e_string = string_replace(e_string, "$RATE", generic_msg_rate_str);
		e_string = string_replace(e_string, "$VOLUME", generic_msg_volume_str);
		e_string = string_replace(e_string, "$LANGUAGE", generic_msg_language->name);
 		e_string = string_replace(e_string, "$PUNCT", generic_msg_punct_str);
		if (generic_msg_voice_str != NULL)
		    e_string = string_replace(e_string, "$VOICE", generic_msg_voice_str);
		else
		    e_string = string_replace(e_string, "$VOICE", "no_voice");

		/* Cut it into two strings */           
		p = strstr(e_string, "$DATA");
		if (p == NULL) exit(1);
		*p = 0;
		execute_synth_str1 = strdup(e_string);
		execute_synth_str2 = strdup(p + (strlen("$DATA")));
		
		free(e_string);
		
		/* execute_synth_str1 se sem musi nejak dostat */
		DBG("Starting child...\n");
		_generic_child(module_pipe, GenericMaxChunkLength);
            }
            break;

        default:
            /* This is the parent. Send data to the child. */

            generic_position = module_parent_wfork(module_pipe, *generic_message,
                                                   generic_message_type,
                                                   GenericMaxChunkLength, GenericDelimiters,
                                                   &generic_pause_requested);

            DBG("Waiting for child...");
            waitpid(generic_pid, &status, 0); 
            generic_speaking = 0;

	    // Report CANCEL if the process was signal-terminated
	    // and END if it terminated normally
	    if (WIFSIGNALED(status)) module_report_event_stop();
	    else module_report_event_end();

            DBG("child terminated -: status:%d signal?:%d signal number:%d.\n",
                WIFEXITED(status), WIFSIGNALED(status), WTERMSIG(status));
        }        
    }

    generic_speaking = 0;

    DBG("generic: speaking thread ended.......\n");    

    pthread_exit(NULL);
}	

void
_generic_child(TModuleDoublePipe dpipe, const size_t maxlen)
{
    char *text;  
    sigset_t some_signals;
    int bytes;
    char *command;
    GString *message;
    int i;
    int ret;

    sigfillset(&some_signals);
    module_sigunblockusr(&some_signals);

    module_child_dp_init(dpipe);

    DBG("Entering child loop\n");
    while(1){
        /* Read the waiting data */
        text = malloc((maxlen + 1) * sizeof(char));
        bytes = module_child_dp_read(dpipe, text, maxlen);
        DBG("read %d bytes in child", bytes);
        if (bytes == 0){
            free(text);
            generic_child_close(dpipe);
        }

        text[bytes] = 0;
        DBG("text read is: |%s|\n", text);

        /* Escape any quotes */
        message = g_string_new("");
        for(i=0; i<=bytes-1; i++){
            if (text[i] == '\'')
                message = g_string_append(message, "'\\''");
	    else{
                g_string_append_printf(message, "%c", text[i]);
            }
        }

        DBG("child: escaped text is |%s|", message->str);

        command = malloc((strlen(message->str)+strlen(execute_synth_str1)+
                          strlen(execute_synth_str2) + 8) * sizeof(char));

        if (strlen(message->str) != 0){
            sprintf(command, "%s%s%s", execute_synth_str1, message->str, execute_synth_str2);

            DBG("child: synth command = |%s|", command);

            DBG("Speaking in child...");
            module_sigblockusr(&some_signals);        
            {
               ret = system(command);
	       DBG("Executed shell command returned with %d", ret);
            }
        }
        module_sigunblockusr(&some_signals);        

        xfree(command);
        xfree(text);
        g_string_free(message, 1);

        DBG("child->parent: ok, send more data");      
        module_child_dp_write(dpipe, "C", 1);
    }
}

static void
generic_child_close(TModuleDoublePipe dpipe)
{   
    DBG("child: Pipe closed, exiting, closing pipes..\n");
    module_child_dp_close(dpipe);          
    DBG("Child ended...\n");
    exit(0);
}

void
generic_set_pitch(int pitch)
{
    float hpitch;

    hpitch = ((float) pitch) * GenericPitchMultiply + GenericPitchAdd;
    if (!GenericPitchForceInteger){
	snprintf(generic_msg_pitch_str, 15, "%.2f", hpitch);
    }else{
	snprintf(generic_msg_pitch_str, 15, "%d", (int) hpitch);
    }
}

void
generic_set_rate(int rate)
{
    float hrate;

    hrate = ((float) rate) * GenericRateMultiply + GenericRateAdd;
    if (!GenericRateForceInteger){
	snprintf(generic_msg_rate_str, 15, "%.2f", hrate);
    }else{
	snprintf(generic_msg_rate_str, 15, "%d", (int) hrate);
    }
}

void
generic_set_volume(int volume)
{
    float hvolume;

    DBG("Volume: %d", volume);

    hvolume = ((float) volume) * GenericVolumeMultiply + GenericVolumeAdd;
    DBG("HVolume: %f", hvolume);
    if (!GenericVolumeForceInteger){
	snprintf(generic_msg_volume_str, 15, "%.2f", hvolume);
    }else{
	snprintf(generic_msg_volume_str, 15, "%d", (int) hvolume);
    }
}

void
generic_set_language(char *lang)
{

    generic_msg_language = (TGenericLanguage*) module_get_ht_option(GenericLanguage,
							lang);
    if (generic_msg_language == NULL){
	DBG("Language %s not found in the configuration file, using defaults.", lang);
	generic_msg_language = (TGenericLanguage*) xmalloc(sizeof(TGenericLanguage));
	generic_msg_language->code = strdup(lang);
	generic_msg_language->charset = NULL;
	generic_msg_language->name = strdup(lang);
    }

    if (generic_msg_language->name == NULL){
	DBG("Language name for %s not found in the configuration file.", lang);
	generic_msg_language = (TGenericLanguage*) xmalloc(sizeof(TGenericLanguage));
	generic_msg_language->code = strdup("en");
	generic_msg_language->charset = strdup("iso-8859-1");
	generic_msg_language->name = strdup("english");
    }

    generic_set_voice(msg_settings.voice);
}

void
generic_set_voice(EVoiceType voice)
{
    assert(generic_msg_language);
    generic_msg_voice_str = module_getvoice(generic_msg_language->code, voice);
    if (generic_msg_voice_str == NULL){
	DBG("Invalid voice type specified or no voice available!");
    }	
}

void
generic_set_punct(EPunctMode punct)
{
    if (punct == PUNCT_NONE){
        generic_msg_punct_str = strdup((char*) GenericPunctNone);
	return;
    }
    else if (punct == PUNCT_SOME){
	generic_msg_punct_str = strdup((char*) GenericPunctSome);
	return;
    }
    else if (punct == PUNCT_ALL){
	generic_msg_punct_str = strdup((char*) GenericPunctAll);
	return;
    }
    else{
	DBG("ERROR: Unknown punctuation setting, ignored");
    }
}

#include "module_main.c"
