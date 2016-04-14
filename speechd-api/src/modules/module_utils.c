/*
 * module_utils.c - Module utilities
 *           Functions to help writing output modules for Speech Dispatcher
 * Copyright (C) 2003,2006, 2007 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1, or (at your option) any later
 * version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this package; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 * $Id: module_utils.c,v 1.55 2008-07-10 15:37:18 hanke Exp $
 */

#include "fdsetconv.h"
#include "fdsetconv.c"

#include "module_utils.h"


extern char* module_index_mark;

#if !(defined(__GLIBC__) && defined(_GNU_SOURCE))
/* Added by Willie Walker - getline is a gcc-ism */
#define BUFFER_LEN 256
ssize_t getline (char **lineptr, size_t *n, FILE *f)
{
        char ch;
        size_t m = 0;
        ssize_t buf_len = 0;
        char * buf = NULL;
        char * p = NULL;

	if (errno != 0) {
                DBG("getline: errno came in as %d!!!\n", errno);
	        errno = 0;
	}
        while ( (ch = getc(f)) !=EOF )
        {
                if (errno != 0)
                        return -1;
                if ( m++ >= buf_len )
                {
                        buf_len += BUFFER_LEN;
                        buf = (char *) realloc(buf, buf_len + 1);
                        if ( buf == NULL )
                        {
                                DBG("buf==NULL");
                                return -1;
                        }
                        p = buf + buf_len - BUFFER_LEN;
                }
                *p = ch;
                p++;
                if ( ch == '\n' )
                        break;
        }
        if ( m == 0 )
        {
                DBG("getline: m=%d!",m);
                return -1;
        } else {
                *p = '\0';
                *lineptr = buf;
                *n = m;
                return m;
        }
}
#endif /* !(defined(__GLIBC__) && defined(_GNU_SOURCE)) */

void*
xmalloc(size_t size)
{
    void *p;

    p = malloc(size);
    if (p == NULL) FATAL("Not enough memmory");
    return p;
}          

void*
xrealloc(void *data, size_t size)
{
    void *p;

    if (data != NULL) 
        p = realloc(data, size);
    else 
        p = malloc(size);

    if (p == NULL) FATAL("Not enough memmory");

    return p;
}          

void
xfree(void *data)
{
    if (data != NULL) free(data);
}

char*
do_message(EMessageType msgtype)
{
    int ret;
    char *cur_line;
    GString *msg;
    size_t n;
    int nlines = 0;

    msg = g_string_new("");

    printf("202 OK RECEIVING MESSAGE\n");
    fflush(stdout);

    while(1){
        cur_line = NULL;
        n = 0;
        ret = getline(&cur_line, &n, stdin);
        nlines++;
        if (ret == -1) return strdup("401 ERROR INTERNAL");

        if (!strcmp(cur_line, "..\n")){
            xfree(cur_line);
            cur_line = strdup(".\n");
        }else if (!strcmp(cur_line, ".\n")){
            /* Strip the trailing \n */
            msg->str[strlen(msg->str)-1] = 0;
	    xfree(cur_line);
            break;
        }
        g_string_append(msg, cur_line);
	xfree(cur_line);
    }
    

    if ((msgtype != MSGTYPE_TEXT) && (nlines > 2)){
        return strdup("305 DATA MORE THAN ONE LINE");
    }

    if ((msgtype == MSGTYPE_CHAR) && (!strcmp(msg->str,"space"))){
	g_string_free(msg, 1);
	msg = g_string_new(" ");
    }

    ret = module_speak(msg->str, strlen(msg->str), msgtype);

    g_string_free(msg,1);
    if (ret <= 0) return strdup("301 ERROR CANT SPEAK");
    
    return strdup("200 OK SPEAKING");
}

char*
do_speak(void)
{
    return do_message(MSGTYPE_TEXT);
}

char*
do_sound_icon(void)
{
    return do_message(MSGTYPE_SOUND_ICON);
}

char*
do_char(void)
{
    return do_message(MSGTYPE_CHAR);
}

char*
do_key(void)
{
    return do_message(MSGTYPE_KEY);
}

void
do_stop(void)
{
    module_stop();
    return;
}

void
do_pause(void)
{
    int ret;
   
    ret = module_pause(); 
    if (ret){
	DBG("WARNING: Can't pause");
	return;
    }

    return;
}

#define SET_PARAM_NUM(name, cond) \
 if(!strcmp(cur_item, #name)){ \
     number = strtol(cur_value, &tptr, 10); \
     if(!(cond)){ err = 2; continue; } \
     if (tptr == cur_value){ err = 2; continue; } \
     msg_settings.name = number; \
 }

#define SET_PARAM_STR(name) \
 if(!strcmp(cur_item, #name)){ \
     xfree(msg_settings.name); \
     if(!strcmp(cur_value, "NULL")) msg_settings.name = NULL; \
     else msg_settings.name = strdup(cur_value); \
 }

#define SET_PARAM_STR_C(name, fconv) \
 if(!strcmp(cur_item, #name)){ \
     ret = fconv(cur_value); \
     if (ret != -1) msg_settings.name = ret; \
     else err = 2; \
 }

char*
do_set(void)
{
    char *cur_item = NULL;
    char *cur_value = NULL;
    char *line = NULL;
    int ret;
    size_t n;
    int number; char *tptr;
    int err = 0;                /* Error status */

    printf("203 OK RECEIVING SETTINGS\n");
    fflush(stdout);

    while(1){
        line = NULL; n = 0;
        ret = getline(&line, &n, stdin);
        if (ret == -1){ err=1; break; }
        if (!strcmp(line, ".\n")){
            xfree(line);
            break;
        }
        if (!err){
            cur_item = strtok(line, "=");
            if (cur_item == NULL){ err=1; continue; }
            cur_value = strtok(NULL, "\n");
            if (cur_value == NULL){ err=1; continue; }
            
            SET_PARAM_NUM(rate, ((number>=-100) && (number<=100)))
            else SET_PARAM_NUM(pitch, ((number>=-100) && (number<=100)))
	    else SET_PARAM_NUM(volume, ((number>=-100) && (number<=100)))
            else SET_PARAM_STR_C(punctuation_mode, str2EPunctMode)
            else SET_PARAM_STR_C(spelling_mode, str2ESpellMode)
            else SET_PARAM_STR_C(cap_let_recogn, str2ECapLetRecogn)
            else SET_PARAM_STR_C(voice, str2EVoice)
            else SET_PARAM_STR(synthesis_voice)
            else SET_PARAM_STR(language)
            else err=2;             /* Unknown parameter */
        }
        xfree(line);
    }

    if (err == 0) return strdup("203 OK SETTINGS RECEIVED");
    if (err == 1) return strdup("302 ERROR BAD SYNTAX");
    if (err == 2) return strdup("303 ERROR INVALID PARAMETER OR VALUE");
    
    return strdup("401 ERROR INTERNAL"); /* Can't be reached */
}

#define SET_AUDIO_NUM(name, cond) \
 if(!strcmp(cur_item, #name)){ \
     number = strtol(cur_value, &tptr, 10); \
     if(!(cond)){ err = 2; continue; } \
     if (tptr == cur_value){ err = 2; continue; } \
     audio_settings.name = number; \
 }

#define SET_AUDIO_STR(name) \
 if(!strcmp(cur_item, #name)){ \
     xfree(audio_settings.name); \
     if(!strcmp(cur_value, "NULL")) audio_settings.name = NULL; \
     else audio_settings.name = strdup(cur_value); \
 }


char*
do_audio(void)
{
    char *cur_item = NULL;
    char *cur_value = NULL;
    char *line = NULL;
    int ret;
    size_t n;
    int number; char *tptr;
    int err = 0;                /* Error status */
    char *status;
    char *msg;

    printf("207 OK RECEIVING AUDIO SETTINGS\n");
    fflush(stdout);

    while(1){
        line = NULL; n = 0;
        ret = getline(&line, &n, stdin);
        if (ret == -1){ err=1; break; }
        if (!strcmp(line, ".\n")){
            xfree(line);
            break;
        }
        if (!err){
            cur_item = strtok(line, "=");
            if (cur_item == NULL){ err=1; continue; }
            cur_value = strtok(NULL, "\n");
            if (cur_value == NULL){ err=1; continue; }
            
            SET_AUDIO_STR(audio_output_method)
            else SET_AUDIO_STR(audio_oss_device)
	    else SET_AUDIO_STR(audio_alsa_device)
	    else SET_AUDIO_STR(audio_nas_server)
            else SET_AUDIO_STR(audio_pulse_server)
	    else SET_AUDIO_NUM(audio_pulse_min_length, 1)
            else err=2;             /* Unknown parameter */
        }
        xfree(line);
    }

    if (err == 1) return strdup("302 ERROR BAD SYNTAX");
    if (err == 2) return strdup("303 ERROR INVALID PARAMETER OR VALUE");

    err = module_audio_init(&status);

    if (err == 0)
      msg = g_strdup_printf("203 OK AUDIO INITIALIZED");
    else
      msg = g_strdup_printf("300-%s\n300 UNKNOWN ERROR", status);
    
    return msg;
}

#define SET_LOGLEVEL_NUM(name, cond) \
 if(!strcmp(cur_item, #name)){ \
     number = strtol(cur_value, &tptr, 10); \
     if(!(cond)){ err = 2; continue; } \
     if (tptr == cur_value){ err = 2; continue; } \
     log_level = number; \
     spd_audio_set_loglevel(log_level); \
 }

char*
do_loglevel(void)
{
    char *cur_item = NULL;
    char *cur_value = NULL;
    char *line = NULL;
    int ret;
    size_t n;
    int number; char *tptr;
    int err = 0;                /* Error status */
    char *msg;

    printf("207 OK RECEIVING LOGLEVEL SETTINGS\n");
    fflush(stdout);

    while(1){
        line = NULL; n = 0;
        ret = getline(&line, &n, stdin);
        if (ret == -1){ err=1; break; }
        if (!strcmp(line, ".\n")){
            xfree(line);
            break;
        }
        if (!err){
            cur_item = strtok(line, "=");
            if (cur_item == NULL){ err=1; continue; }
            cur_value = strtok(NULL, "\n");
            if (cur_value == NULL){ err=1; continue; }

            SET_LOGLEVEL_NUM(log_level, 1)
            else err=2;             /* Unknown parameter */
        }
        xfree(line);
    }

    if (err == 1) return strdup("302 ERROR BAD SYNTAX");
    if (err == 2) return strdup("303 ERROR INVALID PARAMETER OR VALUE");

    msg = g_strdup_printf("203 OK LOG LEVEL SET");

    return msg;
}

char*
do_debug(char* cmd_buf)
{
  /* TODO: Develop the full on/off logic etc. */

  char **cmd;
  char *filename;

  cmd = g_strsplit (cmd_buf, " ", -1);

  if (!cmd[1]){
    g_strfreev(cmd);
    return strdup("302 ERROR BAD SYNTAX");
  }

  if (!strcmp(cmd[1], "ON")){
    if (!cmd[2]){
      g_strfreev(cmd);
      return strdup("302 ERROR BAD SYNTAX");
    }


    filename = cmd[2];
    DBG("Additional logging into specific path %s requested", filename);
    CustomDebugFile = fopen(filename, "w+");
    if (CustomDebugFile == NULL){
      DBG("ERROR: Can't open custom debug file for logging: %d (%s)",
	  errno, strerror(errno));
      return strdup("303 CANT OPEN CUSTOM DEBUG FILE");
    }
    if (Debug == 1)
      Debug = 3;
    else
      Debug = 2;

    DBG("Additional logging initialized");
  }else if (!strcmp(cmd[1], "OFF")){
    if (Debug == 3)
      Debug = 1;
    else 
      Debug = 0;

    if (CustomDebugFile != NULL)
      fclose(CustomDebugFile);
    CustomDebugFile = NULL;
    DBG("Additional logging into specific path terminated");
  }else{
    return strdup("302 ERROR BAD SYNTAX");
  }

  g_strfreev(cmd);
  return g_strdup("200 OK DEBUGGING ON");
}

char *
do_list_voices(void)
{
  VoiceDescription **voices;
  int i;
  char *lang, *dialect;
  GString *voice_list;

  voices = module_list_voices();
  if (voices == NULL){
    return strdup("304 CANT LIST VOICES");
  }
  
  voice_list = g_string_new("");
  for (i=0; ;i++){
    if (voices[i] == NULL) break;
    if (voices[i]->language==NULL) lang=strdup("none");
    else lang=strdup(voices[i]->language);
    if (voices[i]->dialect==NULL) dialect=strdup("none");
    else dialect=strdup(voices[i]->dialect);
    g_string_append_printf(voice_list, "200-%s %s %s\n", voices[i]->name, lang, dialect);
    xfree(lang); xfree(dialect);
  }
  g_string_append(voice_list, "200 OK VOICE LIST SENT");

  DBG("Voice prepared to  sens to speechd");

  return voice_list->str;
}

#undef SET_PARAM_NUM
#undef SET_PARAM_STR

/* This has to return int (although it doesn't return at all) so that we could
 * call it from PROCESS_CMD() macro like the other commands that return
 * something */
void
do_quit(void)
{
    printf("210 OK QUIT\n");    
    fflush(stdout);
    module_close(0);
    return;
}

int
module_get_message_part(const char* message, char* part, unsigned int *pos,
			size_t maxlen, const char* dividers)
{    
    int i, n;
    int num_dividers;
    int len;

    assert(part != NULL);
    assert(message != NULL);

    len = strlen(message);

    if (message[*pos] == 0) return -1;
    
    if (dividers != NULL){
        num_dividers = strlen(dividers);
    }else{
        num_dividers = 0;
    }

    for(i=0; i <= maxlen-1; i++){
        part[i] = message[*pos];

        if (part[i] == 0){                        
            return i;
        }

        // DBG("pos: %d", *pos);

        if ((len-1-i) > 2){
            if ((message[*pos+1] == ' ') || (message[*pos+1] == '\n')
                || (message[*pos+1] == '\r')){
                for(n = 0; n <= num_dividers-1; n++){
                    if ((part[i] == dividers[n])){                        
                        part[i+1] = 0;                
                        (*pos)++;
                        return i+1;            
                    }           
                }
                if ((message[*pos] == '\n') && (message[*pos+1] == '\n')){
                    part[i+1] = 0;                
                    (*pos)++;
                    return i+1;
                }
                if((len-1-i) > 4){
                    if(((message[*pos] == '\r') && (message[*pos+1] == '\n'))
                       && ((message[*pos+2] == '\r') && (message[*pos+3] == '\n'))){
                        part[i+1] = 0;                
                        (*pos)++;
                        return i+1;
                    }
                }
            }
        }

        (*pos)++;
    }
    part[i] = 0;

    return i;
}

void
module_strip_punctuation_some(char *message, char *punct_chars)
{
    int len;
    char *p = message;
    int i;
    assert(message != NULL);

    if (punct_chars == NULL) return;

    len = strlen(message);
    for (i = 0; i <= len-1; i++){
        if (strchr(punct_chars, *p)){
                DBG("Substitution %d: char -%c- for whitespace\n", i, *p);
                *p = ' ';
        }
        p++;
    }
}

char*
module_strip_ssml(char *message)
{

    int len;
    char *out;
    int i, n;
    int omit = 0;

    assert(message != NULL);

    len = strlen(message);
    out = (char*) xmalloc(sizeof(char) * (len+1));

    for (i = 0, n = 0; i <= len; i++){

	if (message[i] == '<'){
	    omit = 1;
	    continue;
	}
	if (message[i] == '>'){
	    omit = 0;
	    continue;
	}
	if (!strncmp(&(message[i]), "&lt;", 4)){ 
	    i+=3;
	    out[n++] = '<';
	}
	else if (!strncmp(&(message[i]), "&gt;", 4)){
	    i+=3;
	    out[n++] = '>';
	}
	else if (!strncmp(&(message[i]), "&amp;", 5)){
	    i+=4;
	    out[n++] = '&';
	}
	else if (!omit || i == len) out[n++] = message[i];
    }
    DBG("In stripping ssml: |%s|", out);

    return out;
}

void
module_strip_punctuation_default(char *buf)
{
    assert(buf != NULL);
    module_strip_punctuation_some(buf, "~#$%^&*+=|<>[]_");
}

void
module_speak_thread_wfork(sem_t *semaphore, pid_t *process_pid, 
                          TChildFunction child_function,
                          TParentFunction parent_function,
                          int *speaking_flag, char **message, const EMessageType *msgtype,
                          const size_t maxlen, const char *dividers, size_t *module_position,
                          int *pause_requested)
{
    TModuleDoublePipe module_pipe;
    int ret;
    int status;

    set_speaking_thread_parameters();

    while(1){        
        sem_wait(semaphore);
        DBG("Semaphore on\n");

        ret = pipe(module_pipe.pc);
        if (ret != 0){
            DBG("Can't create pipe pc\n");
            *speaking_flag = 0;
            continue;
        }

        ret = pipe(module_pipe.cp);
        if (ret != 0){
            DBG("Can't create pipe cp\n");
            close(module_pipe.pc[0]);     close(module_pipe.pc[1]);
            *speaking_flag = 0;
            continue;
        }

	DBG("Pipes initialized");

        /* Create a new process so that we could send it signals */
        *process_pid = fork();

        switch(*process_pid){
        case -1:	
            DBG("Can't say the message. fork() failed!\n");
            close(module_pipe.pc[0]);     close(module_pipe.pc[1]);
            close(module_pipe.cp[0]);     close(module_pipe.cp[1]);
            *speaking_flag = 0;
            continue;

        case 0:
            /* This is the child. Make it speak, but exit on SIGINT. */

            DBG("Starting child...\n");
            (* child_function)(module_pipe, maxlen);           
            exit(0);

        default:
            /* This is the parent. Send data to the child. */

            *module_position = (* parent_function)(module_pipe, *message, *msgtype,
                                                   maxlen, dividers, pause_requested);

            DBG("Waiting for child...");
            waitpid(*process_pid, &status, 0);            
            
            *speaking_flag = 0;

	    module_report_event_end();

            DBG("child terminated -: status:%d signal?:%d signal number:%d.\n",
                WIFEXITED(status), WIFSIGNALED(status), WTERMSIG(status));
        }        
    }
}

size_t
module_parent_wfork(TModuleDoublePipe dpipe, const char* message, EMessageType msgtype,
                    const size_t maxlen, const char* dividers, int *pause_requested)
{
    unsigned int pos = 0;
    char msg[16];
    char *buf;
    int bytes;
    size_t read_bytes = 0;


    DBG("Entering parent process, closing pipes");

    buf = (char*) malloc((maxlen+1) * sizeof(char));

    module_parent_dp_init(dpipe);

    pos = 0;
    while(1){
        DBG("  Looping...\n");

        bytes = module_get_message_part(message, buf, &pos, maxlen, dividers);

        DBG("Returned %d bytes from get_part\n", bytes);

        if (*pause_requested){               
            DBG("Pause requested in parent");                
            module_parent_dp_close(dpipe);
            *pause_requested = 0;
            return 0;
        }             
   
        if (bytes > 0){
            DBG("Sending buf to child:|%s| %d\n", buf, bytes);
            module_parent_dp_write(dpipe, buf, bytes);
        
            DBG("Waiting for response from child...\n");
            while(1){
                read_bytes = module_parent_dp_read(dpipe, msg, 8);
                if (read_bytes == 0){
                    DBG("parent: Read bytes 0, child stopped\n");                    
                    break;
                }
                if (msg[0] == 'C'){
                    DBG("Ok, received report to continue...\n");
                    break;
                }
            }
        }

        if ((bytes == -1) || (read_bytes == 0)){
            DBG("End of data in parent, closing pipes");
            module_parent_dp_close(dpipe);
            break;
        }
            
    }    
    return 0;
}

int
module_parent_wait_continue(TModuleDoublePipe dpipe)
{
    char msg[16];
    int bytes;

    DBG("parent: Waiting for response from child...\n");
    while(1){
        bytes = module_parent_dp_read(dpipe, msg, 8);
        if (bytes == 0){
            DBG("parent: Read bytes 0, child stopped\n");
            return 1;
        }
        if (msg[0] == 'C'){
            DBG("parent: Ok, received report to continue...\n");
            return 0;
        }
    }
}

void
module_parent_dp_init(TModuleDoublePipe dpipe)
{
    close(dpipe.pc[0]);
    close(dpipe.cp[1]);
}

void
module_parent_dp_close(TModuleDoublePipe dpipe)
{
    close(dpipe.pc[1]);
    close(dpipe.cp[0]);
}

void
module_child_dp_init(TModuleDoublePipe dpipe)
{
    close(dpipe.pc[1]);
    close(dpipe.cp[0]);
}

void
module_child_dp_close(TModuleDoublePipe dpipe)
{
    close(dpipe.pc[0]);
    close(dpipe.cp[1]);
}

void
module_child_dp_write(TModuleDoublePipe dpipe, const char *msg, size_t bytes)
{
    int ret;
    assert(msg != NULL);
    ret = write(dpipe.cp[1], msg, bytes);
    assert(ret);
}

int
module_parent_dp_write(TModuleDoublePipe dpipe, const char *msg, size_t bytes)
{
    int ret;
    assert(msg != NULL);
    DBG("going to write %ld bytes", bytes);
    ret = write(dpipe.pc[1], msg, bytes);      
    DBG("written %d bytes", ret);
    return ret;
}

int
module_child_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen)
{
    int bytes;
    while ((bytes = read(dpipe.pc[0], msg, maxlen)) < 0) {
        if (errno != EINTR) {
            FATAL("Unable to read data");
        }
    }
    return bytes;
}

int
module_parent_dp_read(TModuleDoublePipe dpipe, char *msg, size_t maxlen)
{
    int bytes;
    while ((bytes = read(dpipe.cp[0], msg, maxlen)) < 0) {
        if (errno != EINTR) {
            FATAL("Unable to read data");
        }
    }
    return bytes;
}


void
module_sigblockall(void)
{
    int ret;
    sigset_t all_signals;
    
    DBG("Blocking all signals");

    sigfillset(&all_signals);

    ret = sigprocmask(SIG_BLOCK, &all_signals, NULL);
    if (ret != 0)
    DBG("Can't block signals, expect problems with terminating!\n");
}

void
module_sigunblockusr(sigset_t *some_signals)
{
    int ret;
    
    DBG("UnBlocking user signal");

    sigdelset(some_signals, SIGUSR1);
    ret = sigprocmask(SIG_SETMASK, some_signals, NULL);
    if (ret != 0)
    DBG("Can't block signal set, expect problems with terminating!\n"); 
}

void
module_sigblockusr(sigset_t *some_signals)
{
    int ret;
    
    DBG("Blocking user signal");
    
    sigaddset(some_signals, SIGUSR1);
    ret = sigprocmask(SIG_SETMASK, some_signals, NULL);
    if (ret != 0)
	DBG("Can't block signal set, expect problems when terminating!\n");
    
}

void
set_speaking_thread_parameters()
{
    int ret;
    sigset_t all_signals;	    

    ret = sigfillset(&all_signals);
    if (ret == 0){
	ret = pthread_sigmask(SIG_BLOCK,&all_signals,NULL);
        if (ret != 0)
            DBG("Can't set signal set, expect problems when terminating!\n");
    }else{
        DBG("Can't fill signal set, expect problems when terminating!\n");
    }

    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);				
}


int
module_write_data_ok(char *data)
{
    /* Tests */
    if(data == NULL){
        DBG("requested data NULL\n");		
        return -1;
    }

    if(data[0] == 0){
        DBG("requested data empty\n");
        return -1;
    }

    return 0;
}

int
module_terminate_thread(pthread_t thread)
{
    int ret;

    ret = pthread_cancel(thread);
    if (ret != 0){
        DBG("Cancelation of speak thread failed");
        return 1;
    }
    ret = pthread_join(thread, NULL);
    if (ret != 0){
        DBG("join failed!\n");
        return 1;
    }

    return 0;
}

sem_t*
module_semaphore_init()
{
    sem_t *semaphore;
    int ret;

    semaphore = (sem_t *) malloc(sizeof(sem_t));
    if (semaphore == NULL) return NULL;
    ret = sem_init(semaphore, 0, 0);
    if (ret != 0){
        DBG("Semaphore initialization failed");
        xfree(semaphore);
        semaphore = NULL;
    }
    return semaphore;
}

char *
module_recode_to_iso(char *data, int bytes, char *language, char *fallback)
{
    char *recoded;
    
    if (language == NULL) recoded = strdup(data);

    if (!strcmp(language, "cs"))
        recoded = (char*) g_convert_with_fallback(data, bytes, "ISO8859-2", "UTF-8",
                                                  fallback, NULL, NULL, NULL);
    else
        recoded = (char*) g_convert_with_fallback(data, bytes, "ISO8859-1", "UTF-8",
                                                  fallback, NULL, NULL, NULL);

    if(recoded == NULL) DBG("festival: Conversion to ISO coding failed\n");

    return recoded;
}

int
semaphore_post(int sem_id)
{
    static struct sembuf sem_b;

    sem_b.sem_num = 0;
    sem_b.sem_op = 1;          /* V() */
    sem_b.sem_flg = SEM_UNDO;
    return semop(sem_id, &sem_b, 1);
}


void
module_send_asynchronous(char *text)
{
    pthread_mutex_lock(&module_stdout_mutex);
    DBG("Printing reply: %s", text);
    fprintf(stdout, text);
    fflush(stdout);
    DBG("Printed");
    pthread_mutex_unlock(&module_stdout_mutex);
}

void
module_report_index_mark(char *mark)
{
    char *reply;
    DBG("Event: Index mark %s", mark);
    if (mark != NULL)
	reply = g_strdup_printf("700-%s\n700 INDEX MARK\n", mark);
    else
    	return;
    
    module_send_asynchronous(reply);
 
    xfree(reply);
}

void
module_report_event_begin(void)
{
    module_send_asynchronous("701 BEGIN\n");
}

void
module_report_event_end(void)
{
    module_send_asynchronous("702 END\n");
}

void
module_report_event_stop(void)
{
    module_send_asynchronous("703 STOP\n");
}

void
module_report_event_pause(void)
{
    module_send_asynchronous("704 PAUSE\n");
}



/* --- CONFIGURATION --- */
configoption_t *
module_add_config_option(configoption_t *options, int *num_options, char *name, int type,
                  dotconf_callback_t callback, info_t *info,
                  unsigned long context)
{
    configoption_t *opts;
    int num_config_options = *num_options;

    assert(name != NULL);

    num_config_options++;
    opts = (configoption_t*) realloc(options, (num_config_options+1) * sizeof(configoption_t));
    opts[num_config_options-1].name = (char*) strdup(name);
    opts[num_config_options-1].type = type;
    opts[num_config_options-1].callback = callback;
    opts[num_config_options-1].info = info;
    opts[num_config_options-1].context = context;

    *num_options = num_config_options;
    return opts;
}

void*
module_get_ht_option(GHashTable *hash_table, const char *key)
{
    void *option;
    assert(key != NULL);

    option = g_hash_table_lookup(hash_table, key);
    if (option == NULL) DBG("Requested option by key %s not found.\n", key);

    return option;
}


configoption_t *
add_config_option(configoption_t *options, int *num_config_options, char *name, int type,
                  dotconf_callback_t callback, info_t *info,
                  unsigned long context)
{
    configoption_t *opts;

    (*num_config_options)++;
    opts = (configoption_t*) realloc(options, (*num_config_options) * sizeof(configoption_t));
    opts[*num_config_options-1].name = strdup(name);
    opts[*num_config_options-1].type = type;
    opts[*num_config_options-1].callback = callback;
    opts[*num_config_options-1].info = info;
    opts[*num_config_options-1].context = context;
    return opts;
}

int
module_utils_init(void)
{
    /* Init mutex */
    pthread_mutex_init(&module_stdout_mutex, NULL);

    return 0;
}


#define ABORT(msg) g_string_append(info, msg); \
	*status_info = info->str; \
	g_string_free(info, 0); \
	return -1;

int
module_audio_init_spd(char **status_info)
{
  char * error;
  GString *info;
  char *module_audio_pars[10];
  char *outputs;
  int audio_output_set = 0;

  info = g_string_new("");
  outputs = audio_settings.audio_output_method;

    DBG("Openning audio output system");
    if (outputs[0]){
	char *next = outputs;
	size_t len;

	do{
	    outputs = next;
	    next = strchr(outputs, ',');

	    if (!outputs[0] || outputs[0] == ',')
		continue;

	    len = (next ? ((size_t)(next-outputs)) : strlen(outputs));

	    if (len == 3 && strncmp("oss", outputs, len) == 0){
		DBG("Using OSS audio output method");
		module_audio_pars[0] = strdup(audio_settings.audio_oss_device);
		module_audio_pars[1] = NULL;
		module_audio_id = spd_audio_open(AUDIO_OSS, (void**) module_audio_pars, &error);
		if (module_audio_id){
                    DBG("OSS initialization successful");
		    module_audio_output_method = AUDIO_OSS;
		    audio_output_set = 1;
		}
	    } else if (len == 5 && strncmp("libao", outputs, len) == 0){
		DBG("Using libao audio output method");
		module_audio_pars[0] = NULL;
		module_audio_pars[1] = NULL;
		module_audio_id = spd_audio_open(AUDIO_LIBAO, (void**) module_audio_pars, &error);
		if (module_audio_id){
                    DBG("libao initialization successful");
		    module_audio_output_method = AUDIO_LIBAO;
		    audio_output_set = 1;
		}
	    } else if (len == 4 && strncmp("alsa", outputs, len) == 0){
		DBG("Using Alsa audio output method");
		module_audio_pars[0] = audio_settings.audio_alsa_device;
		module_audio_pars[1] = NULL;
		module_audio_id = spd_audio_open(AUDIO_ALSA, (void**) module_audio_pars, &error);
		if (module_audio_id){
                    DBG("Alsa initialization successful");
		    module_audio_output_method = AUDIO_ALSA;
		    audio_output_set = 1;
		    break;
		}
	    } else if (len == 3 && strncmp("nas", outputs, len) == 0){
		DBG("Using NAS audio output method");
		module_audio_pars[0] = audio_settings.audio_nas_server;
		module_audio_pars[1] = NULL;
		module_audio_id = spd_audio_open(AUDIO_NAS, (void**) module_audio_pars, &error);
		if (module_audio_id){
                    DBG("NAS initialization successful");
		    module_audio_output_method = AUDIO_NAS;
		    audio_output_set = 1;
		    break;
		}
	    } else if (len == 5 && strncmp("pulse", outputs, len) == 0){
		DBG("Using PulseAudio output method");
		module_audio_pars[0] = (void *) audio_settings.audio_pulse_server;
		module_audio_pars[1] = (void *) audio_settings.audio_pulse_min_length;
		module_audio_id = spd_audio_open(AUDIO_PULSE, (void**) module_audio_pars, &error);
		if (module_audio_id){
                    DBG("PulseAudio initialization successful");
		    module_audio_output_method = AUDIO_PULSE;
		    audio_output_set = 1;
		    break;
		}
	    } else{
		ABORT("Sound output method specified in configuration not supported. "
		    "Please choose 'oss', 'alsa', 'nas' or 'pulse'.");
	    }
	} while(next++);
    }

    if (!audio_output_set){
	g_string_append_printf(info, "Opening sound device failed. Reason: %s. ", error);
	ABORT("Can't open sound device.");
	g_string_free(info, 1);
    }

    return 0;
}

#undef ABORT
