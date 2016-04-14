/*
 * festival.c - Speech Dispatcher backend for Festival
 *
 * Copyright (C) 2003, 2007 Brailcom, o.p.s.
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
- * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $Id: festival.c,v 1.82 2008-06-09 10:33:38 hanke Exp $
 */

#include "fdset.h"
#include "fdsetconv.h"

#include "module_utils.h"

#include "festival_client.c"

#define MODULE_NAME     "festival"
#define MODULE_VERSION  "0.5"

DECLARE_DEBUG()

/* Thread and process control */
static pthread_t festival_speak_thread;
static sem_t *festival_semaphore;
static int festival_speaking = 0;
static int festival_pause_requested = 0;

static char **festival_message;
static EMessageType festival_message_type;
signed int festival_volume = 0;

int festival_stop_request = 0;
int festival_stop = 0;

int festival_process_pid = 0;

FT_Info *festival_info = NULL;

VoiceDescription** festival_voice_list = NULL;

enum{
    FCT_SOCKET = 0,
    FCT_LOCAL = 1,
} FestivalComType;

struct{
    int pipe_in[2];
    int pipe_out[2];
    int pid;
}module_p;
  
#define COM_SOCKET ((FestivalComType == FCT_SOCKET) ? 1 : 0)
#define COM_LOCAL ((FestivalComType == FCT_LOCAL) ? 1 : 0)

/* Internal functions prototypes */
void* _festival_speak(void*);

void festival_parent_clean();

void festival_set_rate(signed int rate);
void festival_set_pitch(signed int pitch);
void festival_set_voice(EVoiceType voice);
void festival_set_synthesis_voice(char* synthesis_voice);
void festival_set_language(char* language);
void festival_set_punctuation_mode(EPunctMode punct);
void festival_set_cap_let_recogn(ECapLetRecogn recogn);
void festival_set_volume(signed int volume);

int init_festival_standalone();
int init_festival_socket();

int is_text(EMessageType msg_type);

MOD_OPTION_1_INT(FestivalComunicationType)

MOD_OPTION_1_INT(FestivalMaxChunkLength)
MOD_OPTION_1_STR(FestivalDelimiters)
MOD_OPTION_1_STR(FestivalServerHost)
MOD_OPTION_1_STR(FestivalStripPunctChars)
MOD_OPTION_1_INT(FestivalServerPort)
MOD_OPTION_1_INT(FestivalPitchDeviation)
MOD_OPTION_1_INT(FestivalDebugSaveOutput)
MOD_OPTION_1_STR(FestivalRecodeFallback)

MOD_OPTION_1_INT(FestivalCacheOn)
MOD_OPTION_1_INT(FestivalCacheMaxKBytes)
MOD_OPTION_1_INT(FestivalCacheDistinguishVoices)
MOD_OPTION_1_INT(FestivalCacheDistinguishRate)
MOD_OPTION_1_INT(FestivalCacheDistinguishPitch)

MOD_OPTION_1_INT(FestivalReopenSocket)

typedef struct{
    size_t size;
    GHashTable *caches;
    GList *cache_counter;
}TCache;

typedef struct{
    time_t start;
    int count;
    size_t size;
    GHashTable *p_caches;
    char *key;
}TCounterEntry;

typedef struct{
    TCounterEntry *p_counter_entry;
    FT_Wave *fwave;
}TCacheEntry;

TCache FestivalCache;

int cache_init();
int cache_reset();
int cache_insert(char* key, EMessageType msgtype, FT_Wave *value);
FT_Wave* cache_lookup(const char *key, EMessageType msgtype, int add_counter);

pthread_mutex_t sound_output_mutex;

/* Public functions */

int
module_load(void)
{

    INIT_SETTINGS_TABLES();

    REGISTER_DEBUG();

    MOD_OPTION_1_INT_REG(FestivalComunicationType, 0);

    MOD_OPTION_1_STR_REG(FestivalServerHost, "localhost");
    MOD_OPTION_1_INT_REG(FestivalServerPort, 1314);

    MOD_OPTION_1_INT_REG(FestivalDebugSaveOutput, 0);

    MOD_OPTION_1_STR_REG(FestivalRecodeFallback, "?");

    MOD_OPTION_1_INT_REG(FestivalCacheOn, 1);
    MOD_OPTION_1_INT_REG(FestivalCacheMaxKBytes, 5120);
    MOD_OPTION_1_INT_REG(FestivalCacheDistinguishVoices, 0);
    MOD_OPTION_1_INT_REG(FestivalCacheDistinguishRate, 0);
    MOD_OPTION_1_INT_REG(FestivalCacheDistinguishPitch, 0);

    /* TODO: Maybe switch this option to 1 when the bug with the 40ms delay
     in Festival is fixed */
    MOD_OPTION_1_INT_REG(FestivalReopenSocket, 0);

    module_audio_id = NULL;

    return 0;
}

#define ABORT(msg) g_string_append(info, msg); \
	*status_info = info->str; \
	g_string_free(info, 0); \
	return -1;

int
module_init(char **status_info)
{
  int ret;

    GString *info;

    info = g_string_new("");

    DBG("module_init()");

    INIT_INDEX_MARKING();

    /* Initialize appropriate communication mechanism */
    FestivalComType = FestivalComunicationType;
    if (COM_SOCKET){
	g_string_append(info, "Communicating with Festival through a socket. ");
	ret = init_festival_socket();
	if (ret == -1){
	    ABORT("Can't connect to Festival server. Check your configuration "
		  "in etc/speechd-modules/festival.conf for the specified host and port "
		  "and check if Festival is really running there, e.g. with telnet. "
		  "Please see documentation for more info.");
	}else if (ret == -2){
	    ABORT("Connect to the Festival server was successful, "
		  "but I got disconnected immediately. This is most likely "
		  "because of authorization problems. Check the variable "
		  "server_access_list in etc/festival.scm and consult documentation "
		  "for more information.");
	}
    }
    if (COM_LOCAL){
	g_string_append(info, "Communicating with Festival through a local child process.");
	if(init_festival_standalone()){
	    ABORT("Local connect to Festival failed for unknown reasons.");
	}
    }

    /* Get festival voice list */
    festival_voice_list = festivalGetVoices(festival_info);

    /* Initialize global variables */
    festival_message = (char**) xmalloc (sizeof (char*));    
    *festival_message = NULL;

    /* Initialize festival_speak thread to handle communication
     with festival in a separate thread (to be faster in communication
     with Speech Dispatcher) */
    festival_semaphore = module_semaphore_init();
    if (festival_semaphore == NULL) return -1;
    DBG("Festival: creating new thread for festival_speak\n");
    festival_speaking = 0;
    ret = pthread_create(&festival_speak_thread, NULL, _festival_speak, NULL);
    if(ret != 0){
        DBG("Festival: thread failed\n");
	g_string_append(info, "The module couldn't initialize threads"
			"This can be either an internal problem or an"
                        "architecture problem. If you are sure your architecture"
			"supports threads, please report a bug.");
	*status_info = info->str;
	g_string_free(info, 0);
	return -1;
    }

    pthread_mutex_init(&sound_output_mutex, NULL);

    *status_info = info->str;
    g_string_free(info, 0);

    return 0;
}

#undef ABORT

int
module_audio_init(char **status_info){
  return module_audio_init_spd(status_info);
}

VoiceDescription**
module_list_voices(void)
{
  return festival_voice_list;
}

int
module_speak(char *data, size_t bytes, EMessageType msgtype)
{
    int ret;

    DBG("module_speak()\n");

    if (data == NULL) return -1;

    if (festival_speaking){
	DBG("Speaking when requested to write\n");
	return -1;
    }

    festival_stop_request = 0;

    /* Check the parameters */
    if(module_write_data_ok(data) != 0) return -1;

    festival_message_type = msgtype;
    if ((msgtype == MSGTYPE_TEXT) && (msg_settings.spelling_mode == SPELLING_ON))
        festival_message_type = MSGTYPE_SPELL;


    /* If the connection crashed or language or voice
       change, we will need to set all the parameters again */
    if (COM_SOCKET){
	if (festival_connection_crashed){
	    DBG("Recovering after a connection loss");
	    CLEAN_OLD_SETTINGS_TABLE();
	    festival_info = festivalOpen(festival_info);
	    if (festival_info) festival_connection_crashed = 0;
	    else{
		DBG("Can't recover. Not possible to open connection to Festival.");
		return -1;
	    }
	    ret = FestivalSetMultiMode(festival_info, "t");
	    if (ret != 0) return -1;
	}
    }

    /* If the voice was changed, re-set all the parameters */
    // TODO: Handle synthesis_voice change too 
    if ((msg_settings.voice != msg_settings_old.voice)
	|| ((msg_settings.language != NULL) && (msg_settings_old.language!=NULL) &&
	    (strcmp(msg_settings.language, msg_settings_old.language)))){
	DBG("Cleaning old settings table");
        CLEAN_OLD_SETTINGS_TABLE();
    }


    /* Setting voice parameters */
    DBG("Updating parameters");
    UPDATE_STRING_PARAMETER(language, festival_set_language);
    UPDATE_PARAMETER(voice, festival_set_voice);
    UPDATE_STRING_PARAMETER(synthesis_voice, festival_set_synthesis_voice);
    UPDATE_PARAMETER(rate, festival_set_rate);
    UPDATE_PARAMETER(pitch, festival_set_pitch);
    UPDATE_PARAMETER(volume, festival_set_volume);
    UPDATE_PARAMETER(punctuation_mode, festival_set_punctuation_mode);
    UPDATE_PARAMETER(cap_let_recogn, festival_set_cap_let_recogn);

    if (festival_connection_crashed){
	DBG("ERROR: Festival connection not working!");
	return -1;
    }

    DBG("Requested data: |%s| \n", data);

    xfree(*festival_message);
    *festival_message = strdup(data);
    if (*festival_message == NULL){
        DBG("Error: Copying data unsuccesful.");
        return -1;
    }

    /* Send semaphore signal to the speaking thread */
    festival_speaking = 1;
    sem_post(festival_semaphore);

    DBG("Festival: leaving write() normaly\n\r");
    return bytes;
}

int
module_stop(void)
{
    DBG("stop()\n");

    if(festival_speaking){
	/* if(COM_SOCKET) */
	if (0){
	    if (festival_info != 0)
		if ((festival_info->server_fd != -1) && FestivalReopenSocket){
		    /* TODO: Maybe use shutdown here? */
		    close(festival_info->server_fd);
		    festival_info->server_fd = -1;
		    festival_connection_crashed = 1;
		    DBG("festival socket closed by module_stop()");
		}
	}
	if (COM_LOCAL){
	    DBG("festival local stopped by sending SIGINT");
	    /* TODO: Write this function for local communication */
	    //	    festival_stop_local();
	}

	if (!festival_stop){
	    pthread_mutex_lock(&sound_output_mutex);       
	    festival_stop = 1;
	    if (festival_speaking && module_audio_id){
		spd_audio_stop(module_audio_id);
	    }
	    pthread_mutex_unlock(&sound_output_mutex);
	}
    }
    
    return 0;
}

size_t
module_pause(void)
{
    DBG("pause requested\n");
    if(festival_speaking){
        DBG("Sending request for pause to child\n");
        festival_pause_requested = 1;
        DBG("Signalled to pause");
        return 0;
    }else{
        return -1;
    }
}

void
module_close(int status)
{
    
    DBG("festival: close()\n");

    DBG("Stopping the module");
    while(festival_speaking){
        module_stop();
        usleep(50);
    }

    // DBG("festivalClose()");
    // festivalClose(festival_info);
    
    DBG("Terminating threads");
    module_terminate_thread(festival_speak_thread);
   
    if (festival_info)
	delete_FT_Info(festival_info);

    /* TODO: Solve this */
    //    DBG("Removing junk files in tmp/");
    //    system("rm -f /tmp/est* 2> /dev/null");

    DBG("Closing audio output");
    if (module_audio_id)
	spd_audio_close(module_audio_id);

    exit(status);
}


/* Internal functions */

#define CLEAN_UP(code, im) \
        { \
          if(!wave_cached) if (fwave) delete_FT_Wave(fwave); \
          pthread_mutex_lock(&sound_output_mutex); \
          festival_stop = 0; \
          festival_speaking = 0; \
          pthread_mutex_unlock(&sound_output_mutex); \
          im(); \
          goto sem_wait; \
        }

#define CLP(code, im) \
        { \
          pthread_mutex_lock(&sound_output_mutex); \
          festival_stop = 0; \
          festival_speaking = 0; \
          pthread_mutex_unlock(&sound_output_mutex); \
          im(); \
          goto sem_wait; \
        }


int
festival_send_to_audio(FT_Wave *fwave)
{
    AudioTrack track;
    int ret = 0;
    
    track.num_samples = fwave->num_samples;
    track.num_channels = 1;
    track.sample_rate = fwave->sample_rate;
    track.bits = 16;
    track.samples = fwave->samples;
    
    if (track.samples != NULL){
	DBG("Sending to audio");
		switch (spd_audio_endian){
			case SPD_AUDIO_LE:
				ret = spd_audio_play(module_audio_id, track, SPD_AUDIO_LE);
				break;
			case SPD_AUDIO_BE:
				ret = spd_audio_play(module_audio_id, track, SPD_AUDIO_BE);
				break;
		}
	if (ret < 0) DBG("ERROR: Can't play track for unknown reason.");
	DBG("Sent to audio.");
    }

    return 0;
}

void*
_festival_speak(void* nothing)
{	

    int ret;
    int bytes;
    int wave_cached;
    FT_Wave *fwave;
    int debug_count = 0;
    int r;
    int terminate = 0;


    char *callback;

    DBG("festival: speaking thread starting.......\n");

    cache_init();

    set_speaking_thread_parameters();

    while(1){        
    sem_wait:
        sem_wait(festival_semaphore);
        DBG("Semaphore on, speaking\n");

	spd_audio_set_volume(module_audio_id, festival_volume);

	festival_stop = 0;	
	festival_speaking = 1;
	wave_cached = 0;
	fwave = NULL;

	terminate = 0;     

	bytes = strlen(*festival_message);

	module_report_event_begin();

	DBG("Going to synthesize: |%s|", *festival_message);
	if (bytes > 0){
	    if (!is_text(festival_message_type)){	/* it is a raw text */	     
		DBG("Cache mechanisms...");
		fwave = cache_lookup(*festival_message, festival_message_type, 1);
		if (fwave != NULL){
		    wave_cached = 1;
		    if (fwave->num_samples != 0){
			if(FestivalDebugSaveOutput){
			    char filename_debug[256];
			    sprintf(filename_debug, "/tmp/debug-festival-%d.snd", debug_count++);
			    save_FT_Wave_snd(fwave, filename_debug);
			}
						
			festival_send_to_audio(fwave);
			
			if (!festival_stop){
			    CLEAN_UP(0, module_report_event_end);
			}else{			
			    CLEAN_UP(0, module_report_event_stop);
			}

		    }else{
			CLEAN_UP(0, module_report_event_end);
		    }
		}
	    }
	    
	    /*  Set multi-mode for appropriate kind of events */
	    if (is_text(festival_message_type)){	/* it is a raw text */
		ret = FestivalSetMultiMode(festival_info, "t");
		if (ret != 0) CLP(0, module_report_event_stop);
	    }else{			/* it is some kind of event */
		ret = FestivalSetMultiMode(festival_info, "nil");
		if (ret != 0) CLP(0, module_report_event_stop);
	    }

	    switch(festival_message_type)
		{
		case MSGTYPE_TEXT: r = festivalStringToWaveRequest(festival_info, *festival_message); break;
		case MSGTYPE_SOUND_ICON: r = festivalSoundIcon(festival_info, *festival_message); break;
		case MSGTYPE_CHAR: r = festivalCharacter(festival_info, *festival_message); break;
		case MSGTYPE_KEY: r = festivalKey(festival_info, *festival_message); break;
		case MSGTYPE_SPELL: r = festivalSpell(festival_info, *festival_message); break;
		default: r = -1;
		}
	    if (r < 0){
		DBG("Couldn't process the request to say the object.");
		CLP(0, module_report_event_stop);
	    }
	}
    
	while(1){

	    wave_cached = 0;
	    DBG("Retrieving data\n");	   

	    /* (speechd-next) */
	    if (is_text(festival_message_type)){

		if (festival_stop){
		    DBG("Module stopped 1");
		    CLEAN_UP(0, module_report_event_stop);
		}

		DBG("Getting data in multi mode");
		fwave = festivalGetDataMulti(festival_info, &callback,
					     &festival_stop_request,
					     FestivalReopenSocket);

		if (callback != NULL){ 
		    if((festival_pause_requested) && (!strncmp(callback, INDEX_MARK_BODY,
							       INDEX_MARK_BODY_LEN))){
			DBG("Pause requested, pausing.");
			module_report_index_mark(callback);
			xfree(callback);
			festival_pause_requested = 0;
			CLEAN_UP(0, module_report_event_pause);
		    }else{
			module_report_index_mark(callback);
			xfree(callback);
			continue;
		    }
		}
	    }else{			/* is event */
		DBG("Getting data in single mode");
		fwave = festivalStringToWaveGetData(festival_info);
		terminate = 1;
		callback = NULL;
	    }

	    if (fwave == NULL){
		DBG("End of sound samples, terminating this message...");
		CLEAN_UP(0, module_report_event_end);
	    }
	
	    if (festival_message_type == MSGTYPE_CHAR 
		|| festival_message_type == MSGTYPE_KEY 
		|| festival_message_type == MSGTYPE_SOUND_ICON){
		DBG("Storing record for %s in cache\n", *festival_message);
		/* cache_insert takes care of not inserting the same
		 message again */
		cache_insert(strdup(*festival_message), festival_message_type, fwave);
		wave_cached = 1;
	    }

	    if (festival_stop){
		DBG("Module stopped 2");
		CLEAN_UP(0, module_report_event_stop);
	    }       	

	    if (fwave->num_samples != 0){
		DBG("Sending message to audio: %ld bytes\n",
		    (fwave->num_samples) * sizeof(short));	
    
		if(FestivalDebugSaveOutput){
		    char filename_debug[256];
		    sprintf(filename_debug, "/tmp/debug-festival-%d.snd", debug_count++);
		    save_FT_Wave_snd(fwave, filename_debug);
		}
	    
		DBG("Playing sound samples");
		festival_send_to_audio(fwave);

		if (!wave_cached) delete_FT_Wave(fwave);
		DBG("End of playing sound samples");
	    }            

	    if (terminate){
		DBG("Ok, end of samples, returning");
		CLP(0, module_report_event_end);
	    }

	    if (festival_stop){
		DBG("Module stopped 3");
		CLP(0, module_report_event_stop);
	    }
	}

    }


    festival_stop = 0;
    festival_speaking = 0;
	
    DBG("festival: speaking thread ended.......\n");    
    
    pthread_exit(NULL);
}	


int
is_text(EMessageType msg_type)
{
    if (msg_type == MSGTYPE_TEXT || msg_type == MSGTYPE_SPELL) return 1;
    else return 0;
}


void
festival_set_language(char* language)
{   
    FestivalSetLanguage(festival_info, language, NULL);
    xfree(festival_voice_list);
    festival_voice_list = festivalGetVoices(festival_info);
}

void
festival_set_voice(EVoiceType voice)
{
    char* voice_name;

    voice_name = EVoice2str(voice);
    FestivalSetVoice(festival_info, voice_name, NULL);
    xfree(voice_name);
}

void
festival_set_synthesis_voice(char *voice_name)
{

  FestivalSetSynthesisVoice(festival_info, voice_name);
}

void
festival_set_rate(signed int rate)
{
    FestivalSetRate(festival_info, rate);
}

void
festival_set_pitch(signed int pitch)
{
    FestivalSetPitch(festival_info, pitch);
}

void
festival_set_volume(signed int volume)
{
  festival_volume = volume;
}

void
festival_set_punctuation_mode(EPunctMode punct)
{
    char *punct_mode;
    punct_mode = EPunctMode2str(punct);
    FestivalSetPunctuationMode(festival_info, punct_mode);
    xfree(punct_mode);
}

void
festival_set_cap_let_recogn(ECapLetRecogn recogn)
{
    char *recogn_mode;

    if (recogn == RECOGN_NONE) recogn_mode = NULL;
    else recogn_mode = ECapLetRecogn2str(recogn);
    FestivalSetCapLetRecogn(festival_info, recogn_mode, NULL);
    xfree(recogn_mode);
}


/* --- Cache related functions --- */

void
cache_destroy_entry(gpointer data)
{
    TCacheEntry *entry = data;
    xfree(entry->fwave);
    xfree(entry);
}

void
cache_destroy_table_entry(gpointer data)
{
    g_hash_table_destroy(data);
}

void
cache_free_counter_entry(gpointer data, gpointer user_data)
{
    xfree(((TCounterEntry*)data)->key);
    xfree(data);
}

int
cache_init()
{

    if (FestivalCacheOn == 0) return 0;

    FestivalCache.size = 0;
    FestivalCache.caches = g_hash_table_new_full(g_str_hash, g_str_equal, free,
                                                 cache_destroy_table_entry);
    FestivalCache.cache_counter = NULL;
    DBG("Cache: initialized");
    return 0;
}

int
cache_destroy()
{
    g_hash_table_destroy(FestivalCache.caches);
    g_list_foreach(FestivalCache.cache_counter, cache_free_counter_entry, NULL);
    g_list_free(FestivalCache.cache_counter);
    return 0;
}

int
cache_reset()
{
    /* TODO: it could free everything in the cache and go from start,
       but currently it isn't called by anybody */
    return 0;
}

/* Compare two cache entries according to their score (how many
   times the entry was requested divided by the time it exists
   in the database) */
gint
cache_counter_comp(gconstpointer a, gconstpointer b)
{
    const TCounterEntry *A = a;
    const TCounterEntry *B = b;
    time_t t;
    float ret;

    t = time(NULL);
    ret =  (((float) A->count  / (float)(t - A->start)) 
             - ((float) B->count / (float)(t - B->start)));

    if (ret > 0) return -1;
    if (ret == 0) return 0;
    if (ret < 0) return 1;

    return 0;
}

/* List scores of all entries in the cache*/
void
cache_debug_foreach_list_score(gpointer a, gpointer user)
{
    const TCounterEntry *A = a;
    time_t t;

    t = time(NULL);
    DBG("key: %s      -> score %f (count: %d, dtime: %d)", A->key, ((float) A->count  / (float)(t - A->start)),
        (int) A->count, (int) (t - A->start));
}

/* Remove 1/3 of the least used (according to cache_counter_comp) entries 
   (measured by size) */
int
cache_clean(size_t new_element_size)
{
    size_t req_size;
    GList *gl;
    TCounterEntry *centry;

    DBG("Cache: cleaning, cache size %ld kbytes (>max %d).", FestivalCache.size/1024,
        FestivalCacheMaxKBytes);

    req_size = 2*FestivalCache.size/3;

    FestivalCache.cache_counter = g_list_sort(FestivalCache.cache_counter, cache_counter_comp);
    g_list_foreach(FestivalCache.cache_counter, cache_debug_foreach_list_score, NULL);

    while((FestivalCache.size + new_element_size) > req_size){
        gl = g_list_last(FestivalCache.cache_counter);
        if (gl == NULL) break;
        if (gl->data == NULL){
            DBG("Error: Cache: gl->data in cache_clean is NULL, but shouldn't be.");
            return -1;
        }
        centry = gl->data;
        FestivalCache.size -= centry->size;       
        DBG("Cache: Removing element with key '%s'", centry->key);
        if (FestivalCache.size < 0){
            DBG("Error: Cache: FestivalCache.size < 0, this shouldn't be.");
            return -1;
        }
        /* Remove the data itself from the hash table */
        g_hash_table_remove(centry->p_caches, centry->key);
        /* Remove the associated entry in the counter list */
        cache_free_counter_entry(centry, NULL);
        FestivalCache.cache_counter = g_list_delete_link(FestivalCache.cache_counter, gl);
    }

    return 0;
}

/* Generate a key for searching between the different hash tables */
char*
cache_gen_key(EMessageType type)
{
    char *key;
    char ktype;    
    int kpitch = 0, krate = 0, kvoice = 0;

    if (msg_settings.language == NULL) return NULL;

    DBG("v, p, r = %d %d %d", FestivalCacheDistinguishVoices, FestivalCacheDistinguishPitch, FestivalCacheDistinguishRate);

    if (FestivalCacheDistinguishVoices) kvoice = msg_settings.voice;
    if (FestivalCacheDistinguishPitch) kpitch = msg_settings.pitch;
    if (FestivalCacheDistinguishRate) krate = msg_settings.rate;

    if (type == MSGTYPE_CHAR) ktype = 'c';
    else if (type == MSGTYPE_KEY) ktype = 'k';
    else if (type == MSGTYPE_SOUND_ICON) ktype = 's';
    else{
	DBG("Invalid message type for cache_gen_key()");
	return NULL;
    }

    key = g_strdup_printf("%c_%s_%d_%d_%d", ktype, msg_settings.language, kvoice,
                                krate, kpitch);

    return key;
}

/* Insert one entry into the cache */
int
cache_insert(char* key, EMessageType msgtype, FT_Wave *fwave)
{
    GHashTable *cache;
    TCacheEntry *entry;
    TCounterEntry *centry;
    char *key_table;

    if (FestivalCacheOn == 0) return 0;

    if (key == NULL) return -1;
    if (fwave == NULL) return -1;

    /* Check if the entry isn't present already */
    if (cache_lookup(key, msgtype, 0) != NULL)
        return 0;

    key_table = cache_gen_key(msgtype);

    DBG("Cache: Inserting wave with key:'%s' into table '%s'", key, key_table);

    /* Clean less used cache entries if the size would exceed max. size */
    if ((FestivalCache.size + fwave->num_samples * sizeof(short))
        > (FestivalCacheMaxKBytes*1024))
        if (cache_clean(fwave->num_samples * sizeof(short)) != 0) return -1;
    

    /* Select the right table according to language, voice, etc. or create a new one*/
    cache = g_hash_table_lookup(FestivalCache.caches, key_table);
    if (cache == NULL){
        cache = g_hash_table_new(g_str_hash, g_str_equal);
        g_hash_table_insert(FestivalCache.caches, key_table, cache);
    }else{
        xfree(key_table);
    }

    /* Fill the CounterEntry structure that will later allow us to remove
       the less used entries from cache */
    centry = (TCounterEntry*) xmalloc(sizeof(TCounterEntry));
    centry->start = time(NULL);
    centry->count = 1;
    centry->size = fwave->num_samples * sizeof(short);
    centry->p_caches = cache;
    centry->key = strdup(key);
    FestivalCache.cache_counter = g_list_append(FestivalCache.cache_counter, centry);

    entry = (TCacheEntry*) xmalloc(sizeof(TCacheEntry));
    entry->p_counter_entry = centry;
    entry->fwave = fwave;

    FestivalCache.size += centry->size;
    g_hash_table_insert(cache, strdup(key), entry);

    return 0;
}

/* Retrieve wave from the cache */
FT_Wave*
cache_lookup(const char *key, EMessageType msgtype, int add_counter)
{
    GHashTable *cache;
    TCacheEntry *entry;
    char *key_table;

    if (FestivalCacheOn == 0) return NULL;
    if (key == NULL) return NULL;

    key_table = cache_gen_key(msgtype);

    if (add_counter) DBG("Cache: looking up a wave with key '%s' in '%s'", key, key_table);

    if (key_table == NULL) return NULL;
    cache = g_hash_table_lookup(FestivalCache.caches, key_table);
    xfree(key_table);
    if (cache == NULL) return NULL;   

    entry = g_hash_table_lookup(cache, key);
    if (entry == NULL) return NULL;
    entry->p_counter_entry->count++;

    DBG("Cache: corresponding wave found: %s", key);    

    return entry->fwave;
}


int
init_festival_standalone()
{
    int ret;
    int fr;

    if ( (pipe(module_p.pipe_in) != 0) 
          || ( pipe(module_p.pipe_out) != 0)  ){
        DBG("Can't open pipe! Module not loaded.");
        return -1;
    }     

    DBG("Starting Festival as a child process");

    fr = fork();
    switch(fr){
    case -1: DBG("ERROR: Can't fork! Module not loaded."); return -1;
    case 0:
        ret = dup2(module_p.pipe_in[0], 0);
        close(module_p.pipe_in[0]);
        close(module_p.pipe_in[1]);
 
        ret = dup2(module_p.pipe_out[1], 1);
        close(module_p.pipe_out[1]);
        close(module_p.pipe_out[0]);        
        
	/* TODO: fix festival hardcoded path */
	if (execlp("festival", "", (char *) 0) == -1)
	    exit(1);	
                
    default:
        festival_process_pid = fr;
        close(module_p.pipe_in[0]);
        close(module_p.pipe_out[1]);

        usleep(100);            /* So that the other child has at least time to fail
                                   with the execlp */
        ret = waitpid(module_p.pid, NULL, WNOHANG);
        if (ret != 0){
            DBG("Can't execute festival. Bad filename in configuration?");            
            return -1;
        }
	
        return 0;
    }
    
    assert(0);
}

int
init_festival_socket()
{
    int r;

    /* Init festival and register a new voice */
    festival_info = festivalDefaultInfo();
    festival_info->server_host = FestivalServerHost;
    festival_info->server_port = FestivalServerPort;
    
    festival_info = festivalOpen(festival_info);
    if (festival_info == NULL) return -1;
    r = FestivalSetMultiMode(festival_info, "t");
    if (r != 0) return -2;
    
    DBG("FestivalServerHost = %s\n", FestivalServerHost);
    DBG("FestivalServerPort = %d\n", FestivalServerPort);

    return 0;
}

int
stop_festival_local()
{
    if (festival_process_pid > 0)
	kill(festival_process_pid, SIGINT);
    return 0;
}


#include "module_main.c"
