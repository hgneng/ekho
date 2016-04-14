
/*
 * ivona_client.c - Speech Dispatcher backend for Ivona (IVO Software)
 *
 * Copyright (C) Bohdan R. Rau 2008 <ethanak@polip.com>
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
 */

#include <fcntl.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static struct sockaddr_in sinadr;
static char *ivona_get_wave_from_cache(char *to_say,int *nsamples);
static void ivona_store_wave_in_cache(char *to_say,char *wave,int nsamples);

int ivona_init_sock(void)
{
	if (!inet_aton(IvonaServerHost,&sinadr.sin_addr)) {
		struct hostent *h = gethostbyname(IvonaServerHost);
		if (!h) return -1;
		memcpy(&sinadr.sin_addr, h->h_addr, sizeof(struct in_addr));
		endhostent();
	}
	sinadr.sin_family = AF_INET;
	sinadr.sin_port = htons(IvonaServerPort);
	return 0;
}

static int get_unichar(char **str)
{
	wchar_t wc;
	int n;
	wc=*(*str)++ & 255;
	if ((wc & 0xe0)==0xc0) {
		wc &=0x1f;
		n=1;
	}
	else if ((wc & 0xf0)==0xe0) {
		wc &=0x0f;
		n=2;
	}
	else if ((wc & 0xf8)==0xf0) {
		wc &=0x07;
		n=3;
	}
	else if ((wc & 0xfc)==0xf8) {
		wc &=0x03;
		n=4;
	}
	else if ((wc & 0xfe)==0xfc) {
		wc &=0x01;
		n=5;
	}
	else return wc;
	while (n--) {
		if ((**str & 0xc0) != 0x80) {
			wc='?';
			break;
		}
		wc=(wc << 6) | ((*(*str)++) & 0x3f);
	}
	return wc;
}

int ivona_get_msgpart(char **msg,char *icon,char **buf,int *len)
{
	int rc;
	int isicon;
	int n,pos,bytes;
	wchar_t wc;
	char xbuf[1024];
	
	if (!*msg) return 1;
	if (!**msg) return 1;
	isicon=0;
	icon[0]=0;
	if (*buf) **buf=0;
	DBG("Ivona message %s type %d\n",*msg,ivona_message_type);
	switch(ivona_message_type) {
		case MSGTYPE_SOUND_ICON:
		if (strlen(*msg)<63) {
			strcpy(icon,*msg);
			rc=0;
		}
		else {
			rc=1;
		}
		*msg=NULL;
		return rc;
		
		case MSGTYPE_SPELL:
		wc=get_unichar(msg);
		if (!wc) {
			*msg=NULL;
			return 1;
		}
		n=dumbtts_WCharString(ivona_conf,wc,*buf,*len,ivona_cap_mode,&isicon);
		if (n>0) {
			*len=n+128;
			*buf=xrealloc(*buf,*len);
			n=dumbtts_WCharString(ivona_conf,wc,*buf,*len,ivona_cap_mode,&isicon);
		}
		if (n) {
			*msg=NULL;
			return 1;
		}
		if (isicon) strcpy(icon,"capital");
		return 0;
		
		case MSGTYPE_KEY:
		case MSGTYPE_CHAR:
		
		if (ivona_message_type == MSGTYPE_KEY) {
			n=dumbtts_KeyString(ivona_conf,*msg,*buf,*len,ivona_cap_mode,&isicon);
		}
		else {
			n=dumbtts_CharString(ivona_conf,*msg,*buf,*len,ivona_cap_mode,&isicon);
		}
		DBG("Got n=%d",n);
		if (n>0) {
			*len=n+128;
			*buf=xrealloc(*buf,*len);
			if (ivona_message_type == MSGTYPE_KEY) {
				n=dumbtts_KeyString(ivona_conf,*msg,*buf,*len,ivona_cap_mode,&isicon);
			}
			else {
				n=dumbtts_CharString(ivona_conf,*msg,*buf,*len,ivona_cap_mode,&isicon);
			}
		}
		*msg=NULL;
		
		if (!n && isicon) strcpy(icon,"capital");
		return n;
		
		case MSGTYPE_TEXT:
		pos=0;
		bytes=module_get_message_part(*msg,xbuf,&pos, 1023,IvonaDelimiters);
		DBG("Got bytes %d, %s",bytes,xbuf);
		if (bytes <= 0) {
			*msg=NULL;
			return 1;
		}
		*msg+=pos;
		xbuf[bytes]=0;
		
		
		n=dumbtts_GetString(ivona_conf,xbuf,*buf,*len,ivona_punct_mode,IvonaPunctuationSome,",.;:!?");
		
		if (n>0) {
			*len=n+128;
			*buf=xrealloc(*buf,*len);
			n=dumbtts_GetString(ivona_conf,xbuf,*buf,*len,ivona_punct_mode,IvonaPunctuationSome,",.;:!?");
		}
		if (n) {
			*msg=NULL;
			return 1;
		}
		DBG("Returning to Ivona |%s|",*buf);
		return 0;
		
		default:
		
		*msg=NULL;
		DBG("Unknown message type\n");
		return 1;	
	}
}


#define BASE_WAVE_SIZE 65536
#define STEP_WAVE_SIZE 32768

int ivona_send_string(char *to_say)
{
	int fd;
	
	fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0) return -1; 
	if (connect(fd, (struct sockaddr *) &sinadr, sizeof(sinadr)) < 0) {
		close(fd);
		return -1;
	}
	write(fd,to_say,strlen(to_say));
	write(fd,"\n",1);
	return fd;
	
}

char *ivona_get_wave_fd(int fd,int *nsamples,int *offset)
{
	int got,i;
	char *ivona_wave;
	int wave_size;
	int wave_length;
	short *w;
	
	wave_size=BASE_WAVE_SIZE;
	wave_length=0;
	ivona_wave=xmalloc(wave_size);
	for (;;) {
		if (wave_size <wave_length + 8192) {
			ivona_wave=xrealloc(ivona_wave,wave_size + STEP_WAVE_SIZE);
			wave_size += STEP_WAVE_SIZE;
		}
		DBG("Have place for %d bytes",wave_size - wave_length);
		got=read(fd,ivona_wave + wave_length,wave_size - wave_length);
		DBG("Wave part at %d size %d",wave_length,got);
		if (got <= 0) break;
		wave_length += got;
	}
	close(fd);
	w=(short *) ivona_wave;
	for (i=wave_length/2-1;i>=0;i--) if (w[i]) break;
	if (i<100) {
		xfree(ivona_wave);
		return NULL;
	}
	DBG("Trimmed %d samples at end",wave_length/2-i-1);
	*nsamples=i+1;
	for (i=0;i<*nsamples;i++) if (w[i]) break;
	DBG("Should trim %d bytes at start",i);
	*offset=i;
	(*nsamples)-=i;
	return ivona_wave;
}
/*
static char *ivona_get_wave_from_cache(char *to_say,int *nsamples);
void ivona_store_wave_in_cache(char *to_say,char *wave,int nsamples);
*/
char *ivona_get_wave(char *to_say,int *nsamples,int *offset)
{
	int fd;
	char *s;
	s=ivona_get_wave_from_cache(to_say,nsamples);
	if (s) {
		*offset=0;
		return s;
	}
	fd=ivona_send_string(to_say);
	if (fd<0) return NULL;
	s=ivona_get_wave_fd(fd,nsamples,offset);
	if (s) ivona_store_wave_in_cache(to_say,s+2*(*offset),*nsamples);
	return s;
}




/* Plays the specified audio file - from ibmtts/espeak module */

static gboolean
ivona_play_file(char *filename)
{
	gboolean result = TRUE;
#if HAVE_SNDFILE
	int subformat;
	sf_count_t items;
	sf_count_t readcount;
	SNDFILE* sf;
	SF_INFO sfinfo;

	DBG("Ivona: Playing |%s|", filename);
	memset (&sfinfo, 0, sizeof (sfinfo));
	sf = sf_open(filename, SFM_READ, &sfinfo);
	subformat = sfinfo.format & SF_FORMAT_SUBMASK ;
	items = sfinfo.channels * sfinfo.frames;
	DBG("Ivona: frames = %ld, channels = %d", (long) sfinfo.frames, sfinfo.channels);
	DBG("Ivona: samplerate = %i, items = %Ld", sfinfo.samplerate, (long long) items);
	DBG("Ivona: major format = 0x%08X, subformat = 0x%08X, endian = 0x%08X",
		sfinfo.format & SF_FORMAT_TYPEMASK, subformat, sfinfo.format & SF_FORMAT_ENDMASK);
	if (sfinfo.channels < 1 || sfinfo.channels > 2) {
		DBG("Ivona: ERROR: channels = %d.\n", sfinfo.channels);
		result = FALSE;
		goto cleanup1;
	}
	if (sfinfo.frames > 0x7FFFFFFF) {
		DBG("Ivona: ERROR: Unknown number of frames.");
		result = FALSE;
		goto cleanup1;
	}
	if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE) {
		/* Set scaling for float to integer conversion. */
		sf_command (sf, SFC_SET_SCALE_FLOAT_INT_READ, NULL, SF_TRUE);
	}
	AudioTrack track;
	track.num_samples = sfinfo.frames;
	track.num_channels = sfinfo.channels;
	track.sample_rate = sfinfo.samplerate;
	track.bits = 16;
	track.samples = malloc(items * sizeof(short));
	if (NULL == track.samples) {
		DBG("Ivona: ERROR: Cannot allocate audio buffer.");
		result = FALSE;
		goto cleanup1;
	}
	readcount = sf_read_short(sf, (short *) track.samples, items);
	DBG("Ivona: read %Ld items from audio file.", (long long) readcount);

	if (readcount > 0) {
		track.num_samples = readcount / sfinfo.channels;
		DBG("Ivona: Sending %i samples to audio.", track.num_samples);
		/* Volume is controlled by the synthesizer.  Always play at normal on audio device. */
		//spd_audio_set_volume(module_audio_id, IvonaSoundIconVolume);
		int ret = spd_audio_play(module_audio_id, track, SPD_AUDIO_LE);
		if (ret < 0) {
			DBG("ERROR: Can't play track for unknown reason.");
			result = FALSE;
			goto cleanup2;
		}
		DBG("Ivona: Sent to audio.");
	}
 cleanup2:
	xfree(track.samples);
 cleanup1:
	sf_close(sf);
#endif
	return result;
}


void play_icon(char *name)
{
	int len = strlen(IvonaSoundIconPath) + strlen(name) + 2;
	char *buf = g_malloc(len);
	sprintf(buf, "%s/%s", IvonaSoundIconPath, name);
	ivona_play_file(buf);
	g_free(buf);
}

#define IVONA_CACHE_SIZE 256
#define IVONA_CACHE_MAX_SAMPLES 65536
#define IVONA_CACHE_MAX_STRLEN 11

static int ivona_cache_count;

static struct ivona_cache {
	struct ivona_cache *succ,*pred;
	int count;
	char str[16];
	int samples;
	char *wave;
} ica_head,ica_tail,icas[IVONA_CACHE_SIZE];

static void ivona_init_cache(void)
{
	ica_head.pred=&ica_tail;
	ica_tail.succ=&ica_head;
}

void ica_tohead(struct ivona_cache *ica)
{
	if (ica->pred) ica->pred->succ=ica->succ;
	if (ica->succ) ica->succ->pred=ica->pred;
	ica->pred=ica_head.pred;
	ica->pred->succ=ica;
	ica->succ=&ica_head;
	ica_head.pred=ica;
}

static struct ivona_cache *find_min_count(void)
{
	int cnt=0x7fffffff;
	struct ivona_cache *ica,*found;
	found=NULL;
	int i;
	for (ica=ica_tail.succ,i=0;i<IVONA_CACHE_SIZE/2 && ica->samples;ica=ica->succ) {
		if (ica->count < cnt) {
			cnt=ica->count;
			found=ica;
		}
	}
	if (found) {
		for (ica=ica_tail.succ;ica->samples;ica=ica->succ) {
			if (ica->count > 1) ica->count--;
		}
	}
	return found;
}

void ivona_store_wave_in_cache(char *str,char *wave,int samples)
{
	
	struct ivona_cache *ica;
	if (strlen(str)>IVONA_CACHE_MAX_STRLEN) return;
	if (ivona_cache_count < IVONA_CACHE_SIZE) {
		ica=&icas[ivona_cache_count++];
	}
	else {
		ica=find_min_count();
		if (!ica) return;
		xfree(ica->wave);
	}
	ica->count=1;
	ica->wave=xmalloc(samples*2);
	memcpy(ica->wave,wave,samples*2);
	ica->samples=samples;
	strcpy(ica->str,str);
	ica_tohead(ica);
	DBG("Stored cache %s",str);
}

static char *ivona_get_wave_from_cache(char *to_say,int *samples)
{
	struct ivona_cache *ica;
	if (strlen(to_say)>IVONA_CACHE_MAX_STRLEN) return NULL;
	for (ica=ica_tail.succ;ica && ica->samples;ica=ica->succ) {
		DBG("Cache cmp '%s'='%s'",ica->str,to_say);
		if (!strcmp(ica->str,to_say)) {
			char *wave=xmalloc(ica->samples*2);
			memcpy(wave,ica->wave,ica->samples*2);
			*samples=ica->samples;
			ica->count++;
			ica_tohead(ica);
			return wave;
		}
	}
	return NULL;
}
