
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <glib.h>
#include <libdumbtts.h>

#include "module_utils.h"
#include "ivona_client.h"

static struct sockaddr_in sinadr;
char *ivona_get_wave_from_cache(char *to_say, int *nsamples);
void ivona_store_wave_in_cache(char *to_say, char *wave, int nsamples);

int ivona_init_sock(char *host, int port)
{
	if (!inet_aton(host, &sinadr.sin_addr)) {
		struct hostent *h = gethostbyname(host);
		if (!h)
			return -1;
		memcpy(&sinadr.sin_addr, h->h_addr, sizeof(struct in_addr));
		endhostent();
	}
	sinadr.sin_family = AF_INET;
	sinadr.sin_port = htons(port);
	return 0;
}

#define BASE_WAVE_SIZE 65536
#define STEP_WAVE_SIZE 32768

int ivona_send_string(char *to_say)
{
	int fd;

	fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0)
		return -1;
	if (connect(fd, (struct sockaddr *)&sinadr, sizeof(sinadr)) < 0) {
		close(fd);
		return -1;
	}
	write(fd, to_say, strlen(to_say));
	write(fd, "\n", 1);
	return fd;

}

char *ivona_get_wave_fd(int fd, int *nsamples, int *offset)
{
	int got, i;
	char *ivona_wave;
	int wave_size;
	int wave_length;
	short *w;

	wave_size = BASE_WAVE_SIZE;
	wave_length = 0;
	ivona_wave = g_malloc(wave_size);
	for (;;) {
		if (wave_size < wave_length + 8192) {
			ivona_wave =
			    g_realloc(ivona_wave, wave_size + STEP_WAVE_SIZE);
			wave_size += STEP_WAVE_SIZE;
		}
		DBG("Have place for %d bytes", wave_size - wave_length);
		got =
		    read(fd, ivona_wave + wave_length, wave_size - wave_length);
		DBG("Wave part at %d size %d", wave_length, got);
		if (got <= 0)
			break;
		wave_length += got;
	}
	close(fd);
	w = (short *)ivona_wave;
	for (i = wave_length / 2 - 1; i >= 0; i--)
		if (w[i])
			break;
	if (i < 100) {
		g_free(ivona_wave);
		return NULL;
	}
	DBG("Trimmed %d samples at end", wave_length / 2 - i - 1);
	*nsamples = i + 1;
	for (i = 0; i < *nsamples; i++)
		if (w[i])
			break;
	DBG("Should trim %d bytes at start", i);
	*offset = i;
	(*nsamples) -= i;
	return ivona_wave;
}

/*
static char *ivona_get_wave_from_cache(char *to_say,int *nsamples);
void ivona_store_wave_in_cache(char *to_say,char *wave,int nsamples);
*/
char *ivona_get_wave(char *to_say, int *nsamples, int *offset)
{
	int fd;
	char *s;
	s = ivona_get_wave_from_cache(to_say, nsamples);
	if (s) {
		*offset = 0;
		return s;
	}
	fd = ivona_send_string(to_say);
	if (fd < 0)
		return NULL;
	s = ivona_get_wave_fd(fd, nsamples, offset);
	if (s)
		ivona_store_wave_in_cache(to_say, s + 2 * (*offset), *nsamples);
	return s;
}

/* Plays the specified audio file - from ibmtts/espeak module */

void play_icon(char *path, char *name)
{
	char *buf = g_strdup_printf("%s/%s", path, name);
	module_play_file(buf);
	g_free(buf);
}

#define IVONA_CACHE_SIZE 256
#define IVONA_CACHE_MAX_SAMPLES 65536

static int ivona_cache_count;

static struct ivona_cache {
	struct ivona_cache *succ, *pred;
	int count;
	char str[16];
	int samples;
	char *wave;
} ica_head, ica_tail, icas[IVONA_CACHE_SIZE];

void ivona_init_cache(void)
{
	ica_head.pred = &ica_tail;
	ica_tail.succ = &ica_head;
}

void ica_tohead(struct ivona_cache *ica)
{
	if (ica->pred)
		ica->pred->succ = ica->succ;
	if (ica->succ)
		ica->succ->pred = ica->pred;
	ica->pred = ica_head.pred;
	ica->pred->succ = ica;
	ica->succ = &ica_head;
	ica_head.pred = ica;
}

static struct ivona_cache *find_min_count(void)
{
	int cnt = 0x7fffffff;
	struct ivona_cache *ica, *found;
	found = NULL;
	int i;
	for (ica = ica_tail.succ, i = 0;
	     i < IVONA_CACHE_SIZE / 2 && ica->samples; ica = ica->succ) {
		if (ica->count < cnt) {
			cnt = ica->count;
			found = ica;
		}
	}
	if (found) {
		for (ica = ica_tail.succ; ica->samples; ica = ica->succ) {
			if (ica->count > 1)
				ica->count--;
		}
	}
	return found;
}

void ivona_store_wave_in_cache(char *str, char *wave, int samples)
{

	struct ivona_cache *ica;
	if (strlen(str) > IVONA_CACHE_MAX_STRLEN)
		return;
	if (ivona_cache_count < IVONA_CACHE_SIZE) {
		ica = &icas[ivona_cache_count++];
	} else {
		ica = find_min_count();
		if (!ica)
			return;
		g_free(ica->wave);
	}
	ica->count = 1;
	ica->wave = g_malloc(samples * 2);
	memcpy(ica->wave, wave, samples * 2);
	ica->samples = samples;
	strcpy(ica->str, str);
	ica_tohead(ica);
	DBG("Stored cache %s", str);
}

char *ivona_get_wave_from_cache(char *to_say, int *samples)
{
	struct ivona_cache *ica;
	if (strlen(to_say) > IVONA_CACHE_MAX_STRLEN)
		return NULL;
	for (ica = ica_tail.succ; ica && ica->samples; ica = ica->succ) {
		DBG("Cache cmp '%s'='%s'", ica->str, to_say);
		if (!strcmp(ica->str, to_say)) {
			char *wave = g_malloc(ica->samples * 2);
			memcpy(wave, ica->wave, ica->samples * 2);
			*samples = ica->samples;
			ica->count++;
			ica_tohead(ica);
			return wave;
		}
	}
	return NULL;
}
