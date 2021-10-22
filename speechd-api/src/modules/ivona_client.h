/*
 * ivona_client.h - Declarations from ivona_client.c
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
#ifndef IVONA_CLIENT_H
#define IVONA_CLIENT_H

/* A constant, used in both ivona.c and ivona_client.c */
#define IVONA_CACHE_MAX_STRLEN 11

int ivona_init_sock(const char *host, int port);
int ivona_send_string(const char *to_say);
char *ivona_get_wave_fd(int fd, int *nsamples, int *offset);
char *ivona_get_wave(const char *to_say, int *nsamples, int *offset);
void play_icon(const char *path, const char *name);
void ivona_init_cache(void);
void ivona_store_wave_in_cache(const char *to_say, const char *wave, int nsamples);
char *ivona_get_wave_from_cache(const char *to_say, int *nsamples);
#endif
