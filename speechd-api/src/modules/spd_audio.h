
/*
 * spd_audio.h -- The SPD Audio Library Header
 *
 * Copyright (C) 2004 Brailcom, o.p.s.
 * Copyright (C) 2019 Samuel Thibault <samuel.thibault@ens-lyon.org>
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: spd_audio.h,v 1.21 2008-10-15 17:28:17 hanke Exp $
 */

#ifndef __SPD_AUDIO_H
#define __SPD_AUDIO_H

#include <spd_audio_plugin.h>

#define SPD_AUDIO_LIB_PREFIX "spd_"

#ifdef  __cplusplus
extern "C" {
#endif

AudioID *spd_audio_open(char *name, void **pars, char **error);

int spd_audio_play(AudioID * id, AudioTrack track, AudioFormat format);

int spd_audio_begin(AudioID * id, AudioTrack track, AudioFormat format);
int spd_audio_feed_sync(AudioID * id, AudioTrack track, AudioFormat format);
int spd_audio_feed_sync_overlap(AudioID * id, AudioTrack track, AudioFormat format);
int spd_audio_end(AudioID * id);

int spd_audio_stop(AudioID * id);

int spd_audio_close(AudioID * id);

int spd_audio_set_volume(AudioID * id, int volume);

void spd_audio_set_loglevel(AudioID * id, int level);

char const *spd_audio_get_playcmd(AudioID * id);

#ifdef __cplusplus
}
#endif

#endif /* ifndef #__SPD_AUDIO_H */
