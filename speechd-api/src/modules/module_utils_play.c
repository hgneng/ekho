/*
 * module_utils_play.c - Module utilities for audio
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: module_utils.c,v 1.55 2008-07-10 15:37:18 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sndfile.h>

#include "module_utils.h"

/* Plays the specified audio file. */
int module_play_file(const char *filename)
{
	int result = 0;
	int subformat;
	sf_count_t items;
	sf_count_t readcount;
	SNDFILE *sf;
	SF_INFO sfinfo;

	DBG("Playing |%s|", filename);
	memset(&sfinfo, 0, sizeof(sfinfo));
	sf = sf_open(filename, SFM_READ, &sfinfo);
	if (NULL == sf) {
		DBG("%s", sf_strerror(NULL));
		return -1;
	}
	if (sfinfo.channels < 1 || sfinfo.channels > 2) {
		DBG("ERROR: channels = %d.\n", sfinfo.channels);
		result = FALSE;
		goto cleanup1;
	}
	if (sfinfo.frames > 0x7FFFFFFF || sfinfo.frames == 0) {
		DBG("ERROR: Unknown number of frames.");
		result = FALSE;
		goto cleanup1;
	}

	subformat = sfinfo.format & SF_FORMAT_SUBMASK;
	items = sfinfo.channels * sfinfo.frames;
	DBG("Frames = %jd, channels = %ld", sfinfo.frames,
	    (long)sfinfo.channels);
	DBG("Samplerate = %i, items = %lld", sfinfo.samplerate,
	    (long long)items);
	DBG("Major format = 0x%08X, subformat = 0x%08X, endian = 0x%08X",
	    sfinfo.format & SF_FORMAT_TYPEMASK, subformat,
	    sfinfo.format & SF_FORMAT_ENDMASK);

	if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE) {
		/* Set scaling for float to integer conversion. */
		sf_command(sf, SFC_SET_SCALE_FLOAT_INT_READ, NULL, SF_TRUE);
	}
	AudioTrack track;
	track.num_samples = sfinfo.frames;
	track.num_channels = sfinfo.channels;
	track.sample_rate = sfinfo.samplerate;
	track.bits = 16;
	track.samples = g_malloc(items * sizeof(short));
	readcount = sf_read_short(sf, (short *)track.samples, items);
	DBG("Read %lld items from audio file.", (long long)readcount);

	if (readcount > 0) {
		track.num_samples = readcount / sfinfo.channels;
		DBG("Sending %i samples to audio.", track.num_samples);
		int ret = module_tts_output(track, SPD_AUDIO_LE);
		if (ret < 0) {
			DBG("ERROR: Can't play track for unknown reason.");
			result = -1;
			goto cleanup2;
		}
		DBG("Sent to audio.");
	}
cleanup2:
	g_free(track.samples);
cleanup1:
	sf_close(sf);
	return result;
}
