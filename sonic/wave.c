/* Sonic library
   Copyright 2010
   Bill Cox
   This file is part of the Sonic Library.

   The Sonic Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

/*
This file supports read/write wave files.
*/
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <sndfile.h>
#include "wave.h"

struct waveFileStruct {
    SNDFILE *soundFile;
    int numChannels;
};

/* Open the file for reading.  Also determine it's sample rate. */
waveFile openInputWaveFile(
    char *fileName,
    int *sampleRate,
    int *numChannels)
{
    SF_INFO info;
    SNDFILE *soundFile;
    waveFile file;

    info.format = 0;
    soundFile = sf_open(fileName, SFM_READ, &info);
    if(soundFile == NULL) {
	fprintf(stderr, "Unable to open wave file %s: %s\n", fileName, sf_strerror(NULL));
	return NULL;
    }
    file = (waveFile)calloc(1, sizeof(struct waveFileStruct));
    file->soundFile = soundFile;
    file->numChannels = info.channels;
    *sampleRate = info.samplerate;
    *numChannels = info.channels;
    printf("Frames = %ld, sample rate = %d, channels = %d, format = %d\n",
        info.frames, info.samplerate, info.channels, info.format);
    return file;
}

/* Open the file for reading. */
waveFile openOutputWaveFile(
    char *fileName,
    int sampleRate,
    int numChannels)
{
    SF_INFO info;
    SNDFILE *soundFile;
    waveFile file;

    info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    info.samplerate = sampleRate;
    info.channels = numChannels;
    soundFile = sf_open(fileName, SFM_WRITE, &info);
    if(soundFile == NULL) {
	fprintf(stderr, "Unable to open wave file %s: %s\n", fileName, sf_strerror(NULL));
	return NULL;
    }
    file = (waveFile)calloc(1, sizeof(struct waveFileStruct));
    file->soundFile = soundFile;
    file->numChannels = numChannels;
    return file;
}

/* Close the sound file. */
void closeWaveFile(
    waveFile file)
{
    SNDFILE *soundFile = file->soundFile;

    sf_close(soundFile);
    free(file);
}

/* Read from the wave file. */
int readFromWaveFile(
    waveFile file,
    short *buffer,
    int maxSamples)
{
    SNDFILE *soundFile = file->soundFile;
    int samplesRead;
    int numChannels = file->numChannels;

    samplesRead = sf_read_short(soundFile, buffer, maxSamples*numChannels);
    if(samplesRead <= 0) {
	return 0;
    }
    return samplesRead/numChannels;
}

/* Write to the wave file. */
int writeToWaveFile(
    waveFile file,
    short *buffer,
    int numSamples)
{
    SNDFILE *soundFile = file->soundFile;
    int numWritten;

    numWritten = sf_write_short(soundFile, buffer, numSamples*file->numChannels);
    if(numWritten != numSamples*file->numChannels) {
	fprintf(stderr, "Unable to write wave file.\n");
	return 0;
    }
    return 1;
}
