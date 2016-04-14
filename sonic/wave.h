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

/* Support for reading and writing wave files. */

typedef struct waveFileStruct *waveFile;

waveFile openInputWaveFile(char *fileName, int *sampleRate, int *numChannels);
waveFile openOutputWaveFile(char *fileName, int sampleRate, int numChannels);
void closeWaveFile(waveFile file);
int readFromWaveFile(waveFile file, short *buffer, int maxSamples);
int writeToWaveFile(waveFile file, short *buffer, int numSamples);
