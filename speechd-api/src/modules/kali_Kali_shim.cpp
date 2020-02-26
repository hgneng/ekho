/*
 * kali_Kali_shim.c - Shim for Kali
 * to be able to build the Kali module without the Kali SDK.
 *
 * Copyright (C) 2018 Hypra
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
 */

#include <kali/Kali/kali.h>
#include <assert.h>

extern "C" {

bool initKali(void) {
	assert(0);
}

const AudioTrackKali *GetBufMultiKaliStd(short nK) {
	assert(0);
}

void SetSortieBufMultiKaliStd(short nK, bool sortieBuf) {
	assert(0);
}

void SetSortieSonMultiKaliStd(short nK, bool sortieSon) {
	assert(0);
}


short GetDebitDefautKaliStd(void) {
	assert(0);
}

short GetDebitMinKaliStd(void) {
	assert(0);
}

short GetDebitMaxKaliStd(void) {
	assert(0);
}

void SetDebitKali(short debit) {
	assert(0);
}


short GetVolumeDefautKaliStd(void) {
	assert(0);
}

short GetVolumeMinKaliStd(void) {
	assert(0);
}

short GetVolumeMaxKaliStd(void) {
	assert(0);
}

void SetVolumeKali(short volume) {
	assert(0);
}


short GetHauteurDefautKaliStd(void) {
	assert(0);
}

short GetHauteurMinKaliStd(void) {
	assert(0);
}

short GetHauteurMaxKaliStd(void) {
	assert(0);
}

void SetHauteurKali(short hauteur) {
	assert(0);
}


void SetModeLectureKali(short modeLecture) {
	assert(0);
}


short GetNLangueVoixKaliStd(short nVoix) {
	assert(0);
}

char *GetNomLangueKali(short nLangue, char *nomLangue) {
	assert(0);
}

void SetLangueKali(short nLangue) {
	assert(0);
}

short GetNbVoixKali(void) {
	assert(0);
}

char *GetNomVoixKali(short nVoix, char *nomVoix) {
	assert(0);
}

void SetVoixKali(short nVoix) {
	assert(0);
}


short MessageKali(unsigned char* texte) {
	assert(0);
}

short QueryIndexKali(void) {
	assert(0);
}

}
