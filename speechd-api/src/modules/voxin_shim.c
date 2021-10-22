/*
 * voxin_shim.c - Shim for Voxin (version >= 3)
 *
 * Copyright (C) 2019 Samuel Thibault <samuel.thibault@ens-lyon.org>
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

#include "voxin.h"
#include <assert.h>

ECIHand ECIFNDECLARE eciNew(void) {
	assert(0);
}

int ECIFNDECLARE eciGetAvailableLanguages(enum ECILanguageDialect *aLanguages, int *nLanguages) {
	assert(0);
}

ECIHand ECIFNDECLARE eciDelete(ECIHand hEngine) {
	assert(0);
}

void ECIFNDECLARE eciVersion(char *pBuffer) {
	assert(0);
}

void ECIFNDECLARE eciErrorMessage(ECIHand hEngine, void* buffer) {
	assert(0);
}

int ECIFNDECLARE eciGetParam(ECIHand hEngine, enum ECIParam Param) {
	assert(0);
}

int ECIFNDECLARE eciSetParam(ECIHand hEngine, enum ECIParam Param, int iValue) {
	assert(0);
}

Boolean ECIFNDECLARE eciGetVoiceName(ECIHand hEngine, int iVoice, void *pBuffer) {
	assert(0);
}

Boolean ECIFNDECLARE eciCopyVoice(ECIHand hEngine, int iVoiceFrom, int iVoiceTo) {
	assert(0);
}

int ECIFNDECLARE eciGetVoiceParam(ECIHand hEngine, int iVoice, enum ECIVoiceParam Param) {
	assert(0);
}

int ECIFNDECLARE eciSetVoiceParam(ECIHand hEngine, int iVoice, enum ECIVoiceParam Param, int iValue) {
	assert(0);
}

Boolean ECIFNDECLARE eciAddText(ECIHand hEngine, ECIInputText pText) {
	assert(0);
}

Boolean ECIFNDECLARE eciInsertIndex(ECIHand hEngine, int iIndex) {
	assert(0);
}

Boolean ECIFNDECLARE eciSynthesize(ECIHand hEngine) {
	assert(0);
}

Boolean ECIFNDECLARE eciStop(ECIHand hEngine) {
	assert(0);
}

Boolean ECIFNDECLARE eciSynchronize(ECIHand hEngine) {
	assert(0);
}

Boolean ECIFNDECLARE eciSetOutputBuffer(ECIHand hEngine, int iSize, short *psBuffer) {
	assert(0);
}

void ECIFNDECLARE eciRegisterCallback(ECIHand hEngine, ECICallback Callback, void *pData) {
	assert(0);
}

ECIDictHand ECIFNDECLARE eciNewDict(ECIHand hEngine) {
	assert(0);
}

ECIDictHand ECIFNDECLARE eciGetDict(ECIHand hEngine) {
	assert(0);
}

enum ECIDictError ECIFNDECLARE eciSetDict(ECIHand hEngine, ECIDictHand hDict) {
	assert(0);
}

ECIDictHand ECIFNDECLARE eciDeleteDict(ECIHand hEngine, ECIDictHand hDict) {
	assert(0);
}

enum ECIDictError ECIFNDECLARE eciLoadDict(ECIHand hEngine, ECIDictHand hDict, enum ECIDictVolume DictVol, ECIInputText pFilename) {
	assert(0);
}

int voxGetVoices(vox_t *list, unsigned int *nbVoices) {
	assert(0);
}

int voxSetParam(void *handle, voxParam param, int value) {
	assert(0);
}

int voxToString(vox_t *data, char *string, size_t *size) {
	assert(0);
}

int voxString(vox_t *v, char *s, size_t len) {
	assert(0);
}
