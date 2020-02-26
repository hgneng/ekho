/*
 * baratinoo.h - Shim for Baratinoo (VoxyGen)
 * to be able to build the Baratinoo module without the Baratinoo SDK.
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

#include "baratinoo_compat.h"
#include <assert.h>

BARATINOO_INIT_RETURN BCinitlib(BaratinooTraceCB traceCB) {
	assert(0);
}

const char *BCgetBaratinooVersion(void) {
	assert(0);
}

const const BaratinooVersionStruct *BCgetBaratinooVersionStruct(void) {
	assert(0);
}

void BCterminatelib(void) {
	assert(0);
}


BCengine BCnew(const void *privatedata) {
	assert(0);
}

void BCdelete(BCengine engine) {
	assert(0);
}


void BCinit(BCengine engine, const char *config) {
	assert(0);
}

int BCgetNumberOfVoices(BCengine engine) {
	assert(0);
}

BaratinooVoiceInfo BCgetVoiceInfo(BCengine engine, int voiceNumber) {
	assert(0);
}

BARATINOOC_STATE BCgetState(BCengine engine) {
	assert(0);
}

void BCsetWantedEvent(BCengine engine, BARATINOO_EVENT_TYPE type) {
	assert(0);
}

BARATINOOC_STATE BCprocessLoop(BCengine engine, int count) {
	assert(0);
}

BaratinooEvent BCgetEvent(BCengine engine) {
	assert(0);
}

BARATINOOC_STATE BCpurge(BCengine engine) {
	assert(0);
}


/* IO */

void BCsetOutputSignal(BCengine engine, BaratinooOutputSignalCB cb, void *privateData, BARATINOO_SIGNAL_CODING coding, int frequency) {
	assert(0);
}

BCoutputSignalBuffer BCoutputSignalBufferNew(BARATINOO_SIGNAL_CODING coding, int frequency) {
	assert(0);
}

void BCoutputTextBufferSetInEngine(BCoutputSignalBuffer outputSignalBuffer, BCengine engine) {
	assert(0);
}

int BCoutputSignalBufferIsError(BCoutputSignalBuffer outputSignalBuffer) {
	assert(0);
}

char *BCoutputSignalBufferGetSignalBuffer(BCoutputSignalBuffer outputSignalBuffer) {
	assert(0);
}

int BCoutputSignalBufferGetSignalLength(BCoutputSignalBuffer outputSignalBuffer) {
	assert(0);
}

void BCoutputSignalBufferResetSignal(BCoutputSignalBuffer outputSignalBuffer) {
	assert(0);
}

void BCoutputSignalBufferDeleteSignal(void *signal) {
	assert(0);
}

void BCoutputSignalBufferDelete(BCoutputSignalBuffer outputSignalBuffer) {
	assert(0);
}


BCinputTextBuffer BCinputTextBufferNew(BARATINOO_PARSING parsing, BARATINOO_TEXT_ENCODING encoding, int voiceIndex, char *voiceModules) {
	assert(0);
}

BARATINOOC_STATE BCinputTextBufferSetInEngine(BCinputTextBuffer inputTextBuffer, BCengine engine) {
	assert(0);
}

int BCinputTextBufferInit(BCinputTextBuffer inputTextBuffer, const char *text) {
	assert(0);
}

void BCinputTextBufferDelete(BCinputTextBuffer inputTextBuffer) {
	assert(0);
}
