/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifdef VC6
# pragma warning (disable : 4786)
#endif

#include <stdlib.h>
#include <math.h>
#include <iostream>

#ifndef max
static inline long max(long v1, long v2) {return (v1 < v2) ? v2 : v1;}
#endif

#include "midicontextvisitor.h"
#include "xml.h"

#define DEBUGMCV

using namespace std;

namespace MusicXML2 {

//________________________________________________________________________
// score-instrument element suppport
//________________________________________________________________________
void scoreInstrument::reset ()
{
	fID = fName = fAbbreviation = "";
}

//________________________________________________________________________
void scoreInstrument::visitStart ( S_score_instrument& elt )
{
	reset();
	fID = elt->getAttributeValue("id");
}

void scoreInstrument::visitStart ( S_instrument_name& elt )			{ fName = elt->getValue(); }
void scoreInstrument::visitStart ( S_instrument_abbreviation& elt )	{ fAbbreviation = elt->getValue(); }


//________________________________________________________________________
// midi-instrument element suppport
//________________________________________________________________________
void midiInstrument::reset ()
{
	fChan = fBank = fProgram = fVolume = fUnpitched = -1;
	fID = fMIDIName = "";
}

//________________________________________________________________________
void midiInstrument::visitStart ( S_midi_instrument& elt )
{
	reset();
	fID = elt->getAttributeValue("id");
}

void midiInstrument::visitStart ( S_midi_channel& elt )		{ fChan = (int)(*elt); }
void midiInstrument::visitStart ( S_midi_name& elt )		{ fMIDIName = elt->getValue(); }
void midiInstrument::visitStart ( S_midi_bank& elt )		{ fBank = (int)(*elt); }
void midiInstrument::visitStart ( S_midi_program& elt )		{ fProgram = (int)(*elt); }
void midiInstrument::visitStart ( S_midi_unpitched& elt )	{ fUnpitched = (int)(*elt); }
void midiInstrument::visitStart ( S_volume& elt )			{ fVolume = (int)(*elt); }


//________________________________________________________________________
// midicontextvisitor
//________________________________________________________________________
midicontextvisitor::~midicontextvisitor() {}

//________________________________________________________________________
midicontextvisitor::midicontextvisitor(long tpq, midiwriter* writer)
{
	fTPQ = tpq;
	fMidiWriter = writer;
	fInBackup = fInForward = false;
    fEndMeasureDate = 0;
	fEndPartDate = 0;

    fCurrentDate = 0;
    fTranspose = 0;
	fCurrentDynamics = 90;
	fCurrentChan = 0;

    fDivisions = 1; // to be checked
}

//________________________________________________________________________
void midicontextvisitor::addDuration(long dur)
{
    // duration is expressed using the current division
	fLastPosition = fCurrentDate;
	fCurrentDate += dur;
    fEndMeasureDate = max(fEndMeasureDate,fCurrentDate);
}

//________________________________________________________________________
// the various play methods
//________________________________________________________________________
void midicontextvisitor::playPedalChange (midiwriter::pedalType type, const std::string& val)
{
	if (fMidiWriter) {
		int midival;
		if (val == "yes") midival = 127;
		else if (val == "no") midival = 0;
		else midival = atoi(val.c_str());
		fMidiWriter->pedalChange (fCurrentDate, type, midival);

	}
}

//________________________________________________________________________
void midicontextvisitor::playTempoChange (long bpm)
{
	if (fMidiWriter && bpm)
		fMidiWriter->tempoChange (fCurrentDate, bpm);
}

//________________________________________________________________________
void midicontextvisitor::playScoreInstrument (const scoreInstrument& instr)
{
	if (fMidiWriter) {
		if (fMidiInstruments.count(instr.fID)) {
			midiInstrument mi = fMidiInstruments[instr.fID];
			fMidiWriter->newInstrument(instr.fName, mi.fChan);
			playMidiInstrument (mi);
			fCurrentChan = mi.fChan;
		}
		else fMidiWriter->newInstrument(instr.fName);
	}
}

//________________________________________________________________________
void midicontextvisitor::playNote (const notevisitor& note)
{
	notevisitor::type t = note.getType();
	if (t == notevisitor::kUndefinedType) {
		cerr << "midicontextvisitor: unexpected kUndefinedType for note " << note << endl;
		return;
	}
	if (note.isCue()) return;		// cue notes are ignored

	long dur = convert2Tick(note.getDuration());

	if (fMidiWriter && (t != notevisitor::kRest)) {
		int chan = fCurrentChan;
		float pitch = note.getMidiPitch() + fTranspose;
		if (pitch >= 0) pitch += fTranspose;

		// check for instrument specification
		string instr = note.getInstrument();
		if (!instr.empty()) {
			if (fMidiInstruments.count(instr)) {
				midiInstrument mi = fMidiInstruments[instr];
				// to retrieve the midi channel
				chan =  mi.fChan;				// get MIDI channel
				if ((mi.fUnpitched >= 0) || (t == notevisitor::kUnpitched))
					pitch = (float)mi.fUnpitched;		// and optionaly get unpitched data
			}
		}
		if (pitch < 0) pitch = 60;

		int vel = note.getDynamics();
		if (vel == notevisitor::kUndefinedDynamics) vel = fCurrentDynamics;

		int tie = note.getTie();
		long date = note.inChord() ? fLastPosition : fCurrentDate;
		if (note.isGrace()) {		// grace notes
			dur = fTPQ / 6;			// have no duration - set to an arbitrary value
			date -= dur;			// and play in advance
			if (date < 0) date = 0; // check for negative dates
			fMidiWriter->newNote(date, chan, note.getMidiPitch(), vel, dur);
		}
		else if (tie == StartStop::undefined) {
			fMidiWriter->newNote(date, chan, note.getMidiPitch(), vel, dur);
		}
		else if (tie & StartStop::start) {
			fPendingDuration += dur;
			return;
		}
		else if (tie == StartStop::stop) {
			dur += fPendingDuration;
			fMidiWriter->newNote(date, chan, note.getMidiPitch(), vel, dur);
			fPendingDuration = 0;
		}
	}
	// finally adjust the current date
	if (note.isCue()) return;		// cue don't modify the current date
	if (note.isGrace()) return;		// cue don't modify the current date
	if (note.inChord()) return;		// we're in a chord and time has already advanced
	addDuration (dur);
}

//________________________________________________________________________
void midicontextvisitor::playMidiInstrument (const midiInstrument& instr)
{
	if (fMidiWriter) {
		if (instr.fBank >= 0)
			fMidiWriter->bankChange (fCurrentDate, instr.fChan, instr.fBank);
		if (instr.fProgram >= 0)
			fMidiWriter->progChange (fCurrentDate, instr.fChan, instr.fProgram);
		if (instr.fVolume >= 0)
			fMidiWriter->volChange (fCurrentDate, instr.fChan, instr.fVolume);
	}
}

//________________________________________________________________________
// the visitxxx methods
//________________________________________________________________________
void midicontextvisitor::visitStart( S_duration& elt )
{
    long dur = convert2Tick(long(*elt));
	if (fInBackup)
		addDuration( -dur);
	else if (fInForward)
		addDuration( dur );
	else notevisitor::visitStart( elt );
}

//________________________________________________________________________
void midicontextvisitor::visitStart ( S_midi_device& elt )
{
	// not yet supported
}

//________________________________________________________________________
void midicontextvisitor::visitEnd ( S_midi_instrument& elt )
{
	if (!midiInstrument::fID.empty()) {
		fMidiInstruments[midiInstrument::fID] = (*this);
		if (fInSound)
			playMidiInstrument (*this);
	}
	else cerr << "midicontextvisitor: unexpected empty id while visiting S_midi_instrument" << endl;
}

//________________________________________________________________________
void midicontextvisitor::visitStart ( S_divisions& elt )	{ fDivisions = (long)(*elt); }
void midicontextvisitor::visitEnd ( S_measure& elt )		{ fCurrentDate = fEndMeasureDate; }
void midicontextvisitor::visitEnd ( S_transpose& elt )		{ fTranspose = fChromatic + (fOctaveChange * 12); }

//________________________________________________________________________
void midicontextvisitor::visitStart ( S_part& elt )
{
    fCurrentDate = fLastPosition = fPendingDuration = 0;
    fEndMeasureDate = fEndPartDate = 0;
    fTranspose = 0;
    fDivisions = 1;

	fCurrentPartID = elt->getAttributeValue("id");
	int instrCount = fScoreInstruments.count(fCurrentPartID);
	if (fMidiWriter) {
		fMidiWriter->startPart(instrCount);
		multimap<string, scoreInstrument>::iterator start = fScoreInstruments.lower_bound(fCurrentPartID);
		multimap<string, scoreInstrument>::iterator end = fScoreInstruments.upper_bound(fCurrentPartID);
		while (start != end) {
			playScoreInstrument(start->second);
			start++;
		}
	}
}

//________________________________________________________________________
void midicontextvisitor::visitEnd ( S_part& elt )		{ if (fMidiWriter) fMidiWriter->endPart (fCurrentDate); }
void midicontextvisitor::visitEnd ( S_note& elt )		{ playNote (*this); }
void midicontextvisitor::visitStart( S_score_part& elt )	{ fCurrentPartID = elt->getAttributeValue("id"); }
void midicontextvisitor::visitEnd  ( S_score_part& elt )	{ fCurrentPartID = ""; }

//________________________________________________________________________
void midicontextvisitor::visitEnd ( S_score_instrument& elt )
{
	// score-instruments appears in the header section, in the score-part element
	// the corresponding data is stored for future use, actually at the beginning of a part
	if (!fCurrentPartID.empty())
		fScoreInstruments.insert(pair<string, scoreInstrument>(fCurrentPartID, *this));
	else cerr << "midicontextvisitor: unexpected empty part id while visiting S_score_instrument" << endl;
}

//________________________________________________________________________
void midicontextvisitor::visitStart ( S_sound& elt )
{
	fInSound = true;
	Sxmlattribute attr = elt->getAttribute("dynamics");
	if (attr) fCurrentDynamics = convert2Vel(long(*attr));

	attr = elt->getAttribute("tempo");
	if (attr) playTempoChange( long(*attr) );

	attr = elt->getAttribute("damper-pedal");
	if (attr) playPedalChange( midiwriter::kSostenutoPedal, attr->getValue() );

	attr = elt->getAttribute("soft-pedal");
	if (attr) playPedalChange( midiwriter::kSostenutoPedal, attr->getValue() );

	attr = elt->getAttribute("sostenuto-pedal");
	if (attr) playPedalChange( midiwriter::kSostenutoPedal, attr->getValue() );

	attr = elt->getAttribute("division");
	if (attr) fDivisions = (long)(*attr);
}

}

