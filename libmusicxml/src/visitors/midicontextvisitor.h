/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __midiContextVisitor__
#define __midiContextVisitor__

#include <string>
#include <map>
#include "xml.h"
#include "typedefs.h"
#include "visitor.h"
#include "notevisitor.h"
#include "transposevisitor.h"

namespace MusicXML2 
{

/*!
\addtogroup visitors
@{
*/

/*!
\brief score-instrument data

	midi-instrument elements may appear in score-part elements (header) and in sound elements
*/
class EXP scoreInstrument : 
	public visitor<S_score_instrument>,
	public visitor<S_instrument_name>,
	public visitor<S_instrument_abbreviation>
{
	protected:
		virtual void reset();
		virtual void visitStart( S_score_instrument& elt );
		virtual void visitStart( S_instrument_name& elt );
		virtual void visitStart( S_instrument_abbreviation& elt );

	public:
				 scoreInstrument() {}
		virtual ~scoreInstrument() {}

		std::string fName;			///< the score instrument name
		std::string fAbbreviation;	///< the instrument abbreviated name
		std::string fID;			///< the score-instrument id
};

/*!
\brief midi-instrument data

	midi-instrument elements may appear in score-part elements (header) and in sound elements
*/
class EXP midiInstrument : 
	public visitor<S_midi_instrument>,
	public visitor<S_midi_channel>,
	public visitor<S_midi_name>,
	public visitor<S_midi_bank>,
	public visitor<S_midi_program>,
	public visitor<S_midi_unpitched>,
	public visitor<S_volume>
{
	protected:
		virtual void reset();
		virtual void visitStart( S_midi_instrument& elt );
		virtual void visitStart( S_midi_channel& elt );
		virtual void visitStart( S_midi_name& elt );
		virtual void visitStart( S_midi_bank& elt );
		virtual void visitStart( S_midi_program& elt );
		virtual void visitStart( S_volume& elt );
		virtual void visitStart( S_midi_unpitched& elt );

	public:
				 midiInstrument() : fChan(-1), fBank(-1), fProgram(-1), fVolume(-1), fUnpitched(-1) {}
		virtual ~midiInstrument() {}

		int fChan;			///< an optional MIDI channel  (-1 when undefined)
		int fBank;			///< an optional MIDI bank number  (-1 when undefined)
		int fProgram;		///< a MIDI program change (-1 when undefined)
		int fVolume;		///< a percentage from 0 to 100  (-1 when undefined)
		int fUnpitched;		///< a note number from 1 to 127 (-1 when undefined)
		std::string fMIDIName;	///< MIDI names correspond to ProgramName meta-events within a Standard MIDI File
		std::string fID;	///< the midi-instrument id
};
 

/*!
\brief An abstract interface to be used with midicontextvisitor
*/
class midiwriter {
	public:
		enum pedalType { kDamperPedal, kSoftpedal, kSostenutoPedal };

		virtual ~midiwriter() {}
		
		virtual void startPart (int instrCount) = 0;
		virtual void newInstrument (std::string instrName, int chan=-1) = 0;
		virtual void endPart (long date) = 0;

		virtual void newNote (long date, int chan, float pitch, int velocity, int duration) = 0;
		virtual void tempoChange (long date, int bpm) = 0;
		virtual void pedalChange (long date, pedalType t, int value) = 0;

		virtual void volChange (long date, int chan, int vol) = 0;
		virtual void bankChange (long date, int chan, int bank) = 0;
		virtual void progChange (long date, int chan, int prog) = 0;
};


/*!
\brief A Visitor that maintains a context for MIDI generation.
\todo Handling of grace notes. Handling of divisions attribute in coda or segno sound attribute. 
Handling of actual duration in fine sound element.
*/

class EXP midicontextvisitor : 
	public notevisitor,
	public transposevisitor,
	public midiInstrument,
	public scoreInstrument,
	public visitor<S_divisions>,
	public visitor<S_backup>,
	public visitor<S_forward>,
	public visitor<S_midi_device>,
	public visitor<S_measure>,
	public visitor<S_part>,
	public visitor<S_score_part>,
	public visitor<S_sound>
{
    private:
		typedef std::multimap<std::string, scoreInstrument> instrumentsList;
		typedef std::map<std::string, midiInstrument>  midi_instrumentsList;
        instrumentsList		 fScoreInstruments;
		midi_instrumentsList fMidiInstruments;
		std::string			 fCurrentPartID;

        bool	fInBackup;      // In backup state
        bool	fInForward;     // In forward state
        bool	fInSound;       // In sound element
        long 	fEndMeasureDate;	// max of the date kept in a measure
		long	fEndPartDate;		// max of the part duration
   
    protected:
		midiwriter*	fMidiWriter;
 		long		fDivisions;			// current division
        long		fCurrentDate;		// current date
        long		fLastPosition;		// last time position (used for chord)
		long		fPendingDuration;	// pending duration (used for tied notes)
        long		fCurrentDynamics;	// current dynamics ie MIDI velocity
        long		fTranspose;			// current transpose value
		long		fTPQ;				// ticks-per-quater value for date conversion
        long		fCurrentChan;		// current midi channel
		
        void addDuration(long dur);		// move the current time, dur must be expressed in tpq
  		long convert2Tick(long val) { return (val*fTPQ)/fDivisions; }
  		long convert2Vel(long val)	{ return  (val * 90) / 100; }

 		virtual void playScoreInstrument (const scoreInstrument& instr);
 		virtual void playMidiInstrument (const midiInstrument& instr);
 		virtual void playNote (const notevisitor& note);
 		virtual void playTempoChange (long bmp);
 		virtual void playPedalChange (midiwriter::pedalType type, const std::string& val);

		virtual void visitStart( S_divisions& elt );
		virtual void visitStart( S_duration& elt );
		virtual void visitStart( S_backup& elt )	{ fInBackup = true; }
		virtual void visitEnd  ( S_backup& elt )	{ fInBackup = false; }
		virtual void visitStart( S_forward& elt )	{ fInForward = true; }
		virtual void visitEnd  ( S_forward& elt )	{ fInForward = false; }
		virtual void visitEnd  ( S_score_instrument& elt );
		virtual void visitStart( S_midi_device& elt );
		virtual void visitEnd  ( S_midi_instrument& elt );
		virtual void visitEnd  ( S_measure& elt );
		virtual void visitStart( S_part& elt );
		virtual void visitEnd  ( S_part& elt );
		virtual void visitEnd  ( S_note& elt );
		virtual void visitStart( S_score_part& elt );
		virtual void visitEnd  ( S_score_part& elt );
		virtual void visitStart( S_sound& elt );
		virtual void visitEnd  ( S_sound& elt )		{ fInSound = false; }
		virtual void visitEnd  ( S_transpose& elt );

    public:    
				 midicontextvisitor(long tpq, midiwriter* writer=0);
       	virtual ~midicontextvisitor();
};

/*! @} */

}

#endif
