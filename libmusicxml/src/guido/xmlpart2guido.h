/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __xmlpart2guido__
#define __xmlpart2guido__

#include <ostream>
#include <stack>
#include <map>
#include <string>

#include "clefvisitor.h"
#include "exports.h"
#include "guido.h"
#include "keysignvisitor.h"
#include "metronomevisitor.h"
#include "notevisitor.h"
#include "rational.h"
#include "timesignvisitor.h"
#include "typedefs.h"
#include "visitor.h"
#include "xml.h"

namespace MusicXML2 
{

/*!
\ingroup visitors guido
@{
*/

/*!
\brief A score visitor to produce a Guido representation.
*/
//______________________________________________________________________________
class EXP xmlpart2guido : 
	public clefvisitor,
	public timesignvisitor,
	public metronomevisitor,
	public notevisitor,
	public keysignvisitor,
	public visitor<S_backup>,
	public visitor<S_barline>,
	public visitor<S_coda>,
	public visitor<S_direction>,
	public visitor<S_divisions>,
	public visitor<S_dynamics>,
	public visitor<S_ending>,
	public visitor<S_forward>,
	public visitor<S_measure>,
	public visitor<S_octave_shift>,
	public visitor<S_part>,
	public visitor<S_repeat>,
	public visitor<S_segno>,
	public visitor<S_sound>,
	public visitor<S_wedge>
{
	// the guido elements stack
	std::stack<Sguidoelement>	fStack;
	// structure to store delayed elements ie elements enclosed in direction with offset
	typedef struct {
		long delay;
		Sguidoelement element;
	} delayedElement;
	vector<delayedElement>	fDelayed;
	// fields to controls the guido output generation
	bool	fGenerateComments, fGenerateStem, fGenerateBars, fGeneratePositions;

	// internal parsing state
	bool	fInCue, fInGrace, fInhibitNextBar, fPendingBar, fBeamOpened, fMeasureEmpty, fCrescPending;
	
	S_measure	fCurrentMeasure;  

	bool	fNotesOnly;				// a flag to generate notes only (used for several voices on the same staff)
	bool	fSkipDirection;			// a flag to skip direction elements (for notes only mode or due to different staff)
	int		fCurrentStaffIndex;		// the index of the current guido staff
	int		fCurrentStaff;			// the staff we're currently generating events for (0 by default)
	int		fTargetStaff;			// the musicxml target staff (0 by default)
	int		fTargetVoice;			// the musicxml target voice (0 by default)

	long	fCurrentDivision;		// the current measure division, expresses the time unit in division of the quarter note
	long	fCurrentOffset;			// the current direction offset: represents an element relative displacement in current division unit
	rational fCurrentMeasureLength;	// the current measure length (max of the current measure positions)
	rational fCurrentMeasurePosition;// the current position in the measure
	rational fCurrentVoicePosition;	// the current position within a voice
	rational fCurrentTimeSign;		// the current time signature
	int		fMeasNum;

	int		fCurrentBeamNumber;		// number attribute of the current beam
	int		fCurrentStemDirection;	// the current stems direction, used for stem direction changes
	int		fPendingPops;			// elements to be popped at chord exit (like fermata, articulations...)

	void start (Sguidoelement& elt)		{ fStack.push(elt); }
	void add  (Sguidoelement& elt)		{ fStack.top()->add(elt); }
	void addDelayed (Sguidoelement elt, long offset);	// adding elements to the delayed elements
	void checkDelayed (long time);						// checks the delayed elements for ready elements 
	void push (Sguidoelement& elt)		{ add(elt); fStack.push(elt); }
	void pop ()							{ fStack.pop(); }

	void moveMeasureTime (int duration, bool moveVoiceToo=false);
	void reset ();
	void stackClean	();

	int  checkArticulation ( const notevisitor& note );			// returns the count of articulations pushed on the stack
	std::vector<Sxmlelement>  getChord ( const S_note& note );	// build a chord vector
	void checkStaff		 (int staff );					// check for staff change
	void checkStem		 ( const S_stem& stem );
	void checkBeamBegin	 ( const std::vector<S_beam>& beams );
	void checkBeamEnd	 ( const std::vector<S_beam>& beams );
	void checkCue		 ( const notevisitor& nv );
	void checkGrace		 ( const notevisitor& nv );
	int  checkFermata	 ( const notevisitor& stem );
	void checkSlurBegin	 ( const std::vector<S_slur>& slurs );
	void checkSlurEnd	 ( const std::vector<S_slur>& slurs );
	void checkTiedBegin	 ( const std::vector<S_tied>& tied );
	void checkTiedEnd	 ( const std::vector<S_tied>& tied );
	void checkVoiceTime	 ( const rational& currTime, const rational& voiceTime);
	void newNote		 ( const notevisitor& nv );

	std::string			noteName		( const notevisitor& nv );
	guidonoteduration	noteDuration	( const notevisitor& nv );

	std::vector<S_beam>::const_iterator findValue ( const std::vector<S_beam>& beams, const std::string& val ) const;
	std::vector<S_slur>::const_iterator findTypeValue ( const std::vector<S_slur>& slurs, const std::string& val ) const;
	std::vector<S_tied>::const_iterator findTypeValue ( const std::vector<S_tied>& tied, const std::string& val ) const;
	
	static std::string alter2accident ( float alter );

	protected:
		enum { kStemUndefined, kStemUp, kStemDown, kStemNone };
		enum { kLeaveChord=-1, kNoChord, kEnterChord } chordState;

		virtual void visitStart( S_backup& elt);
		virtual void visitStart( S_barline& elt);
		virtual void visitStart( S_coda& elt);
		virtual void visitStart( S_direction& elt);
		virtual void visitStart( S_divisions& elt);
		virtual void visitStart( S_dynamics& elt);
		virtual void visitStart( S_forward& elt);
		virtual void visitStart( S_measure& elt);
		virtual void visitStart( S_note& elt);
		virtual void visitStart( S_octave_shift& elt);
		virtual void visitStart( S_part& elt);
		virtual void visitStart( S_segno& elt);
		virtual void visitStart( S_wedge& elt);
		virtual void visitEnd  ( S_clef& elt);
		virtual void visitEnd  ( S_direction& elt);
		virtual void visitEnd  ( S_ending& elt);
		virtual void visitEnd  ( S_key& elt);
		virtual void visitEnd  ( S_measure& elt);
		virtual void visitEnd  ( S_metronome& elt);
		virtual void visitEnd  ( S_note& elt);
		virtual void visitEnd  ( S_repeat& elt);
		virtual void visitEnd  ( S_sound& elt);
		virtual void visitEnd  ( S_time& elt);

    public:
				 xmlpart2guido(bool generateComments, bool generateStem, bool generateBar=true);
		virtual ~xmlpart2guido() {}

		Sguidoelement& current ()					{ return fStack.top(); }
		void	initialize (Sguidoelement seq, int staff, int guidostaff, int voice, bool notesonly, rational defaultTimeSign);
		void	generatePositions (bool state)		{ fGeneratePositions = state; }
		const rational& getTimeSign () const		{ return fCurrentTimeSign; }
};


} // namespace MusicXML2


#endif
