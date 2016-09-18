/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "metronomevisitor.h"

using namespace std;

namespace MusicXML2 {

//________________________________________________________________________
void metronomevisitor::reset ()
{
	fBeats.clear();
	fPerMinute = 0;
	reset(fCurrentBeat);
}

//________________________________________________________________________
void metronomevisitor::reset (beat& b)
{
	b.fUnit = "";
	b.fDots = 0;
}

void metronomevisitor::visitStart ( S_metronome& elt )		{ reset(); }
void metronomevisitor::visitEnd ( S_metronome& elt )		
{ 
	if (fCurrentBeat.fUnit.size()) {
		fBeats.push_back(fCurrentBeat); 
		reset (fCurrentBeat);
	}
}

void metronomevisitor::visitStart ( S_beat_unit& elt )		
{ 
	if (fCurrentBeat.fUnit.size()) {
		fBeats.push_back(fCurrentBeat); 
		reset (fCurrentBeat);
	}
	fCurrentBeat.fUnit = elt->getValue();
}

void metronomevisitor::visitStart ( S_beat_unit_dot& elt )	{ fCurrentBeat.fDots++; }
void metronomevisitor::visitStart ( S_per_minute& elt )		{ fPerMinute = (int)(*elt); }

}
