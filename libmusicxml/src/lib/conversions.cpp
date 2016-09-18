/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "conversions.h"

using namespace std;
using namespace MusicXML2;

namespace MusicXML2 
{

//--------------------------------------------------------------------------------
// static declarations, used to provide conversion between strings and constants
//--------------------------------------------------------------------------------
TrillStart::type TrillStart::fSNTbl[]		= { upper, main, below };
string TrillStart::fSNStrings[]				= { "upper", "main", "below" };
bimap<string, TrillStart::type> TrillStart::fSN2String(fSNStrings, fSNTbl, last);

const string TrillStart::xml (type d)					{ return fSN2String[d]; }
TrillStart::type TrillStart::xml (const string str) 	{ return fSN2String[str]; }

//--------------------------------------------------------------------------------
TrillStep::type TrillStep::fTSTbl[]		= { whole, half, unison, none };
string TrillStep::fTSStrings[]			= { "whole", "half", "unison", "none" };
bimap<string, TrillStep::type> TrillStep::fTS2String(fTSStrings, fTSTbl, last);

const string TrillStep::xml (type d)				{ return fTS2String[d]; }
TrillStep::type TrillStep::xml (const string str) 	{ return fTS2String[str]; }

//--------------------------------------------------------------------------------
FullCue::type FullCue::fFCTbl[]			= { full, cue };
string FullCue::fFCStrings[]			= { "full", "cue" };
bimap<string, FullCue::type> FullCue::fFC2String(fFCStrings, fFCTbl, last);

const string FullCue::xml (type d) 				{ return fFC2String[d]; }
FullCue::type FullCue::xml (const string str) 	{ return fFC2String[str]; }

//--------------------------------------------------------------------------------
YesNo::type YesNo::fYNTbl[]				= { yes, no };
string YesNo::fYNStrings[]				= { "yes", "no" };
bimap<string, YesNo::type> YesNo::fYN2String(fYNStrings, fYNTbl, last);

const string YesNo::xml (type d) 			{ return fYN2String[d]; }
YesNo::type YesNo::xml (const string str) 	{ return fYN2String[str]; }

//--------------------------------------------------------------------------------
StartStop::type StartStop::fStartStopTbl[]	= { start, stop, cont };
string StartStop::fStartStopStrings[]		= { "start", "stop", "continue" };
bimap<string, StartStop::type> StartStop::fStartStop2String(fStartStopStrings, fStartStopTbl, last);

const string StartStop::xml (type d) 				{ return fStartStop2String[d]; }
StartStop::type StartStop::xml (const string str) 	{ return fStartStop2String[str]; }

//--------------------------------------------------------------------------------
LineType::type LineType::fLineTypeTbl[]		= { solid, dashed, dotted, wavy };
string LineType::fLineTypeStrings[]			= { "solid", "dashed", "dotted", "wavy" };
bimap<string, LineType::type> LineType::fLineType2String(fLineTypeStrings, fLineTypeTbl, last);

const string LineType::xml (type d) 			{ return fLineType2String[d]; }
LineType::type LineType::xml (const string str) { return fLineType2String[str]; }

//--------------------------------------------------------------------------------
NoteType::type NoteType::fTypeTbl[]	= { t256th, t128th, t64th, t32nd, t16th, eighth, 
                                        quarter, half, whole, breve, tlong };
string NoteType::fTypeStrings[]		= { "256th", "128th", "64th", "32nd", "16th", "eighth", 
                                        "quarter", "half", "whole", "breve", "longa" };
bimap<string, NoteType::type> NoteType::fType2String(fTypeStrings, fTypeTbl, count);

const string NoteType::xml (type d)  				{ return fType2String[d]; }
NoteType::type NoteType::xml (const string str)  	{ return fType2String[str]; }
rational NoteType::type2rational(type d) {
	rational r, ratio(1,256);
	int i = 1;
	while (i <= whole) {
		if (d & i) {
			r = ratio;
			break;
		}
		i <<= 1;
		ratio *= 2;
	}
	return r;
}

}


