/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "clefvisitor.h"

using namespace std;

namespace MusicXML2 {

//________________________________________________________________________
// transpose element suppport
//________________________________________________________________________
void clefvisitor::reset ()
{
	fLine = kStandardLine;
	fOctaveChange = 0;
	fNumber = kNoNumber;
	fSign = "";
}

void clefvisitor::visitStart ( S_clef& elt )
{ 
	reset(); 
	fNumber = elt->getAttributeIntValue("number", kNoNumber); 
}

void clefvisitor::visitStart ( S_clef_octave_change& elt )	{ fOctaveChange = (int)(*elt); }
void clefvisitor::visitStart ( S_line& elt )		{ fLine = (int)(*elt); }
void clefvisitor::visitStart ( S_sign& elt )		{ fSign = elt->getValue(); }

}
