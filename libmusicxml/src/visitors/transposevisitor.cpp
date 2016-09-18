/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/


#include "transposevisitor.h"

using namespace std;

namespace MusicXML2 {

//________________________________________________________________________
// transpose element suppport
//________________________________________________________________________
void transposevisitor::reset ()
{
	fDiatonic = fChromatic = fOctaveChange = 0;
	fDouble = false;
}

void transposevisitor::visitStart ( S_transpose& elt )		{ reset(); }
void transposevisitor::visitStart ( S_diatonic& elt )		{ fDiatonic = (int)(*elt); }
void transposevisitor::visitStart ( S_chromatic& elt )		{ fChromatic = (int)(*elt); }
void transposevisitor::visitStart ( S_octave_change& elt )	{ fOctaveChange = (int)(*elt); }
void transposevisitor::visitStart ( S_double& elt )			{ fDouble = true; }

}
