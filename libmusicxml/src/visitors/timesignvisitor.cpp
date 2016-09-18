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
#include "timesignvisitor.h"

using namespace std;

namespace MusicXML2 {

//________________________________________________________________________
// transpose element suppport
//________________________________________________________________________
void timesignvisitor::reset ()
{
	fSenzaMisura = false;
	fCurrentBeat = "";
	fTimeSign.clear();
	fStaffNumber = kNoStaffNumber;
	fSymbol = "";
}

void timesignvisitor::visitStart ( S_time& elt ) {
	reset();
	fStaffNumber = elt->getAttributeIntValue("number", kNoStaffNumber);
	fSymbol = elt->getAttributeValue("symbol");
}

void timesignvisitor::visitStart ( S_beats& elt )			{ fCurrentBeat = elt->getValue(); }
void timesignvisitor::visitStart ( S_beat_type& elt )		{ fTimeSign.push_back(make_pair(fCurrentBeat, elt->getValue())); }
void timesignvisitor::visitStart ( S_senza_misura& elt )	{ fSenzaMisura = true; }


rational timesignvisitor::timesign(unsigned int index)
{
	rational r(0,1);
	if (index < fTimeSign.size()) {
		const pair<string,string>& ts = fTimeSign[index];
		long num = strtol (ts.first.c_str(), 0, 10);
		long denum = strtol (ts.second.c_str(), 0, 10);
		if (num && denum) r.set(num, denum);
	}
	return r;
}

}
