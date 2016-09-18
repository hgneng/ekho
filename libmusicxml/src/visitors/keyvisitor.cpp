/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "keyvisitor.h"

using namespace std;

namespace MusicXML2 {


//________________________________________________________________________
ostream& operator<< (ostream& os, const keyvisitor& elt)
{
    elt.print(os);
    return os;
}

//________________________________________________________________________
// transpose element suppport
//________________________________________________________________________
void keyvisitor::reset ()
{
	fFifths = fCancel = 0;
	fMode = "";
}

void keyvisitor::visitStart ( S_key& elt )		{ reset(); }
void keyvisitor::visitStart ( S_cancel& elt )	{ fCancel = (int)(*elt); }
void keyvisitor::visitStart ( S_fifths& elt )	{ fFifths = (int)(*elt); }
void keyvisitor::visitStart ( S_mode& elt )		{ fMode = elt->getValue(); }

//________________________________________________________________________
void keyvisitor::print (ostream& out) const
{
	out << fFifths;
	if (fMode.size())	out << " mode: " << fMode;
	if (fCancel)		out << " cancel: " << fCancel;
}

}
