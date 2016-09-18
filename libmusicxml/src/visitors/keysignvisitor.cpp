/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "keysignvisitor.h"

using namespace std;

namespace MusicXML2 {

//________________________________________________________________________
// transpose element suppport
//________________________________________________________________________
void keysignvisitor::reset ()
{
	fFifths = fCancel = 0;
	fMode = "";
}

void keysignvisitor::visitStart ( S_key& elt )		{ reset(); }
void keysignvisitor::visitStart ( S_fifths& elt )	{ fFifths = (int)(*elt); }
void keysignvisitor::visitStart ( S_cancel& elt )	{ fCancel = (int)(*elt); }
void keysignvisitor::visitStart ( S_mode& elt )		{ fMode = elt->getValue(); }

}
