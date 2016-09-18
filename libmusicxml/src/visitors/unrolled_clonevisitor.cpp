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

#include <vector>
#include "unrolled_clonevisitor.h"

using namespace std;
namespace MusicXML2
{

//______________________________________________________________________________
void unrolled_clonevisitor::visitStart( S_measure& elt)
{
	start(elt);
	Sxmlattribute attr = lastCopy()->getAttribute("number");
	if (attr) {
		attr->setValue(fMeasureNum++);
	}
}

//______________________________________________________________________________
void unrolled_clonevisitor::visitStart( S_part& elt)
{
	fMeasureNum = 1;
	start(elt);
}

//______________________________________________________________________________
void unrolled_clonevisitor::visitStart( S_key& elt)
{
	if (elt != fCurrentKey) {
		fCurrentKey = elt;
		fKeyCopy = true;
		Sxmlelement xml = elt;
		start (xml);
	}
	else {
		fKeyCopy = false;
		clone(false);
	}
}

//______________________________________________________________________________
void unrolled_clonevisitor::visitStart( S_clef& elt)
{
	if (elt != fCurrentClef) {
		fCurrentClef = elt;
		fClefCopy = true;
		Sxmlelement xml = elt;
		start (xml);
	}
	else {
		fClefCopy = false;
		clone(false);
	}
}

//______________________________________________________________________________
void unrolled_clonevisitor::visitStart( S_time& elt)
{
	if (elt != fCurrentTime) {
		fCurrentTime = elt;
		fTimeCopy = true;
		Sxmlelement xml = elt;
		start (xml);
	}
	else {
		fTimeCopy = false;
		clone(false);
	}
}

//______________________________________________________________________________
void unrolled_clonevisitor::visitEnd  ( S_key& elt)		{ if (fKeyCopy) end( elt ); else clone(true); }
void unrolled_clonevisitor::visitEnd  ( S_clef& elt)	{ if (fClefCopy) end( elt ); else clone(true); }
void unrolled_clonevisitor::visitEnd  ( S_time& elt)	{ if (fTimeCopy) end( elt ); else clone(true); }

//______________________________________________________________________________
void unrolled_clonevisitor::visitStart( S_sound& elt)
{
	start(elt);
	vector<Sxmlattribute> attr = lastCopy()->attributes();
	vector<Sxmlattribute>::iterator iter;
	for (iter=attr.begin(); iter != attr.end();) {
		const string name = (*iter)->getName();
		if ((name == "segno" ) ||
			(name == "forward-repeat" ) ||
			(name == "coda" ) ||
			(name == "fine" ) ||
			(name == "dacapo" ) ||
			(name == "dalsegno" ) ||
			(name == "tocoda" )) {
			iter = attr.erase(iter);
		}
		else iter++;
	}
}

}
