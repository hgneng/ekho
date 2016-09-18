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

#include <iostream>
#include "clonevisitor.h"

using namespace std;

namespace MusicXML2
{

//______________________________________________________________________________
void clonevisitor::copyAttributes (const Sxmlelement& src, Sxmlelement& dst )
{
	vector<Sxmlattribute> attr = src->attributes();
	vector<Sxmlattribute>::const_iterator iter;
	for (iter=attr.begin(); iter != attr.end(); iter++) {
		Sxmlattribute attrcopy = xmlattribute::create();
		attrcopy->setName( (*iter)->getName());
		attrcopy->setValue( (*iter)->getValue());
		dst->add( attrcopy );
	}
}

//______________________________________________________________________________
Sxmlelement clonevisitor::copy (const Sxmlelement& src)
{
	Sxmlelement copy = xmlelement::create();
	if (copy) {
		copy->setName( src->getName());
		copy->setValue( src->getValue());
		copyAttributes (src, copy);
	}
	return copy;
}

//______________________________________________________________________________
void clonevisitor::visitStart ( Sxmlelement& elt )
{
	if (!fClone) return;
	Sxmlelement copy = xmlelement::create();
	copy->setName( elt->getName());
	copy->setValue( elt->getValue());
	copyAttributes (elt, copy);
	fLastCopy = copy;
	if (fStack.empty())
		fStack.push(copy);
	else fStack.top()->push(copy);
	if (!elt->empty()) fStack.push(copy);
}

//______________________________________________________________________________
void clonevisitor::visitEnd ( Sxmlelement& elt )
{
	if (!fClone) return;
	if (!elt->empty()) fStack.pop();
}

}
