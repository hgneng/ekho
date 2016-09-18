/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include <iostream>
#include "xmlvisitor.h"

using namespace std;

namespace MusicXML2 
{

//______________________________________________________________________________
ostream& operator<< (ostream& os, const xmlendl& endl)
{
    endl.print(os);
    return os;
}

//______________________________________________________________________________
void xmlendl::print(std::ostream& os) const { 
	int i = fIndent;
    os << std::endl;
    while (i-- > 0)  os << "    ";
}

//______________________________________________________________________________
void xmlvisitor::visitStart ( S_comment& elt ) 
{
	fOut <<  fendl << "<!--" << elt->getValue() << "-->";
}

//______________________________________________________________________________
void xmlvisitor::visitStart ( S_processing_instruction& elt ) 
{
	fOut <<  fendl << "<?" << elt->getValue() << "?>";
}

//______________________________________________________________________________
void xmlvisitor::visitStart ( Sxmlelement& elt ) 
{
	fOut <<  fendl << "<" << elt->getName();
	// print the element attributes first
	vector<Sxmlattribute>::const_iterator attr; 
	for (attr = elt->attributes().begin(); attr != elt->attributes().end(); attr++)
		fOut << " " << (*attr)->getName() << "=\"" << (*attr)->getValue() << "\"";				
	if (elt->empty()) {
		fOut << "/>";	// element is empty, we can direclty close it
	}
	else {
		fOut << ">";
		if (!elt->getValue().empty())
			fOut << elt->getValue();
		if (elt->size())
			fendl++;
	}
}

//______________________________________________________________________________
void xmlvisitor::visitEnd ( Sxmlelement& elt ) 
{
	if (!elt->empty()) {
		if (elt->size()) {
			fendl--;
			cout << fendl;
		}
		fOut << "</" << elt->getName() << ">";
	}
}

}
