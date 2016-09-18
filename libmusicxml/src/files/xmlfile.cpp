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
#include "xmlfile.h"
#include "xmlvisitor.h"
#include "tree_browser.h"

using namespace std; 
using namespace MusicXML2; 
namespace MusicXML2 
{

//______________________________________________________________________________
SXMLFile TXMLFile::create ()  { TXMLFile* o = new TXMLFile; assert(o!=0); return o; }

//______________________________________________________________________________
TDocType::TDocType (const string start) : fStartElement(start), fPublic(true) 
{
	fPubLitteral = "-//Recordare//DTD MusicXML 2.0 Partwise//EN";
	if (start == "score-partwise") {
		fSysLitteral = "http://www.musicxml.org/dtds/partwise.dtd";
	}
	else if (start == "score-timewise") {
		fSysLitteral = "http://www.musicxml.org/dtds/timewise.dtd";
	}
}

//______________________________________________________________________________
void TDocType::print (ostream& s) 
{
	s	<< endl << "<!DOCTYPE " << fStartElement
		<< (fPublic ? " PUBLIC " : " SYSTEM ") 
		<< "\"" << fPubLitteral << "\"\n\t\t\t\"" 
		<< fSysLitteral << "\">";	
}

//______________________________________________________________________________
void TXMLDecl::print (ostream& s) 
{
	s	<< "<?xml version=\"" << fVersion << "\"";
	if (!fEncoding.empty())			s << " encoding=\"" << fEncoding  << "\"";
	if (fStandalone != kundefined)	s << " standalone=\"" << (fStandalone ? "yes" : "no") << "\"";
	s << "?>";
}

//______________________________________________________________________________
void TXMLFile::print (ostream& stream) 
{
	if (fXMLDecl) fXMLDecl->print(stream);
	if (fDocType) fDocType->print(stream);
	xmlvisitor v(stream);
	tree_browser<xmlelement> browser(&v);
	browser.browse(*elements());
}

}
