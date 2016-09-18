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
#include "xmlreader.h"
#include "factory.h"

using namespace std;

namespace MusicXML2
{

extern "C" {
bool readfile   (const char * file, reader * r);
bool readstream (FILE * file, reader * r);
bool readbuffer (const char * buffer, reader * r);
}

#if 0
#define debug(str,val)	cout << str << " - " << val << endl
#else
#define debug(str,val)
#endif

//_______________________________________________________________________________
SXMLFile xmlreader::readbuff(const char* buffer)
{
	fFile = TXMLFile::create();
	debug("read buffer", '-');
	return readbuffer (buffer, this) ? fFile : 0;
}

//_______________________________________________________________________________
SXMLFile xmlreader::read(const char* file)
{
	fFile = TXMLFile::create();
	debug("read", file);
	return readfile (file, this) ? fFile : 0;
}

//_______________________________________________________________________________
SXMLFile xmlreader::read(FILE* file)
{
	fFile = TXMLFile::create();
	return readstream (file, this) ? fFile : 0;
}

//_______________________________________________________________________________
void xmlreader::newComment (const char* comment)
{
	Sxmlelement elt = factory::instance().create("comment");
	elt->setValue(comment);
	fStack.top()->push(elt);
}

//_______________________________________________________________________________
void xmlreader::newProcessingInstruction (const char* pi)
{
	Sxmlelement elt = factory::instance().create("pi");
	elt->setValue(pi);
	fStack.top()->push(elt);
}

//_______________________________________________________________________________
bool xmlreader::newElement (const char* eltName)
{
	debug("newElement", eltName);
	Sxmlelement elt = factory::instance().create(eltName);
	if (!elt) return false;
	if (!fFile->elements()) {
	debug("first element", eltName);
		fFile->set(elt);
	}
	else {
	debug("push element", eltName);
		fStack.top()->push(elt);
	}
	fStack.push(elt);
	return true;
}

//_______________________________________________________________________________
bool xmlreader::endElement (const char* eltName)
{
	debug("endElement", eltName);
	Sxmlelement top = fStack.top();
	fStack.pop();
	return top->getName() == eltName;
}

//_______________________________________________________________________________
bool xmlreader::newAttribute (const char* name, const char *value)
{
	debug("newAttribute", name);
	Sxmlattribute attr = xmlattribute::create();
	if (attr) {
		attr->setName(name);
		attr->setValue(value);
		fStack.top()->add(attr);
		return true;
	}
	return false;
}

//_______________________________________________________________________________
bool xmlreader::xmlDecl (const char* version, const char *encoding, int standalone)
{
	debug("xmlDecl",version);
	TXMLDecl * dec = new TXMLDecl(version, encoding, standalone);
	fFile->set (dec);
	return dec != 0;
}

//_______________________________________________________________________________
bool xmlreader::docType (const char* start, bool status, const char *pub, const char *sys)
{
	debug("docType",start);
	TDocType * dt = new TDocType(start, status, pub, sys);
	fFile->set (dt);
	return dt != 0;
}

//_______________________________________________________________________________
void xmlreader::setValue (const char* value)
{
	debug("setValue", value);
	fStack.top()->setValue(value);
}

//_______________________________________________________________________________
void xmlreader::error (const char* s, int lineno)
{
	cerr << s  << " on line " << lineno << endl;
}

}

