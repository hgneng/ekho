/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include <stdlib.h>
#include <string>
#include <sstream>
#include <iostream>

#include "xml.h"
#include "visitor.h"

using namespace std;

namespace MusicXML2 
{

//______________________________________________________________________________
// xmlattribute
//______________________________________________________________________________
Sxmlattribute xmlattribute::create() { xmlattribute * o = new xmlattribute; assert(o!=0); return o; }

//______________________________________________________________________________
void xmlattribute::setName (const string& name) 		{ fName = name; }
void xmlattribute::setValue (const string& value) 		{ fValue = value; }

//______________________________________________________________________________
void xmlattribute::setValue (long value)
{
	stringstream s;
	s << value;
	s >> fValue;
}

//______________________________________________________________________________
void xmlattribute::setValue (int value)
{
	stringstream s;
	s << value;
	s >> fValue;
}

//______________________________________________________________________________
void xmlattribute::setValue (float value)
{
	stringstream s;
	s << value;
	s >> fValue;
}

xmlattribute::operator int () const		{ return atoi(fValue.c_str()); }
xmlattribute::operator long () const	{ return atol(fValue.c_str()); }
xmlattribute::operator float () const	{ return (float)atof(fValue.c_str()); }

//______________________________________________________________________________
// xmlelement
//______________________________________________________________________________
Sxmlelement xmlelement::create()				{ xmlelement * o = new xmlelement; assert(o!=0); return o; }
void xmlelement::setValue (int value)			{ setValue((long)value); }
void xmlelement::setValue (const string& value) 	{ fValue = value; }
void xmlelement::setName (const string& name) 		{ fName = name; }
//______________________________________________________________________________
void xmlelement::setValue (long value)
{
	stringstream s;
	s << value;
	s >> fValue;
}

//______________________________________________________________________________
void xmlelement::setValue (unsigned long value)
{
	stringstream s;
	s << value;
	s >> fValue;
}

//______________________________________________________________________________
void xmlelement::setValue (float value)
{
	stringstream s;
	s << value;
	s >> fValue;
}

//______________________________________________________________________________
long xmlelement::add (const Sxmlattribute& attr)
{ 
	fAttributes.push_back(attr);
	return fAttributes.size()-1;
}

//______________________________________________________________________________
void xmlelement::acceptIn(basevisitor& v) {
	visitor<Sxmlelement>* p = dynamic_cast<visitor<Sxmlelement>*>(&v);
	if (p) {
		Sxmlelement xml = this;
		p->visitStart (xml);
	}
}

//______________________________________________________________________________
void xmlelement::acceptOut(basevisitor& v) {
	visitor<Sxmlelement>* p = dynamic_cast<visitor<Sxmlelement>*>(&v);
	if (p) {
		Sxmlelement xml = this;
		p->visitEnd (xml);
	}
}

//______________________________________________________________________________
const Sxmlattribute xmlelement::getAttribute(const string& attrname) const 
{
	vector<Sxmlattribute>::const_iterator it;
	for (it = attributes().begin(); it != attributes().end(); it++) {
		if ((*it)->getName() == attrname)
			return *it;
	}
	return 0;
}

//______________________________________________________________________________
const string xmlelement::getAttributeValue (const string& attrname) const
{
	Sxmlattribute attribute = getAttribute(attrname);
	return attribute ? attribute->getValue() : "";
}

//______________________________________________________________________________
long xmlelement::getAttributeLongValue (const string& attrname, long defaultvalue) const
{
	Sxmlattribute attribute = getAttribute(attrname);
	return attribute ? atol(attribute->getValue().c_str()) : defaultvalue;
}

//______________________________________________________________________________
int xmlelement::getAttributeIntValue (const string& attrname, int defaultvalue) const
{
	Sxmlattribute attribute = getAttribute(attrname);
	return attribute ? atoi(attribute->getValue().c_str()) : defaultvalue;
}

//______________________________________________________________________________
float xmlelement::getAttributeFloatValue (const string& attrname, float defaultvalue) const
{
	Sxmlattribute attribute = getAttribute(attrname);
	return attribute ? (float)atof(attribute->getValue().c_str()) : defaultvalue;
}

xmlelement::operator int () const	{ return atoi(fValue.c_str()); }
xmlelement::operator long () const	{ return atol(fValue.c_str()); }
xmlelement::operator float () const { return (float)atof(fValue.c_str()); }

//______________________________________________________________________________
bool xmlelement::operator ==(const xmlelement& elt) const
{
	if (getType() != elt.getType()) return false;
	if (getName() != elt.getName()) return false;
	if (getValue()!= elt.getValue()) return false;
	const vector<Sxmlattribute>& attr1 = attributes();
	const vector<Sxmlattribute>& attr2 = elt.attributes();
	if (attr1.size() != attr2.size()) return false;

	vector<Sxmlattribute>::const_iterator iter1 = attr1.begin();
	vector<Sxmlattribute>::const_iterator iter2 = attr2.begin();
	while (iter1 != attr1.end()) {
		if (iter2 == attr2.end()) return false;
		if ((*iter1)->getName() != (*iter2)->getName()) return false;
		if ((*iter1)->getValue() != (*iter2)->getValue()) return false;
		iter1++; iter2++;
	}
	return true;
}

//______________________________________________________________________________
ctree<xmlelement>::iterator xmlelement::find(int type)
{ 
	return find(type, begin());
}

ctree<xmlelement>::iterator xmlelement::find(int type, ctree<xmlelement>::iterator iter)
{
	while (iter != end()) {
		if ((*iter)->getType() == type) break;
		iter++;
	}
	return iter;
}

//______________________________________________________________________________
const std::string xmlelement::getValue (int subElementType)
{
	ctree<xmlelement>::iterator iter = find(subElementType);
	return (iter != end()) ? (*iter)->getValue() : "";
}

int xmlelement::getIntValue (int subElementType, int defaultvalue)
{
	ctree<xmlelement>::iterator iter = find(subElementType);
	return (iter != end()) ? int(*(*iter)) : defaultvalue;
}

long xmlelement::getLongValue (int subElementType, long defaultvalue)
{
	ctree<xmlelement>::iterator iter = find(subElementType);
	return (iter != end()) ? long(*(*iter)) : defaultvalue;
}

float xmlelement::getFloatValue	(int subElementType, float defaultvalue)
{
	ctree<xmlelement>::iterator iter = find(subElementType);
	return (iter != end()) ? float(*(*iter)) : defaultvalue;
}

}
