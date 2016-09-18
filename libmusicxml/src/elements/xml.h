/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __xml__
#define __xml__

#include <string>
#include <vector>

#include "exports.h"
#include "ctree.h"
#include "smartpointer.h"

namespace MusicXML2
{

/*!
\addtogroup MusicXML
@{
*/

class xmlelement;
class xmlattribute;

typedef SMARTP<xmlattribute> 	Sxmlattribute;
typedef SMARTP<xmlelement> 		Sxmlelement;

/*!
\brief A generic xml attribute representation.

	An attribute is represented by its name and its value.
*/
//______________________________________________________________________________
class EXP xmlattribute : public smartable {
	//! the attribute name
	std::string	fName;
	//! the attribute value
	std::string 	fValue;
    protected:
		xmlattribute() {}
		virtual ~xmlattribute() {}
    public:
		static SMARTP<xmlattribute> create();

		void setName (const std::string& name);
		void setValue (const std::string& value);
		void setValue (long value);
		void setValue (int value);
		void setValue (float value);

		const std::string& getName () const		{ return fName; }
		//! returns the attribute value as a string
		const std::string& getValue () const	{ return fValue; }
		//! returns the attribute value as a int
		operator int () const;
		//! returns the attribute value as a long
		operator long () const;
		//! returns the attribute value as a float
		operator float () const;
};


/*!
\brief A generic xml element representation.

	An element is represented by its name, its value,
	the lists of its attributes and its enclosed elements.
	Attributes and elements must be added in the required
	order.
*/
//______________________________________________________________________________
class EXP xmlelement : public ctree<xmlelement>, public visitable
{
	//! the element name
	std::string	fName;
	//! the element value
	std::string fValue;
	//! list of the element attributes
	std::vector<Sxmlattribute> fAttributes;

    protected:
		//! the element type
		int	fType;

				 xmlelement() : fType(0) {}
		virtual ~xmlelement() {}

	public:
 		typedef ctree<xmlelement>::iterator			iterator;

		static SMARTP<xmlelement> create();

		virtual void acceptIn(basevisitor& visitor);
		virtual void acceptOut(basevisitor& visitor);

		void setValue (unsigned long value);
		void setValue (long value);
		void setValue (int value);
		void setValue (float value);
		void setValue (const std::string& value);
		void setName (const std::string& name);

		int					getType () const		{ return fType; }
		const std::string&	getName () const		{ return fName; }

		//! returns the element value as a string
		const std::string& getValue () const		{ return fValue; }
		//! returns the element value as a long
		operator long () const;
		//! returns the element value as a int
		operator int () const;
		//! returns the element value as a float
		operator float () const;
		//! elements comparison
		bool operator ==(const xmlelement& elt) const;
		bool operator !=(const xmlelement& elt) const	{ return !(*this == elt); }

		//! adds an attribute to the element
		long add (const Sxmlattribute& attr);

		// getting information about attributes
        const std::vector<Sxmlattribute>& attributes() const { return fAttributes; }
        const Sxmlattribute	getAttribute			(const std::string& attrname) const;
		const std::string	getAttributeValue		(const std::string& attrname) const;
		long				getAttributeLongValue	(const std::string& attrname, long defaultvalue) const;
		int					getAttributeIntValue	(const std::string& attrname, int defaultvalue) const;
		float				getAttributeFloatValue	(const std::string& attrname, float defaultvalue) const;

		// finding sub elements by type
		ctree<xmlelement>::iterator			find(int type);
		ctree<xmlelement>::iterator			find(int type, ctree<xmlelement>::iterator start);

		// getting sub elements values
		const std::string	getValue		(int subElementType);
		int					getIntValue		(int subElementType, int defaultvalue);
		long				getLongValue	(int subElementType, long defaultvalue);
		float				getFloatValue	(int subElementType, float defaultvalue);

		// misc
		bool empty () const	{ return fValue.empty() && elements().empty(); }
};

/*! @} */

}

#endif
