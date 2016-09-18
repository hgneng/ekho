/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __types__
#define __types__

#include "exports.h"
#include "xml.h"
#include "visitor.h"

namespace MusicXML2 
{

/*!
\addtogroup MusicXML
@{
*/

template <int elt> class musicxml : public xmlelement
{ 
    protected:	
		musicxml() { fType = elt; }

	public:
		static SMARTP<musicxml<elt> > new_musicxml()	
			{ musicxml<elt>* o = new musicxml<elt>; assert(o!=0); return o; }
		static SMARTP<musicxml<elt> > new_musicxml(const std::vector<Sxmlelement>& elts)	
			{ musicxml<elt>* o = new musicxml<elt>(elts); assert(o!=0); return o; }

        virtual void acceptIn(basevisitor& v) {
			if (visitor<SMARTP<musicxml<elt> > >* p = dynamic_cast<visitor<SMARTP<musicxml<elt> > >*>(&v)) {
				SMARTP<musicxml<elt> > sptr = this;
				p->visitStart(sptr);
			}
			else xmlelement::acceptIn(v);
		}

        virtual void acceptOut(basevisitor& v) {
			if (visitor<SMARTP<musicxml<elt> > >* p = dynamic_cast<visitor<SMARTP<musicxml<elt> > >*>(&v)) {
				SMARTP<musicxml<elt> > sptr = this;
				p->visitEnd(sptr);
			}
			else xmlelement::acceptOut(v);
		}
};

/*! @} */

}

#endif
