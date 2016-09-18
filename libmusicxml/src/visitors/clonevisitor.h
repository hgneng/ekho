/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __cloneVisitor__
#define __cloneVisitor__

#include <stack>
#include "visitor.h"
#include "xml.h"

namespace MusicXML2 
{   

/*!
\addtogroup visitors
@{
*/

/*!
\brief A visitor that clones a musicxml tree
*/
class EXP clonevisitor : 
	public visitor<Sxmlelement>
{
    public:
				 clonevisitor() : fClone(true) {}
       	virtual ~clonevisitor() {}
              
		virtual void visitStart( Sxmlelement& elt );
		virtual void visitEnd  ( Sxmlelement& elt );
		
		virtual Sxmlelement clone()	{ return fStack.top(); }

	protected:
		virtual void			clone(bool state)	{ fClone = state; }
		virtual void			copyAttributes (const Sxmlelement& src, Sxmlelement& dst);
		virtual Sxmlelement		copy (const Sxmlelement& elt);
		virtual Sxmlelement&	lastCopy ()	{ return fLastCopy; }

		bool					fClone;
		Sxmlelement				fLastCopy;
		std::stack<Sxmlelement> fStack;
};

/*! @} */

}

#endif
