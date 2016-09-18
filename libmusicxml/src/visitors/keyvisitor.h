/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __keyVisitor__
#define __keyVisitor__

#include <ostream>

#include "typedefs.h"
#include "visitor.h"

namespace MusicXML2 
{

/*!
\addtogroup visitors
@{
*/

/*!
\brief key element data
*/
class EXP keyvisitor : 
	public visitor<S_key>,
	public visitor<S_cancel>,
	public visitor<S_fifths>,
	public visitor<S_mode>,
	public visitor<S_key_step>,
	public visitor<S_key_alter>,
	public visitor<S_key_octave>
{
	protected:
				void reset();
		virtual void visitStart ( S_key& elt );
		virtual void visitStart ( S_cancel& elt );
		virtual void visitStart ( S_fifths& elt );
		virtual void visitStart ( S_mode& elt );

		virtual void visitStart ( S_key_step& elt )		{}	// not yet supported
		virtual void visitStart ( S_key_alter& elt )	{}	// not yet supported
		virtual void visitStart ( S_key_octave& elt )	{}	// not yet supported

	public:
				 keyvisitor() { reset(); }
		virtual ~keyvisitor() {}

		virtual void print (std::ostream& out) const;

		int			fFifths;
		int			fCancel;
		std::string	fMode;
};

EXP std::ostream& operator<< (std::ostream& os, const keyvisitor& elt);


/*! @} */

}

#endif
