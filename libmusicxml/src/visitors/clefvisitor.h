/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __clefVisitor__
#define __clefVisitor__

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
class EXP clefvisitor : 
	public visitor<S_clef>,
	public visitor<S_sign>,
	public visitor<S_line>,
	public visitor<S_clef_octave_change>
{
	protected:
			    void reset();
		virtual void visitStart ( S_clef& elt );
		virtual void visitStart ( S_sign& elt );
		virtual void visitStart ( S_line& elt );
		virtual void visitStart ( S_clef_octave_change& elt );

	public:
		enum { kStandardLine, kTrebleStdLine=2, kBassStdLine=4, kCStdLine=3, kTabStdLine=5 };
		enum { kNoNumber = -1 };
		
				 clefvisitor() { reset(); }
		virtual ~clefvisitor() {}

		std::string	fSign;
		int			fLine;
		int			fOctaveChange;
		int			fNumber;
};


/*! @} */

}

#endif
