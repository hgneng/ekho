/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __transposeVisitor__
#define __transposeVisitor__

#include "typedefs.h"
#include "visitor.h"

namespace MusicXML2 
{

/*!
\addtogroup visitors
@{
*/

/*!
\brief a musicxml transpose element visitor
*/
class EXP transposevisitor : 
	public visitor<S_transpose>,
	public visitor<S_diatonic>,
	public visitor<S_chromatic>,
	public visitor<S_octave_change>,
	public visitor<S_double>
{
	protected:
		virtual void visitStart( S_transpose& elt );
		virtual void visitStart( S_diatonic& elt );
		virtual void visitStart( S_chromatic& elt );
		virtual void visitStart( S_octave_change& elt );
		virtual void visitStart( S_double& elt );

	public:
				 transposevisitor() { reset(); }
		virtual ~transposevisitor() {}
		virtual void reset();

		int		fDiatonic;
		int		fChromatic;
		int		fOctaveChange;
		bool	fDouble;
};


/*! @} */

}

#endif
