/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __keysignvisitor__
#define __keysignvisitor__

#include <vector>
#include <utility>
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
class EXP keysignvisitor : 
	public visitor<S_key>,
	public visitor<S_cancel>,
	public visitor<S_fifths>,
	public visitor<S_mode>
{
	protected:
		int fCurrentBeat;

				void reset();
		virtual void visitStart( S_key& elt );
		virtual void visitStart( S_fifths& elt );
		virtual void visitStart( S_cancel& elt );
		virtual void visitStart( S_mode& elt );

	public:
				 keysignvisitor() { reset(); }
		virtual ~keysignvisitor() {}

		std::string fMode;
		int		fFifths;
		int		fCancel;
};


/*! @} */

}

#endif
