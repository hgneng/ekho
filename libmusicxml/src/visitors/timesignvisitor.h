/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __timesignVisitor__
#define __timesignVisitor__

#include <vector>
#include <utility>
#include "typedefs.h"
#include "rational.h"
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
class EXP timesignvisitor : 
	public visitor<S_time>,
	public visitor<S_beats>,
	public visitor<S_beat_type>,
	public visitor<S_senza_misura>
{
	protected:
		std::string fCurrentBeat;

				void reset();
		virtual void visitStart( S_time& elt );
		virtual void visitStart( S_beats& elt );
		virtual void visitStart( S_beat_type& elt );
		virtual void visitStart( S_senza_misura& elt );

	public:
		enum { kNoStaffNumber = -1 };

				 timesignvisitor() { reset(); }
		virtual ~timesignvisitor() {}

		/*!	gives the time sign as a rational
			\param index an index into fTimeSign vector
			\return the time signature as a rational or rational(0,1)
		*/
		rational timesign(unsigned int index);
		
		std::vector<std::pair<std::string,std::string> > fTimeSign;
		std::string fSymbol;
		int		fStaffNumber;
		bool	fSenzaMisura;
};


/*! @} */

}

#endif
