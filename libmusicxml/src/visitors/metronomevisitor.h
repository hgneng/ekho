/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __metronomevisitor__
#define __metronomevisitor__

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
class EXP metronomevisitor : 
	public visitor<S_metronome>,
	public visitor<S_beat_unit>,
	public visitor<S_beat_unit_dot>,
	public visitor<S_per_minute>
{
	protected:
		typedef struct { std::string fUnit; int fDots; } beat;
		beat fCurrentBeat;

				void reset();
		virtual void reset(beat& b);
		virtual void visitStart( S_metronome& elt );
		virtual void visitEnd  ( S_metronome& elt );
		virtual void visitStart( S_beat_unit& elt );
		virtual void visitStart( S_beat_unit_dot& elt );
		virtual void visitStart( S_per_minute& elt );

	public:
				 metronomevisitor() { reset(); }
		virtual ~metronomevisitor() {}

		std::vector<beat> fBeats;
		int		fPerMinute;
};


/*! @} */

}

#endif
