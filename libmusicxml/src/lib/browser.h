/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __browser__
#define __browser__

#include "ctree.h"

namespace MusicXML2 
{

//______________________________________________________________________________
template <typename T> class browser {
	public:
		virtual ~browser() {}
		virtual void browse (T& t) = 0;
};

}

#endif
