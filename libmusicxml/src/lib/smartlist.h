/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __smartlist__
#define __smartlist__

#include <vector>
#include "smartpointer.h"

namespace MusicXML2 
{

/*
	smartlist are provided only to exchange container (vector, list,...) with clients 
	while avoiding the runtime library issue with windows visual c++
*/
template <typename T, typename L=std::vector<T> >
class smartlist  : public smartable, public L {
	protected:
				 smartlist() {}
		virtual ~smartlist() {}
	public:
		typedef SMARTP<smartlist<T> >	ptr;
		static ptr create() { smartlist<T> * o = new smartlist<T>; assert(o!=0); return o; }
};

}

#endif
