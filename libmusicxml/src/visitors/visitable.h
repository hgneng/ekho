/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __visitable__
#define __visitable__

#include "basevisitor.h"

namespace MusicXML2 
{

/*!
 * \brief base class for visitable objects
 */
class visitable
{
	public:
		virtual ~visitable() {}
		virtual void acceptIn(basevisitor& visitor)  {}
		virtual void acceptOut(basevisitor& visitor) {}
};

} // namespace MusicXML2


#endif
