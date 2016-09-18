/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __visitor__
#define __visitor__

#include "basevisitor.h"

namespace MusicXML2 
{

/*!
\addtogroup visitors
@{
*/

template<class C> class visitor : virtual public basevisitor
{
    public:
		virtual ~visitor() {}
		virtual void visitStart( C& elt ) {};
		virtual void visitEnd  ( C& elt ) {};
};

/*! @} */

} // namespace MusicXML2


#endif
