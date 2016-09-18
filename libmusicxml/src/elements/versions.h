/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __versions__
#define __versions__

#include "exports.h"

namespace MusicXML2 
{

/*!
\addtogroup MusicXML
@{
*/


/*!
\brief Library and components version information.
*/
//______________________________________________________________________________
class EXP versions {
    public:
		static int			libVersion();
		static const char*	libVersionStr();

		static int			xml2guidoVersion();
		static const char*	xml2guidoVersionStr();

};

}

#endif
