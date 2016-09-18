/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "versions.h"

namespace MusicXML2 
{
//______________________________________________________________________________
int			versions::libVersion()				{ return 210; }
const char*	versions::libVersionStr()			{ return "2.1.0"; }

int			versions::xml2guidoVersion()		{ return 200; }
const char*	versions::xml2guidoVersionStr()		{ return "2.0.0"; }

}

