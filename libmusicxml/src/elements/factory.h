/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __factory__
#define __factory__

#include <string>
#include <map>
#include "functor.h"
#include "singleton.h"
#include "xml.h"

namespace MusicXML2 
{

/*!
\addtogroup MusicXML
@{
*/

class EXP factory : public singleton<factory>{

	std::map<std::string, functor<Sxmlelement>*> fMap;
	std::map<int, const char*>	fType2Name;
	public:
				 factory();
		virtual ~factory() {}

		Sxmlelement create(const std::string& elt) const;	
		Sxmlelement create(int type) const;	
};

}

/*! @} */

#endif
