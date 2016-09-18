/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __xml_tree_browser__
#define __xml_tree_browser__

#include "tree_browser.h"
#include "xml.h"

namespace MusicXML2 
{

/*!
\addtogroup MusicXML
@{
*/

//______________________________________________________________________________
class EXP xml_tree_browser : public tree_browser<xmlelement> 
{
	public:
				 xml_tree_browser(basevisitor* v) : tree_browser<xmlelement>(v) {}
		virtual ~xml_tree_browser() {}
		virtual void browse (xmlelement& t);
};

/*! @} */


}

#endif
