/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include <iostream>
#include "xml_tree_browser.h"

using namespace std;
namespace MusicXML2 
{

void xml_tree_browser::browse (xmlelement& t) {
	enter(t);
	ctree<xmlelement>::literator iter;
	for (iter = t.lbegin(); iter != t.lend(); iter++)
		browse(**iter);
	leave(t);
}


}
