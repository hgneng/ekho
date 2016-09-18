/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __tree_browser__
#define __tree_browser__

#include "exports.h"
#include "basevisitor.h"
#include "browser.h"
#include "ctree.h"

namespace MusicXML2 
{

//______________________________________________________________________________
template <typename T> class EXP tree_browser : public browser<T> 
{
	protected:
		basevisitor*	fVisitor;

		virtual void enter (T& t)		{ t.acceptIn(*fVisitor); }
		virtual void leave (T& t)		{ t.acceptOut(*fVisitor); }

	public:
		typedef typename ctree<T>::treePtr treePtr;
		
				 tree_browser(basevisitor* v) : fVisitor(v) {}
		virtual ~tree_browser() {}

		virtual void set (basevisitor* v)	{  fVisitor = v; }
		virtual void browse (T& t) {
			enter(t);
			typename ctree<T>::literator iter;
			for (iter = t.lbegin(); iter != t.lend(); iter++)
				browse(**iter);
			leave(t);
		}
};

}

#endif
