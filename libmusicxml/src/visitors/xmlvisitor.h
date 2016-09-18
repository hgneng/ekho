/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __xmlvisitors__
#define __xmlvisitors__

#include <ostream>

#include "tree_browser.h"
#include "typedefs.h"
#include "visitor.h"
#include "xml.h"

namespace MusicXML2 
{

//______________________________________________________________________________
/*!
\internal
\brief to be used in place of std::endl
	to provide a correct indentation of the xml output.
*/
class xmlendl {
	private:
		int fIndent;
	public:
				 xmlendl() : fIndent(0) {}
		virtual ~xmlendl() {}

		//! increase the indentation
		xmlendl& operator++ (int)  { fIndent++; return *this; }
		//! decrease the indentation
		xmlendl& operator-- (int)  { fIndent--; return *this; }
		//! reset the indentation to none
		void print(std::ostream& os) const;
};
std::ostream& operator<< (std::ostream& os, const xmlendl& eol);

//______________________________________________________________________________
class xmlvisitor : 
	public visitor<S_comment>,
	public visitor<S_processing_instruction>,
	public visitor<Sxmlelement>
{
	std::ostream&	fOut;
	xmlendl			fendl;

    public:
				 xmlvisitor(std::ostream& stream) : fOut(stream) {}
		virtual ~xmlvisitor() {}

		virtual void visitStart ( Sxmlelement& elt);
		virtual void visitEnd   ( Sxmlelement& elt);
		virtual void visitStart ( S_comment& elt);
		virtual void visitStart ( S_processing_instruction& elt);
};

} // namespace MusicXML2


#endif
