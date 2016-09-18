/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __xmlreader__
#define __xmlreader__

#include <stack>
#include <stdio.h>
#include "exports.h"
#include "xmlfile.h"
#include "reader.h"

namespace MusicXML2 
{

//______________________________________________________________________________
class EXP xmlreader : public reader
{ 
	std::stack<Sxmlelement>	fStack;
	SXMLFile				fFile;

	public:
				 xmlreader() {}
		virtual ~xmlreader() {}
		
		SXMLFile readbuff(const char* file);
		SXMLFile read(const char* file);
		SXMLFile read(FILE* file);

		bool	xmlDecl (const char* version, const char *encoding, int standalone);
		bool	docType (const char* start, bool status, const char *pub, const char *sys);

		void	newComment (const char* comment);
		void	newProcessingInstruction (const char* pi);
		bool	newElement (const char* eltName);
		bool	newAttribute (const char* eltName, const char *val);
		void	setValue (const char* value);
		bool	endElement (const char* eltName);
		void	error (const char* s, int lineno);
};

}

#endif
