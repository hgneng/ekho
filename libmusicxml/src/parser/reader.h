/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __reader__
#define __reader__

class reader
{ 
	public:
				 reader() {}
		virtual ~reader() {};
		
		virtual bool	xmlDecl (const char* version, const char *encoding, int standalone) = 0;
		virtual bool	docType (const char* start, bool status, const char *pub, const char *sys) = 0;

		virtual void	newComment (const char* comment) {}
		virtual void	newProcessingInstruction (const char* pi) {}
		virtual bool	newElement (const char* eltName) = 0;
		virtual bool	newAttribute (const char* eltName, const char *val) = 0;
		virtual void	setValue (const char* value) = 0;
		virtual bool	endElement (const char* eltName) = 0;
		virtual void	error (const char* s, int lineno) = 0;
};


#endif
