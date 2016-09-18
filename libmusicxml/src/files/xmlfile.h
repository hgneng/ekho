/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __xmlfile__
#define __xmlfile__

#include <ostream>
#include <string>
#include "exports.h"
#include "xml.h"
#include "ctree.h"

namespace MusicXML2 
{

//______________________________________________________________________________
class EXP TXMLDecl {
	std::string fVersion;
	std::string fEncoding;
	int			fStandalone;

	public:
		enum { kundefined=-1, kNo, kYes };
				 TXMLDecl (const std::string version, const std::string encoding, int stdalone=kundefined) 
						: fVersion(version), fEncoding(encoding), fStandalone(stdalone) {}
		virtual ~TXMLDecl() {}
		void print (std::ostream& s);
};

//______________________________________________________________________________
class EXP TDocType {
	std::string fStartElement;
	bool		fPublic;
	std::string fPubLitteral;
	std::string fSysLitteral;

	public:
				 TDocType (const std::string start);
				 TDocType (const std::string start, bool pub, const std::string publit, const std::string syslit) 
						: fStartElement(start), fPublic(pub), fPubLitteral(publit), fSysLitteral(syslit) {}
		virtual ~TDocType() {}
		void print (std::ostream& s);
};

//______________________________________________________________________________
class EXP TXMLFile : public smartable
{ 
	TXMLDecl *			fXMLDecl;
	TDocType *			fDocType;
	Sxmlelement			fXMLTree;
    protected:
				 TXMLFile() : fXMLDecl(0), fDocType(0) {}
		virtual ~TXMLFile() { delete fXMLDecl; delete fDocType; }
	public:
		static SMARTP<TXMLFile> create();

	public:		
		Sxmlelement	elements()				{ return fXMLTree; }
		void		set (Sxmlelement root)	{ fXMLTree = root; }
		void		set (TXMLDecl * dec)	{ fXMLDecl = dec; }
		void		set (TDocType * dt)		{ fDocType = dt; }
		void		print (std::ostream& s);
};
typedef SMARTP<TXMLFile> SXMLFile;


}

#endif
