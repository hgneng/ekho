/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __musicxmlfactory__
#define __musicxmlfactory__

#include <vector>
#include "xmlfile.h"
#include "elements.h"

using namespace std;
namespace MusicXML2
{

//------------------------------------------------------------------------
class musicxmlfactory 
{
	private:
		SXMLFile	fFile;
		Sxmlelement	fRoot;
		Sxmlelement	fIdentification;
		Sxmlelement	fPartList;

	protected:
		void			sort ();
		Sxmlelement		getSubElement (Sxmlelement elt, int type) const;
		Sxmlelement		getNotations (Sxmlelement elt) const		{ return getSubElement (elt, MusicXML2::k_notations); }
		Sxmlelement		getAttributes (Sxmlelement elt) const		{ return getSubElement (elt, MusicXML2::k_attributes); }
		Sxmlelement		getArticulations (Sxmlelement elt) const	{ return getSubElement (getNotations(elt), MusicXML2::k_articulations); }
//		Sxmlelement		getDynamics (Sxmlelement elt) const			{ return getSubElement (getNotations(elt), k_dynamics); }
		
	public:
				 musicxmlfactory();
		virtual	~musicxmlfactory() {}
		
		// getting the 'auto' elements (i.e. elements created by the factory constructor
		virtual Sxmlelement	root ()				{ return fRoot; }
		virtual Sxmlelement	identification ()	{ return fIdentification; }
		virtual Sxmlelement	partlist ()			{ return fPartList; }

		// managing header information
		virtual void	header	(const char* worknumber, const char* worktitle, const char* movementnumber, const char* movementtitle);
		virtual void	creator	(const char* c, const char* type=0);
		virtual void	rights	(const char* c, const char* type=0);
		virtual void	encoding(const char* software = 0);

		virtual void	addgroup (int number, const char* name, const char* abbrev, bool groupbarline, std::vector<Sxmlelement>& parts);
		virtual void	addpart (const Sxmlelement& part);


		virtual Sxmlelement	scorepart (const char* id, const char* name, const char* abbrev);
		virtual Sxmlelement	part (const char* id);
		virtual Sxmlelement	newmeasure (int number) const;
		virtual Sxmlelement	newmeasure (int number, const char* time, const char* clef, int line, int key, int division) const;
		virtual Sxmlelement	newnote (const char* step, float alter, int octave, int duration, const char* type=0);
		virtual Sxmlelement	newrest (int duration, const char* type=0);
		virtual Sxmlelement newdynamics (int type, const char* placement=0);
		virtual Sxmlelement newbarline (const char* location, const char* barstyle, const char *repeat=0);

		virtual void		makechord (const std::vector<Sxmlelement>& notes);
		virtual void		maketuplet (int actual, int normal, const std::vector<Sxmlelement>& notes);
		virtual void		tie (Sxmlelement start, Sxmlelement end);
		virtual void		addnotation (Sxmlelement elt, Sxmlelement notation);
		virtual void		addarticulation (Sxmlelement elt, Sxmlelement articulation);

		virtual void add (Sxmlelement elt, const std::vector<Sxmlelement>& subelts) const;
		virtual void add (Sxmlelement elt, const Sxmlelement& subelt) const		{ elt->push (subelt); }
		virtual void add (Sxmlelement elt, const Sxmlattribute& attr) const		{ elt->add (attr); }

		virtual Sxmlelement		element(int type, const char * value=0) const;
		virtual Sxmlelement		element(int type, int value) const;
		virtual Sxmlelement		element(int type, float value) const;
		template<typename T> Sxmlattribute	attribute(const char * name, T value) const {
													Sxmlattribute attribute = xmlattribute::create();
													attribute->setName (name);
													attribute->setValue (value);
													return attribute;
												}

		virtual void			print (std::ostream& s) 	{ sort(); fFile->print(s); }
		virtual Sxmlelement		getElements() 				{ sort(); return fRoot; }
};

}

#endif
