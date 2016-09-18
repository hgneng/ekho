/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include <vector>

#include "libmusicxml.h"
#include "musicxmlfactory.h"
#include "versions.h"

using namespace std;
namespace MusicXML2
{

EXP int			musicxmllibVersion()			{ return versions::libVersion(); }
EXP const char* musicxmllibVersionStr()			{ return versions::libVersionStr(); }
EXP int			musicxml2guidoVersion()			{ return versions::xml2guidoVersion(); }
EXP	const char* musicxml2guidoVersionStr()		{ return versions::xml2guidoVersionStr(); }

//------------------------------------------------------------------------
EXP TFactory factoryOpen  ()						{ return new musicxmlfactory(); }
EXP void	factoryClose (TFactory f)				{ delete f; }
EXP void	factoryPrint (TFactory f, ostream& out)	{ f->print(out); }


//------------------------------------------------------------------------
static TElement		__retainElt  (Sxmlelement elt)		{ elt->addReference(); return (TElement)elt; }
static TAttribute	__retainAttr (Sxmlattribute attr)	{ attr->addReference(); return (TAttribute)attr; }
static Sxmlelement		__releaseElt  (TElement elt)					{ Sxmlelement xml(elt); elt->removeReference(); return xml; }
static Sxmlattribute	__releaseAttr (TAttribute attr)					{ Sxmlattribute xml(attr); attr->removeReference(); return xml; }

//------------------------------------------------------------------------
// high level operations
//------------------------------------------------------------------------
EXP void	factoryHeader	(TFactory f, const char* wn, const char* wt, const char* mn, const char* mt)
	{ f->header(wn, wt, mn, mt); }
EXP void	factoryCreator	(TFactory f, const char* c, const char* type)
	{ f->creator(c, type); }
EXP void	factoryRights	(TFactory f, const char* c, const char* type)
	{ f->rights(c, type); }
EXP void	factoryEncoding (TFactory f, const char* soft)
	{ f->encoding(soft); }

EXP void factoryAddGroup (TFactory f, int number, const char* name, const char* abbrev, bool groupbarline, TElement* parts)
{
	vector<Sxmlelement> list;
	while (*parts) {
		list.push_back(__releaseElt(*parts));
		parts++;
	}
	f->addgroup (number, name, abbrev, groupbarline, list);
}

EXP void		factoryAddPart (TFactory f, TElement part)
	{ f->addpart (__releaseElt(part)); }

EXP TElement	factoryScorepart (TFactory f, const char* id, const char* name, const char* abbrev)
	{ return __retainElt (f->scorepart (id, name, abbrev)); }
EXP TElement	factoryPart (TFactory f, const char* id)
	{ return __retainElt (f->part (id)); }

EXP TElement	factoryMeasure (TFactory f, int number)
	{ return __retainElt (f->newmeasure (number)); }
EXP TElement	factoryMeasureWithAttributes (TFactory f, int number, const char* time, const char* clef, int line, int key, int division)
	{ return __retainElt (f->newmeasure (number, time, clef, line, key, division)); }

EXP TElement	factoryNote	(TFactory f, const char* step, float alter, int octave, int duration, const char* type)
	{ return __retainElt (f->newnote (step, alter, octave, duration, type)); }
EXP TElement	factoryRest	(TFactory f, int duration, const char* type)
	{ return __retainElt (f->newrest (duration, type)); }
EXP TElement	factoryDynamic	(TFactory f, int type, const char* placement)
	{ return __retainElt (f->newdynamics (type, placement)); }
EXP TElement	factoryBarline	(TFactory f, const char* location, const char* barstyle, const char *repeat)
	{ return __retainElt (f->newbarline (location, barstyle, repeat)); }



EXP void		factoryTuplet (TFactory f, int actual, int normal, TElement * notes)
{
	vector<Sxmlelement> list;
	while (*notes) {
		list.push_back(*notes);
		notes++;
	}
	f->maketuplet (actual, normal, list);
}

EXP void		factoryTie	(TFactory f, TElement from, TElement to)
	{ f->tie (from, to); }
EXP void		factoryNotation	(TFactory f, TElement elt, TElement notation)
	{ f->addnotation (elt, __releaseElt(notation)); }
EXP void		factoryArticulation	(TFactory f, TElement elt, TElement articulation)
	{ f->addarticulation (elt, __releaseElt(articulation)); }

EXP void		factoryChord (TFactory f, TElement * notes)
{
	vector<Sxmlelement> list;
	while (*notes) {
		list.push_back(*notes);
		notes++;
	}
	f->makechord (list);
}

//------------------------------------------------------------------------
EXP void		factoryAddElement	(TFactory f, TElement elt, TElement subelt)
	{ f->add (elt, __releaseElt(subelt)); }
EXP void		factoryAddAttribute	(TFactory f, TElement elt, TAttribute attr)
	{ f->add (elt, __releaseAttr(attr)); }


EXP void		factoryAddElements	(TFactory f, TElement elt, TElement* subelts)
{
	vector<Sxmlelement> list;
	while (*subelts) {
		list.push_back(*subelts);
		subelts++;
	}
	f->add (elt, list);
}


//------------------------------------------------------------------------
// elements creation
//------------------------------------------------------------------------
template <typename T> TElement	__factoryElement (TFactory f, int type, T value)
	{ return __retainElt (f->element (type, value)); }

EXP TElement	factoryElement		(TFactory f, int type)
	{ return __factoryElement (f, type, (char*)0); }
EXP TElement	factoryStrElement	(TFactory f, int type, const char * value)
	{ return __factoryElement (f, type, value); }
EXP TElement	factoryIntElement	(TFactory f, int type, int value)
	{ return __factoryElement (f, type, value); }
EXP TElement	factoryFloatElement	(TFactory f, int type, float value)
	{ return __factoryElement (f, type, value); }

//------------------------------------------------------------------------
// attributes creation
//------------------------------------------------------------------------
template <typename T> TAttribute	__factoryAttribute (TFactory f, const char * name, T value)
	{ return __retainAttr (f->attribute (name, value)); }

EXP TAttribute	factoryStrAttribute		(TFactory f, const char * name, const char* value)
	{ return __factoryAttribute (f, name, value); }
EXP TAttribute	factoryIntAttribute		(TFactory f, const char * name, int value)
	{ return __factoryAttribute (f, name, value); }
EXP TAttribute	factoryFloatAttribute	(TFactory f, const char * name, float value)
	{ return __factoryAttribute (f, name, value); }

//------------------------------------------------------------------------
EXP void		factoryFreeElement		(TFactory f, TElement elt)		{ elt->removeReference(); }
EXP void		factoryFreeAttribute	(TFactory f, TAttribute attr)	{ attr->removeReference(); }



}
