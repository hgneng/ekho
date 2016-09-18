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
#include <time.h>

#include "libmusicxml.h"
#include "musicxmlfactory.h"
#include "factory.h"
#include "sortvisitor.h"
#include "xml_tree_browser.h"
#include "versions.h"

using namespace std;
namespace MusicXML2
{


//------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------
musicxmlfactory::musicxmlfactory ()
{
//	init();
	fFile = TXMLFile::create();
	fRoot = factory::instance().create(k_score_partwise);

	fFile->set( new TXMLDecl("1.0", "", TXMLDecl::kNo));
	fFile->set( new TDocType("score-partwise"));
	fFile->set (fRoot);
	
	fIdentification = factory::instance().create(k_identification);
	fRoot->push (fIdentification);
	
	fPartList = factory::instance().create(k_part_list);
	fRoot->push (fPartList);
}

//------------------------------------------------------------------------
Sxmlelement	musicxmlfactory::scorepart (const char* id, const char* name, const char* abbrev)
{
	Sxmlelement part = element(k_score_part);
	part->add (attribute("id", id));
	if (name)			part->push (element(k_part_name, name));
	if (abbrev)			part->push (element(k_part_abbreviation, abbrev));
	return part;
}

//------------------------------------------------------------------------
Sxmlelement	musicxmlfactory::part (const char* id)
{
	Sxmlelement part = element(k_part);
	part->add (attribute("id", id));
	return part;
}

//------------------------------------------------------------------------
void musicxmlfactory::header (const char* worknumber, const char* worktitle, const char* movementnumber, const char* movementtitle)
{
	if (worknumber || worktitle) {
		Sxmlelement work = element(k_work);
		if (worknumber) work->push (element(k_work_number, worknumber));
		if (worktitle) work->push (element(k_work_title, worktitle));
		fRoot->push (work);
	}
	if (movementnumber) fRoot->push (element(k_movement_number, movementnumber));
	if (movementtitle) fRoot->push (element(k_movement_title, movementtitle));
}

//------------------------------------------------------------------------
void musicxmlfactory::creator (const char* c, const char* type)
{
	Sxmlelement creator = element(k_creator, c);
	if (type) creator->add (attribute("type", type));
	fIdentification->push (creator);
}

//------------------------------------------------------------------------
void musicxmlfactory::rights (const char* c, const char* type)
{
	Sxmlelement rights = element(k_rights, c);
	if (type) rights->add (attribute("type", type));
	fIdentification->push (rights);
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::newmeasure(int number) const
{
	Sxmlelement measure = element (k_measure);
	measure->add (attribute ("number", number));
	return measure;
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::newmeasure (int number, const char* time, const char* clef, int line, int key, int division) const
{
	Sxmlelement m = newmeasure (number);
	Sxmlelement attributes = getAttributes (m);
	if (division) attributes->push (element(k_divisions, division));
	if (time) {
		int beat, beatType;
		int n = sscanf (time, "%d/%d", &beat, &beatType);
		if (n == 2) {
			Sxmlelement t = element (k_time);
			t->push (element(k_beats, beat));
			t->push (element(k_beat_type, beatType));
			attributes->push (t);
		}
	}
	if (clef) {
		Sxmlelement c = element (k_clef);
		c->push (element (k_sign, clef));
		if (line) c->push (element (k_line, line));
		attributes->push (c);
	}
	if (key) {
		Sxmlelement k = element (k_key);
		k->push (element (k_fifths, key));
		attributes->push (k);
	}
	return m;
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::newnote (const char* step, float alter, int octave, int duration, const char* type)
{
	Sxmlelement elt = element(k_note);
	Sxmlelement pitch = element(k_pitch);
	pitch->push (element (k_step, step));
	if (alter) pitch->push (element (k_alter, alter));
	pitch->push (element (k_octave, octave));
	elt->push(pitch);
	if (duration) elt->push(element(k_duration, duration));
	if (type) elt->push(element(k_type, type));
	return elt;
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::newrest (int duration, const char* type)
{
	Sxmlelement elt = element(k_note);
	if (duration) elt->push(element(k_duration, duration));
	if (type) elt->push(element(k_type, type));
	return elt;
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::newdynamics (int type, const char* placement)
{
	Sxmlelement dynamics = element (k_dynamics);
	if (placement)
		dynamics->add (attribute( "placement", placement));
	dynamics->push (element(type));
	return dynamics;
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::newbarline (const char* location, const char* barstyle, const char *repeat)
{
	Sxmlelement barline = element (k_barline);
	if (location)
		barline->add (attribute( "location", location));
	if (barstyle)
		barline->push (element(k_bar_style, barstyle));
	if (repeat) {
		Sxmlelement repeatelt = (element(k_repeat));
		repeatelt->add (attribute( "direction", repeat));
		barline->push (repeatelt);
	}
	return barline;
}

//------------------------------------------------------------------------
void musicxmlfactory::makechord (const vector<Sxmlelement>& notes)
{
	if (notes.empty()) return;
	vector<Sxmlelement>::const_iterator i = notes.begin();
	i++;								// skip first note
	for (; i != notes.end(); i++)
		(*i)->push(element(k_chord));
}

//------------------------------------------------------------------------
void musicxmlfactory::maketuplet(int actual, int normal, const std::vector<Sxmlelement>& notes)
{
	if (notes.empty()) return;
	Sxmlelement timemod = element(k_time_modification);
	timemod->push (element (k_actual_notes, actual));
	timemod->push (element (k_normal_notes, normal));
	for (unsigned int i=0; i < notes.size(); i++)
		notes[i]->push(timemod);
	Sxmlelement notations = getNotations (notes[0]);
	Sxmlelement tuplet = element (k_tuplet);
	tuplet->add (attribute ("type", "start"));
	notations->push (tuplet);

	notations = getNotations (notes[notes.size()-1]);
	tuplet = element (k_tuplet);
	tuplet->add (attribute ("type", "stop"));
	notations->push (tuplet);
}

//------------------------------------------------------------------------
void musicxmlfactory::tie (Sxmlelement start, Sxmlelement stop)
{
	Sxmlelement tieStart = element (k_tie);
	tieStart->add (attribute ("type", "start"));
	start->push (tieStart);
	Sxmlelement tiedStart = element (k_tied);
	tiedStart->add (attribute ("type", "start"));
	addnotation (start, tiedStart);
	
	Sxmlelement tieStop = element (k_tie);
	tieStop->add (attribute ("type", "stop"));
	stop->push (tieStop);
	Sxmlelement tiedStop = element (k_tied);
	tiedStop->add (attribute ("type", "stop"));
	addnotation (stop, tiedStop);
}

//------------------------------------------------------------------------
void musicxmlfactory::addnotation (Sxmlelement elt, Sxmlelement notation)
{
	Sxmlelement notations = getNotations (elt);
	notations->push (notation);
}

//------------------------------------------------------------------------
void musicxmlfactory::addarticulation (Sxmlelement elt, Sxmlelement articulation)
{
	Sxmlelement articulations = getArticulations (elt);
	articulations->push (articulation);
}

//------------------------------------------------------------------------
void musicxmlfactory::addgroup (int number, const char* name, const char* abbrev, bool groupbarline, vector<Sxmlelement>& parts)
{
	Sxmlelement groupStart = element(k_part_group);
	groupStart->add (attribute ("number", number));
	groupStart->add (attribute ("type", "start"));
	if (name)			groupStart->push (element(k_group_name, name));
	if (abbrev)			groupStart->push (element(k_group_abbreviation, abbrev));
	if (groupbarline)	groupStart->push (element(k_group_barline, "yes"));
	fPartList->push (groupStart);

	for (vector<Sxmlelement>::const_iterator i = parts.begin(); i != parts.end(); i++)
		addpart(*i);

	Sxmlelement groupStop = element(k_part_group);
	groupStop->add (attribute ("number", number));
	groupStop->add (attribute ("type", "stop"));
	fPartList->push (groupStop);	
}

//------------------------------------------------------------------------
void musicxmlfactory::addpart (const Sxmlelement& part)	
{ 
	switch (part->getType()) {
		case k_score_part:
			fPartList->push(part);
			break;
		case k_part:
			fRoot->push(part);
			break;
		default:
			cerr << "musicxmlfactory::addpart unexpected type " <<  part->getType() << endl;
	}
}

//------------------------------------------------------------------------
void musicxmlfactory::add (Sxmlelement elt, const std::vector<Sxmlelement>& subelts) const
{
	for (unsigned int i=0; i < subelts.size(); i++) 
		elt->push( subelts[i]);
}

//------------------------------------------------------------------------
Sxmlelement musicxmlfactory::element(int type, const char * value) const
{
	Sxmlelement elt = factory::instance().create(type);
	if (value) elt->setValue (value);
	return elt;
}
Sxmlelement musicxmlfactory::element(int type, int value) const
{
	Sxmlelement elt = factory::instance().create(type);
	if (value) elt->setValue (value);
	return elt;
}
Sxmlelement musicxmlfactory::element(int type, float value) const
{
	Sxmlelement elt = factory::instance().create(type);
	if (value) elt->setValue (value);
	return elt;
}

//------------------------------------------------------------------------
static const char * timestring ()
{
	static char buff[64];
	time_t t;
	time(&t);
	strftime(buff, 64, "%Y-%m-%d", gmtime(&t));
	return buff;
}

//------------------------------------------------------------------------
void musicxmlfactory::encoding(const char* software)
{
	Sxmlelement encoding = element (k_encoding);
	if (software) encoding->push (element(k_software, software));

	string lib = "MusicXML Library version ";
	lib += musicxmllibVersionStr();
	encoding->push (element(k_software, lib.c_str()));
	encoding->push (element (k_encoding_date, timestring()));
	fIdentification->push (encoding);
}

//------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------
void musicxmlfactory::sort()
{
	sortvisitor sorter;
	xml_tree_browser browser(&sorter);
	browser.browse(*fRoot);
}

//------------------------------------------------------------------------
Sxmlelement	musicxmlfactory::getSubElement (Sxmlelement elt, int type) const
{
	vector<Sxmlelement>&  subelts = elt->elements();
	for (unsigned int i=0; i < subelts.size(); i++) {
		if (subelts[i]->getType() == type)
			return subelts[i];
	}
	Sxmlelement sub = element(type);
	elt->push (sub);
	return sub;
}

}
