/*

  Copyright (C) 2003-2008  Grame
  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the MusicXML Library use.
  It shows how to build a memory representation of a score from scratch. 

*/


#include <ctime>
#include <iostream>
#include <string>
#include <stdlib.h>

#ifdef WIN32
#include <windows.h>
#endif

#include "elements.h"
#include "factory.h"
#include "xml.h"
#include "xmlfile.h"

using namespace MusicXML2;
using namespace std;

#define kDivision	4

//------------------------------------------------------------------------
// a function that return random numbers in the given range
//------------------------------------------------------------------------
static int getrandom(int range) {
#ifdef WIN32
	float f = (float)rand() / RAND_MAX;
#else
	float f = (float)random() / RAND_MAX;
#endif
	return (int)(f * range); 
}

static string randomNote() {
	int n = getrandom(7);
	string note;
	note += (char('A' + n));
	return note; 
}

//------------------------------------------------------------------------
static Sxmlattribute newAttribute(const string& name, const string& value)
{
	Sxmlattribute attribute = xmlattribute::create();
	attribute->setName(name);
	attribute->setValue(value);
	return attribute;
}

//------------------------------------------------------------------------
static Sxmlattribute newAttributeI(const string& name, int value)
{
	Sxmlattribute attribute = xmlattribute::create();
	attribute->setName(name);
	attribute->setValue(value);
	return attribute;
}

//------------------------------------------------------------------------
static Sxmlelement newElement(int type, const string& value)
{
	Sxmlelement elt = factory::instance().create(type);
	elt->setValue (value);
	return elt;
}

//------------------------------------------------------------------------
static Sxmlelement newElementI(int type, int value)
{
	Sxmlelement elt = factory::instance().create(type);
	elt->setValue (value);
	return elt;
}

//------------------------------------------------------------------------
static Sxmlelement makeAttributes() {
	Sxmlelement attributes = factory::instance().create(k_attributes);
	attributes->push (newElementI(k_divisions, kDivision));

	Sxmlelement time = factory::instance().create(k_time);
	time->push (newElement(k_beats, "4"));
	time->push (newElement(k_beat_type, "4"));
	attributes->push (time);

	Sxmlelement clef = factory::instance().create(k_clef);
	clef->push (newElement(k_sign, "G"));
	clef->push (newElement(k_line, "2"));
	attributes->push (clef);
	
	return attributes;
}

//------------------------------------------------------------------------
// creates a measure containing random notes
// the function takes the measure number as an argument
//------------------------------------------------------------------------
static Sxmlelement makemeasure(unsigned long num) {
	Sxmlelement measure = factory::instance().create(k_measure);
	measure->add (newAttributeI("number", num));
	if (num==1) {					//  creates specific elements of the first measure
		measure->push(makeAttributes());		// division, time, clef...
	}
	for (int i = 0; i < 4; i++) {		// next adds 4 quarter notes
		Sxmlelement note = factory::instance().create(k_note);		// creates the note
		Sxmlelement pitch = factory::instance().create(k_pitch);	// creates a pitch
		pitch->push (newElement(k_step, randomNote()));				// sets the pitch to a random value
		pitch->push (newElementI(k_octave, 4 + getrandom(2)));		// sets the octave to a random value
		note->push (pitch);											// adds the pitch to the note
		note->push (newElementI(k_duration, kDivision));				// sets the note duration to a quarter note
		note->push (newElement(k_type, "quarter"));					// creates the graphic elements of the note
		measure->push (note);		// and finally adds the note to the measure
	}
	return measure;
}

#define kPartID	"P1"
//------------------------------------------------------------------------
// creates a part containing 'count' measures
//------------------------------------------------------------------------
Sxmlelement makePart(int count) {
	Sxmlelement part = factory::instance().create(k_part);
	part->add (newAttribute("id", kPartID));
	for (int i=1; i<=count; i++)			// and 'count' times
		part->push (makemeasure(i));			// adds a new measure to the part
	return part;
}

//------------------------------------------------------------------------
// creates the part list element
//------------------------------------------------------------------------
static Sxmlelement makePartList() {
	Sxmlelement partlist = factory::instance().create(k_part_list);
	Sxmlelement scorepart = factory::instance().create(k_score_part);
	scorepart->add (newAttribute("id", kPartID));
	scorepart->push (newElement(k_part_name, "Part name"));
	Sxmlelement scoreinstr = factory::instance().create(k_score_instrument);
	scoreinstr->add (newAttribute("id", "I1"));
	scoreinstr->push (newElement(k_instrument_name, "Any instr."));
	scorepart->push (scoreinstr);
	partlist->push(scorepart);
	return partlist;
}

//------------------------------------------------------------------------
// creates the identification element
//------------------------------------------------------------------------
static Sxmlelement makeIdentification() {
	Sxmlelement id = factory::instance().create(k_identification);
	Sxmlelement encoding = factory::instance().create(k_encoding);

	Sxmlelement creator = newElement(k_creator, "Georg Chance");
	creator->add(newAttribute("type", "Composer"));
	id->push (creator);
	
	encoding->push (newElement(k_software, "MusicXML Library v2"));
	id->push (encoding);
	return id;
}

//------------------------------------------------------------------------
// the function that creates and writes the score
//------------------------------------------------------------------------
static Sxmlelement randomMusic(int measuresCount) {
	Sxmlelement score = factory::instance().create(k_score_partwise);
	score->push (newElement(k_movement_title, "Random Music"));
	score->push (makeIdentification());
	score->push (makePartList());
	score->push(makePart(measuresCount));			// adds a part to the score
	return score;
}

int main (int argc, char * argv[]) {
	// if present, reads the measures count from the command line
	int count = (argc == 2) ? atoi(argv[1]) : 20;
	// sets the random numbers seed
#ifdef WIN32
	srand((unsigned)time(0));
#else
	srandom((unsigned)time(0));
#endif
	SXMLFile f = TXMLFile::create();
	f->set( new TXMLDecl("1.0", "", TXMLDecl::kNo));
	f->set( new TDocType("score-partwise"));
	f->set( randomMusic(count) );
	f->print(cout);
	cout << endl;
    return 0;
}

