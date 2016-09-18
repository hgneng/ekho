/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifdef WIN32
# pragma warning (disable : 4786)
#endif

#include <iostream>
#include <math.h>

#include "factory.h"
#include "transposition.h"
#include "transposevisitor.h"
#include "xml_tree_browser.h"

#ifdef WIN32
inline int round(float v) {
	int n = int(v);
	if ( (v-n) >= 0.5)  n++;
	if ( (v-n) <= -0.5) n--;
	return n;
}
#endif

using namespace std;

namespace MusicXML2
{

//________________________________________________________________________
transposition::transposition (int steps)
{
	fChromaticSteps = steps;
	fOctaveChange = getOctave(fChromaticSteps);
	fTableShift = getKey (getOctaveStep(fChromaticSteps));
	fKeySign = fCurrentKeySign = getKey(fChromaticSteps);
	initialize();
}

transposition::~transposition() {}

//________________________________________________________________________
/*
	The cycle of fifth is a special ordering of notes, beginning, say, with a F
	double flat, and ending with a B double sharp, with an interval of a perfect
	fifth between each note. This leads to the following list:
	Fbb, Cbb, Gbb, Dbb, Abb, Ebb, Bbb, Fb, Cb, Gb, Db, Ab, Eb, Bb, F, C, G, D,
	A, E, B, F#, C#, G#, D#, A#, E#, B#, F##, C##, G##, D##, A##, E##, B##.
	To apply transposition, we first look in the table for the correct
	shifting, and apply the same to every note to transpose.
*/
void transposition::initialize ()
{
	fFifthCycle.clear();
	for (int i=-2; i<=2; i++) {
		fFifthCycle.push_back(make_pair("F", i));
		fFifthCycle.push_back(make_pair("C", i));
		fFifthCycle.push_back(make_pair("G", i));
		fFifthCycle.push_back(make_pair("D", i));
		fFifthCycle.push_back(make_pair("A", i));
		fFifthCycle.push_back(make_pair("E", i));
		fFifthCycle.push_back(make_pair("B", i));
	}
}

//________________________________________________________________________
// transpose a pitch using the table of fifth cycle
void transposition::transpose ( string& pitch, float& alter, int& octave, int tableshift ) const
{
	int ialter = round(alter);
	float diff = alter - ialter;
	int pitch1 = notevisitor::step2i(pitch);
	for (unsigned int i=0; i < fFifthCycle.size(); i++) {
		if ((fFifthCycle[i].second == ialter) && (fFifthCycle[i].first == pitch)) {
			i += tableshift;
			if (i > fFifthCycle.size()) i -= 12;
			else if (i < 0) i += 12;

			pitch = fFifthCycle[i].first;
			alter = fFifthCycle[i].second + diff;

			int pitch2 = notevisitor::step2i(pitch);
			if ((pitch2 < pitch1) && (fChromaticSteps > 0)) octave++;
			else if ((pitch2 > pitch1) && (fChromaticSteps < 0)) octave--;

			return;
		}
	}
	cerr << "transpose: pitch out of fifth cycle table (" << pitch << " " << ialter << ")" << endl;
}

//________________________________________________________________________
void transposition::setPitch ( notevisitor& v )
{
	if (v.getType() != notevisitor::kPitched) return;

	string step = v.getStep();
	float alter = v.getAlter();
	int oct = v.getOctave() + fOctaveChange;
	transpose(step, alter, oct, fTableShift);

	v.setStep (step);
	v.setOctave (oct);
	v.setAlter (alter);
}

//________________________________________________________________________
// return a note accidental according to a key signature
int transposition::getAccidental ( Diatonic pitch, int key )
{
	int curpitch = notevisitor::F;
	while (key > 0) {
		if (pitch == curpitch)	return 1;		// pitch is sharp
		curpitch += 4;									// add a diatonic fifth
		if (curpitch > notevisitor::diatonicSteps)
			curpitch -= notevisitor::diatonicSteps;		// octave reduction
		key--;
	}

	curpitch = notevisitor::B;
	while (key < 0) {
		if (pitch == curpitch)	return -1;		// pitch is flat
		curpitch += 3;									// add a diatonic fourth
		if (curpitch > notevisitor::diatonicSteps)
			curpitch -= notevisitor::diatonicSteps;		// octave reduction
		key++;
	}
	return 0;
}

//________________________________________________________________________
string transposition::getAccident ( float alter )
{
	if (alter >= 2)			return "double_sharp";
	else if (alter >= 1)	return "sharp";
	else if (alter >= 0)	return "natural";
	else if (alter <= -1)	return "flat";
	else if (alter <= -2)	return "flat_flat";
	else cerr << "getAccident: alteration " << alter << " not supported" << endl;
	return "";
}

//________________________________________________________________________
int transposition::transposeKey (int key, Chromatic steps, int& enharmonicChange)
{
	int newkey = key + getKey(steps);
	if (newkey >= 6) {
		newkey -= 12;
		enharmonicChange = 1;
	}
	else if (newkey < -6) {
		newkey += 12;
		enharmonicChange = -1;
	}
	else enharmonicChange = 0;
	return newkey;
}

//________________________________________________________________________
int transposition::getKey (  Chromatic steps )
{
	// the method looks for the key signature corresponding to a chromatic degree
	// the key signature is the simplest between flats and sharps alternatives
	// sharps are positive values and flats are negative
	steps = getOctaveStep(steps);		// first converts into a value modulo 12
	if (steps < 0) steps = 12 + steps;	// works only on positive values

	int sharps = 0;
	int curstep = 0;
	while (curstep != steps) {
		curstep += 7;		 	// add a fifth
		curstep %= 12;			// modulus an octave
		sharps++;
	}
	return (sharps >= 6 ? sharps - 12 : sharps);	// simplest key is chosen here
}

//________________________________________________________________________
ctree<xmlelement>::iterator transposition::insertAlter ( S_note& elt, ctree<xmlelement>::iterator pos, float value )
{
	Sxmlelement alter = factory::instance().create(k_alter);
	if (alter) {
		alter->setValue (value);
		pos = elt->insert (pos, alter);	// position is now the inserted element
		pos++;							// go back to the previous position
	}
	return pos;
}

//________________________________________________________________________
ctree<xmlelement>::iterator transposition::insertAccident ( S_note& elt, ctree<xmlelement>::iterator pos, const string& value )
{
	Sxmlelement acc = factory::instance().create(k_accidental);
	if (acc) {
		acc->setValue (value);
		pos = elt->insert (pos, acc);	// position is now the inserted element
		pos++;							// go back to the previous position
	}
	return pos;
}

//________________________________________________________________________
// The visit methods
//________________________________________________________________________
void transposition::visitEnd ( S_note& elt )
{
	bool accidentDone = false;
	bool alterDone = false;
	notevisitor::visitEnd (elt);
	setPitch (*this);		// computes the new pitch according to the current note data

	Diatonic step = notevisitor::step2i(notevisitor::getStep());
	int keyAccident = getAccidental (step, fCurrentKeySign);
	float alter = notevisitor::getAlter();
	string accident;		// next computes the accidental data
	if (alter)
		accident = getAccident (alter);
	else if (keyAccident)
		accident = "natural";

	ctree<xmlelement>::iterator next;
	for (ctree<xmlelement>::iterator i = elt->begin(); i != elt->end(); ) {
		next = i;
		next++;
		switch (i->getType()) {

			case k_step:
				i->setValue( notevisitor::getStep() );
				break;
			case k_octave:
				i->setValue( notevisitor::getOctave() );
				if (!alterDone && alter)
					next = insertAlter(elt, i, alter)++;
				break;
			case k_alter:
				if (!alter) next = elt->erase(i);
				else i->setValue( alter );
				alterDone = true;
				break;
			case k_accidental:
				if (accident.empty()) next = elt->erase(i);
				else i->setValue(accident);
				accidentDone = true;
				break;

			case k_time_modification:
			case k_stem:
			case k_notehead:
			case k_staff:
			case k_beam:
			case k_notations:
			case k_lyric:
				if (!accidentDone && accident.size()) {
					next = insertAccident(elt, i, accident)++;
					accidentDone = true;
				}
				break;
		}
		i = next;
	}
	if (!accidentDone && accident.size())
		insertAccident(elt, elt->end(), accident);
}

//________________________________________________________________________
// key management
// non standard keys are not yet supported
//________________________________________________________________________
void transposition::visitStart ( S_cancel& elt )
{
	int foo;		// enharmonic change is ignored for cancel elements
	int newkey = transposeKey (int(*elt), fChromaticSteps, foo);
	elt->setValue( newkey );
}

//________________________________________________________________________
void transposition::visitStart ( S_fifths& elt )
{
	int enharmonic;
	fCurrentKeySign = transposeKey (int(*elt), fChromaticSteps, enharmonic);
	elt->setValue( fCurrentKeySign );
	// simple enharmonic key has been chosen, adjust the fifths table shift value
	if (enharmonic) fTableShift -= enharmonic * 12;
}

//________________________________________________________________________
void transposition::visitStart ( S_part& elt ) {
	fCurrentKeySign = fKeySign;
	fTableShift = getKey (getOctaveStep(fChromaticSteps));
}

}
