/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>

#include "conversions.h"
#include "partsummary.h"
#include "rational.h"
#include "xml2guidovisitor.h"
#include "xmlpart2guido.h"
#include "xml_tree_browser.h"

using namespace std;

namespace MusicXML2 
{

//______________________________________________________________________________
xmlpart2guido::xmlpart2guido(bool generateComments, bool generateStem, bool generateBar) : 
	fGenerateComments(generateComments), fGenerateStem(generateStem), 
	fGenerateBars(generateBar),
	fNotesOnly(false), fCurrentStaffIndex(0), fCurrentStaff(0),
	fTargetStaff(0), fTargetVoice(0)
{
	fGeneratePositions = true;
	xmlpart2guido::reset();
}

//______________________________________________________________________________
void xmlpart2guido::reset ()
{
	guidonotestatus::resetall();
	fCurrentBeamNumber = 0;
	fMeasNum = 0;
	fInCue = fInGrace = fInhibitNextBar = fPendingBar 
		   = fBeamOpened = fCrescPending = fSkipDirection = false;
	fCurrentStemDirection = kStemUndefined;
	fCurrentDivision = 1;
	fCurrentOffset = 0;
	fPendingPops = 0;
	fMeasNum = 0;
}

//______________________________________________________________________________
void xmlpart2guido::initialize (Sguidoelement seq, int staff, int guidostaff, int voice, 
		bool notesonly, rational defaultTimeSign) 
{
	fCurrentStaff = fTargetStaff = staff;	// the current and target staff
	fTargetVoice = voice;					// the target voice
	fNotesOnly = notesonly;					// prevent multiple output for keys, clefs etc... 
	fCurrentTimeSign = defaultTimeSign;		// a default time signature
	fCurrentStaffIndex = guidostaff;		// the current guido staff index
	start (seq);
}

//________________________________________________________________________
// some code for the delayed elements management
// delayed elements are elements enclosed in a <direction> element that
// contains a non-null <offset> element. This offset postpone the graphic
// appearance of the element in 'offset' time units in the futur.
// Time units are <division> time units 
//________________________________________________________________________
// add an element to the list of delayed elements
void xmlpart2guido::addDelayed (Sguidoelement elt, long offset) 
{
	add(elt);
	return;
	
	if (offset > 0) {
		delayedElement de;
		de.delay = offset;
		de.element = elt;
		fDelayed.push_back(de);
	}
	else add (elt);
}

//________________________________________________________________________
// checks ready elements in the list of delayed elements
// 'time' is the time elapsed since the last check, it is expressed in
// <division> time units
void xmlpart2guido::checkDelayed (long time)
{
	vector<delayedElement>::iterator it = fDelayed.begin();
	while (it!=fDelayed.end()) {
		it->delay -= time;
		if (it->delay < 0) {
			add (it->element);
			it = fDelayed.erase(it);
		}
		else it++;
	}
}

//______________________________________________________________________________
void xmlpart2guido::stackClean () 
{
	if (fInCue) {
		pop();			
		fInCue = false;
	}
	if (fInGrace) {
		pop();			
		fInGrace = false;
	}
}

//______________________________________________________________________________
void xmlpart2guido::checkStaff (int staff) {
    if (staff != fCurrentStaff) {
        Sguidoelement tag = guidotag::create("staff");
		int offset = staff - fCurrentStaff;
//cout << "move from staff " << fCurrentStaffIndex << " to " << (fCurrentStaffIndex + offset) << endl;
		fCurrentStaff = staff;
        fCurrentStaffIndex += offset;
		tag->add (guidoparam::create(fCurrentStaffIndex, false));
        add (tag);
    }
}

//______________________________________________________________________________
void xmlpart2guido::moveMeasureTime (int duration, bool moveVoiceToo)
{
	rational r(duration, fCurrentDivision*4);
	r.rationalise();
	fCurrentMeasurePosition += r;
	fCurrentMeasurePosition.rationalise();
	if (fCurrentMeasurePosition > fCurrentMeasureLength)
		fCurrentMeasureLength = fCurrentMeasurePosition;
	if (moveVoiceToo) {
		fCurrentVoicePosition += r;
		fCurrentVoicePosition.rationalise();
	}
}

//______________________________________________________________________________
// check the current position in the current voice:  when it lags behind 
// the current measure position, it creates the corresponding empty element
//______________________________________________________________________________
void xmlpart2guido::checkVoiceTime ( const rational& currTime, const rational& voiceTime)
{
	rational diff = currTime - voiceTime;
	diff.rationalise();
	if (diff.getNumerator() > 0) {
		guidonoteduration dur (diff.getNumerator(), diff.getDenominator());
		Sguidoelement note = guidonote::create(fTargetVoice, "empty", 0, dur, "");
		add (note);
		fCurrentVoicePosition += diff;
		fCurrentVoicePosition.rationalise();
	}
	else if (diff.getNumerator() < 0)
		cerr << "warning! checkVoiceTime: measure time behind voice time " << string(diff) << endl;
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_backup& elt )
{
	stackClean();	// closes pending chords, cue and grace
	int duration = elt->getIntValue(k_duration, 0);
	if (duration) {		
		// backup is supposed to be used only for moving between voices
		// thus we don't move the voice time (which is supposed to be 0)
		moveMeasureTime (-duration, false);
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_forward& elt )
{
	bool scanElement = (elt->getIntValue(k_voice, 0) == fTargetVoice) 
						&& (elt->getIntValue(k_staff, 0) == fTargetStaff);
	int duration = elt->getIntValue(k_duration, 0);
	moveMeasureTime(duration, scanElement);
	if (!scanElement) return;

	stackClean();	// closes pending chords, cue and grace

	if (duration) {		
		rational r(duration, fCurrentDivision*4);
		r.rationalise();
		guidonoteduration dur (r.getNumerator(), r.getDenominator());
		Sguidoelement note = guidonote::create(fTargetVoice, "empty", 0, dur, "");
		add (note);
		fMeasureEmpty = false;
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_part& elt ) 
{
	reset();
	if (!current()) {
		Sguidoelement seq = guidoseq::create();
		start (seq);
	}
}


//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_measure& elt ) 
{
	const string& implicit = elt->getAttributeValue ("implicit");
	if (implicit == "yes") fPendingBar = false;
	if (fPendingBar) {
		// before adding a bar, we need to check that there are no repeat begin at this location
		ctree<xmlelement>::iterator repeat = elt->find(k_repeat);
		if ((repeat == elt->end()) || (repeat->getAttributeValue("direction") != "forward")) {
			checkStaff (fTargetStaff);
			Sguidoelement tag = guidotag::create("bar");
			add (tag);
		}
	}
	fCurrentMeasure = elt;
	fMeasNum++;
	fCurrentMeasureLength.set  (0, 1);
	fCurrentMeasurePosition.set(0, 1);
	fCurrentVoicePosition.set  (0, 1);
	fInhibitNextBar = false; // fNotesOnly;
	fPendingBar = false;
	fPendingPops = 0;
	fMeasureEmpty = true;
	if (fGenerateComments) {
		stringstream s;
		s << "   (* meas. " << fMeasNum << " *) ";
		string comment="\n"+s.str();
		Sguidoelement elt = guidoelement ::create(comment);
		add (elt);
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_measure& elt ) 
{
	stackClean();	// closes pending chords, cue and grace
	checkVoiceTime (fCurrentMeasureLength, fCurrentVoicePosition);	

	if (!fInhibitNextBar) {
		if (fGenerateBars) fPendingBar = true;
		else if (!fMeasureEmpty) {
			if (fCurrentVoicePosition < fCurrentMeasureLength)
				fPendingBar = true;
		}
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_direction& elt ) 
{
	if (fNotesOnly || (elt->getIntValue(k_staff, 0) != fTargetStaff)) {
		fSkipDirection = true;
	}
	else {
		fCurrentOffset = elt->getLongValue(k_offset, 0);
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_direction& elt ) 
{
	fSkipDirection = false;
	fCurrentOffset = 0;
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_key& elt ) 
{
	if (fNotesOnly) return;
	Sguidoelement tag = guidotag::create("key");
	tag->add (guidoparam::create(keysignvisitor::fFifths, false));
    add (tag);
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_coda& elt )
{
	if (fSkipDirection) return;
	Sguidoelement tag = guidotag::create("coda");
	add(tag);
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_segno& elt )
{
	if (fSkipDirection) return;
	Sguidoelement tag = guidotag::create("segno");
	add(tag);
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_wedge& elt )
{
	if (fSkipDirection) return;

	string type = elt->getAttributeValue("type");
	Sguidoelement tag;
	if (type == "crescendo") {
		tag = guidotag::create("crescBegin");
		fCrescPending = true;
	}
	else if (type == "diminuendo") {
		tag = guidotag::create("dimBegin");
		fCrescPending = false;
	}
	else if (type == "stop") {
		tag = guidotag::create(fCrescPending ? "crescEnd" : "dimEnd");
	}
	if (tag) {
		if (fCurrentOffset) addDelayed(tag, fCurrentOffset);
		else add (tag);
	}

}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_metronome& elt ) 
{
	if (fSkipDirection) return;

	metronomevisitor::visitEnd (elt);
	if (fBeats.size() != 1) return;					// support per minute tempo only (for now)
	if (!metronomevisitor::fPerMinute) return;		// support per minute tempo only (for now)

	Sguidoelement tag = guidotag::create("tempo");
	beat b = fBeats[0];
	rational r = NoteType::type2rational(NoteType::xml(b.fUnit)), rdot(3,2);
	while (b.fDots-- > 0) {
		r *= rdot;
	}
	r.rationalise();

	stringstream s;
	s << "[" << (string)r << "] = " << metronomevisitor::fPerMinute;
	tag->add (guidoparam::create("tempo=\""+s.str()+"\"", false));
	if (fCurrentOffset) addDelayed(tag, fCurrentOffset);
	add (tag);
}

//______________________________________________________________________________
void xmlpart2guido::visitStart( S_dynamics& elt)
{
	if (fSkipDirection) return;

	ctree<xmlelement>::literator iter;
	for (iter = elt->lbegin(); iter != elt->lend(); iter++) {
		if ((*iter)->getType() != k_other_dynamics) {
			Sguidoelement tag = guidotag::create("intens");
			tag->add (guidoparam::create((*iter)->getName()));
			if (fGeneratePositions) xml2guidovisitor::addPosition(elt, tag, 12);
			if (fCurrentOffset) addDelayed(tag, fCurrentOffset);
			else add (tag);
		}
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitStart( S_octave_shift& elt)
{
	if (fSkipDirection) return;

	const string& type = elt->getAttributeValue("type");
	int size = elt->getAttributeIntValue("size", 0);

	switch (size) {
		case 8:		size = 1; break;
		case 15:	size = 2; break;
		default:	return;
	}

	if (type == "up")
		size = -size;
	else if (type == "stop")
		size = 0;
	else if (type != "down") return;

	Sguidoelement tag = guidotag::create("oct");
	if (tag) {
		tag->add (guidoparam::create(size, false));
//		add (tag);			// todo: handling of octave offset with notes
// in addition, there is actually a poor support for the oct tag in guido
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_note& elt ) 
{
	notevisitor::visitStart ( elt );
}

//______________________________________________________________________________
string xmlpart2guido::alter2accident ( float alter ) 
{
	stringstream s;
	while (alter > 0.5) {
		s << "#";
		alter -= 1;
	}
	while (alter < -0.5) {
		s << "&";
		alter += 1;
	}
	
	string accident;
	s >> accident;
	return accident;
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_sound& elt )
{
	if (fNotesOnly) return;

	Sguidoelement tag = 0;
	Sxmlattribute attribute;
	
	if ((attribute = elt->getAttribute("dacapo")))
		tag = guidotag::create("daCapo");
	else {
		if ((attribute = elt->getAttribute("dalsegno"))) {
			tag = guidotag::create("dalSegno");
		}
		else if ((attribute = elt->getAttribute("tocoda"))) {
			tag = guidotag::create("daCoda");
		}
		else if ((attribute = elt->getAttribute("fine"))) {
			tag = guidotag::create("fine");
		}
//		if (tag) tag->add(guidoparam::create("id="+attribute->getValue(), false));
	}
	if (tag) add (tag);
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_ending& elt )
{
	string type = elt->getAttributeValue("type");
	if (type == "start") {
		Sguidoelement tag = guidotag::create("volta");
		string num = elt->getAttributeValue ("number");
		tag->add(guidoparam::create(num, true));
		tag->add(guidoparam::create(num + ".", true));
		push(tag);
	}
	else {
		if (type == "discontinue")
			current()->add(guidoparam::create("format=\"|-\"", false));
		pop();
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_repeat& elt ) 
{
	Sguidoelement tag;
	string direction = elt->getAttributeValue("direction");
	if (direction == "forward") 
		tag = guidotag::create("repeatBegin");
	else if (direction == "backward") {
		tag = guidotag::create("repeatEnd");
		fInhibitNextBar = true;
	}
	if (tag) add(tag);	
}

//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_barline& elt ) 
{
	const string& location = elt->getAttributeValue("location");
	if (location == "middle") {
		// todo: handling bar-style (not yet supported in guido)
		Sguidoelement tag = guidotag::create("bar");
		add(tag);
	}
	// todo: support for left and right bars
	// currently automatically handled at measure boundaries
	else if (location == "right") {
	}
	else if (location == "left") {
	}
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_time& elt ) 
{
	string timesign;
	if (!timesignvisitor::fSenzaMisura) {
    	if (timesignvisitor::fSymbol == "common") {
			rational ts = timesignvisitor::timesign(0);
			if ((ts.getDenominator() == 2) && (ts.getNumerator() == 2))
				timesign = "C/";
			else if ((ts.getDenominator() == 4) && (ts.getNumerator() == 4))
				timesign = "C";
			else 
				timesign = string(ts);
			fCurrentTimeSign = ts;
		}
    	else if (timesignvisitor::fSymbol == "cut") {
            timesign = "C/";
			fCurrentTimeSign = rational(2,2);
		}
		else {
			stringstream s; string sep ="";
			fCurrentTimeSign.set(0,1);
			for (unsigned int i = 0; i < timesignvisitor::fTimeSign.size(); i++) {
				s << sep << timesignvisitor::fTimeSign[i].first << "/" << timesignvisitor::fTimeSign[i].second;
				sep = "+";
//				rational ts = timesignvisitor::timesign(i);
				fCurrentTimeSign += timesignvisitor::timesign(i);
			}
			s >> timesign;
		}

    }
	if (fNotesOnly) return;

	Sguidoelement tag = guidotag::create("meter");
    tag->add (guidoparam::create(timesign));
	if (fGenerateBars) tag->add (guidoparam::create("autoBarlines=\"off\"", false));
	add(tag);
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_clef& elt ) 
{
	int staffnum = elt->getAttributeIntValue("number", 0);
	if ((staffnum != fTargetStaff) || fNotesOnly) return;

	stringstream s; 
	if ( clefvisitor::fSign == "G")			s << "g";
	else if ( clefvisitor::fSign == "F")	s << "f";
	else if ( clefvisitor::fSign == "C")	s << "c";
	else if ( clefvisitor::fSign == "percussion")	s << "perc";
	else if ( clefvisitor::fSign == "TAB")	s << "TAB";
	else if ( clefvisitor::fSign == "none")	s << "none";
	else {													// unknown clef sign !!
		cerr << "warning: unknown clef sign \"" << clefvisitor::fSign << "\"" << endl;
		return;	
	}
	
	string param;
	if (clefvisitor::fLine != clefvisitor::kStandardLine) 
		s << clefvisitor::fLine;
    s >> param;
	if (clefvisitor::fOctaveChange == 1)
		param += "+8";
	else if (clefvisitor::fOctaveChange == -1)
		param += "-8";
	Sguidoelement tag = guidotag::create("clef");
	checkStaff (staffnum);
    tag->add (guidoparam::create(param));
    add(tag);
}

//______________________________________________________________________________
// tools and methods for converting notes
//______________________________________________________________________________
vector<S_slur>::const_iterator xmlpart2guido::findTypeValue ( const std::vector<S_slur>& slurs, const string& val ) const 
{
	std::vector<S_slur>::const_iterator i;
	for (i = slurs.begin(); i != slurs.end(); i++) {
		if ((*i)->getAttributeValue("type") == val) break;
	}
	return i;
}

//______________________________________________________________________________
vector<S_tied>::const_iterator xmlpart2guido::findTypeValue ( const std::vector<S_tied>& tied, const string& val ) const 
{
	std::vector<S_tied>::const_iterator i;
	for (i = tied.begin(); i != tied.end(); i++) {
		if ((*i)->getAttributeValue("type") == val) break;
	}
	return i;
}

//______________________________________________________________________________
vector<S_beam>::const_iterator xmlpart2guido::findValue ( const std::vector<S_beam>& beams, const string& val ) const 
{
	std::vector<S_beam>::const_iterator i;
	for (i = beams.begin(); i != beams.end(); i++) {
		if ((*i)->getValue() == val) break;
	}
	return i;
}

//______________________________________________________________________________
void xmlpart2guido::checkTiedBegin ( const std::vector<S_tied>& tied ) 
{
	std::vector<S_tied>::const_iterator i = findTypeValue(tied, "start");
	if (i != tied.end()) {
		Sguidoelement tag = guidotag::create("tieBegin");
		string num = (*i)->getAttributeValue ("number");
        if (num.size())
            tag->add (guidoparam::create(num, false));
		string placement = (*i)->getAttributeValue("placement");
        if (placement == "below")
            tag->add (guidoparam::create("curve=\"down\"", false));
		add(tag);
	}
}

void xmlpart2guido::checkTiedEnd ( const std::vector<S_tied>& tied ) 
{
	std::vector<S_tied>::const_iterator i = findTypeValue(tied, "stop");
	if (i != tied.end()) {
		Sguidoelement tag = guidotag::create("tieEnd");
		string num = (*i)->getAttributeValue ("number");
        if (num.size())
            tag->add (guidoparam::create(num, false));
		add(tag);
	}
}

//______________________________________________________________________________
void xmlpart2guido::checkSlurBegin ( const std::vector<S_slur>& slurs ) 
{
	std::vector<S_slur>::const_iterator i = findTypeValue(slurs, "start");
	if (i != slurs.end()) {
		string tagName = "slurBegin";
		string num = (*i)->getAttributeValue("number");
		if (num.size()) tagName += ":" + num;
		Sguidoelement tag = guidotag::create(tagName);
		string placement = (*i)->getAttributeValue("placement");
        if (placement == "below")
            tag->add (guidoparam::create("curve=\"down\"", false));
		add(tag);
	}
}

void xmlpart2guido::checkSlurEnd ( const std::vector<S_slur>& slurs ) 
{
	std::vector<S_slur>::const_iterator i = findTypeValue(slurs, "stop");
	if (i != slurs.end()) {
		string tagName = "slurEnd";
		string num = (*i)->getAttributeValue("number");
		if (num.size()) tagName += ":" + num;
		Sguidoelement tag = guidotag::create (tagName);
		add(tag);
	}
}

//______________________________________________________________________________
void xmlpart2guido::checkBeamBegin ( const std::vector<S_beam>& beams ) 
{
	std::vector<S_beam>::const_iterator i = findValue(beams, "begin");
	if (i != beams.end()) {
		if (!fBeamOpened ) {
			fCurrentBeamNumber = (*i)->getAttributeIntValue("number", 1);
//			Sguidoelement tag = guidotag::create("beamBegin");	// poor support of the begin end form in guido
//			add (tag);
			Sguidoelement tag = guidotag::create("beam");
			push (tag);
			fBeamOpened = true;
		}
	}
}

void xmlpart2guido::checkBeamEnd ( const std::vector<S_beam>& beams ) 
{
	std::vector<S_beam>::const_iterator i;
	for (i = beams.begin(); (i != beams.end()) && fBeamOpened; i++) {
		if (((*i)->getValue() == "end") && ((*i)->getAttributeIntValue("number", 1) == fCurrentBeamNumber)) {
			fCurrentBeamNumber = 0;
			pop();
			fBeamOpened = false;
		}
	}
/*
	std::vector<S_beam>::const_iterator i = findValue(beams, "end");
	if (i != beams.end()) {
		if (fCurrentBeamNumber == (*i)->getAttributeIntValue("number", 1)) {
			fCurrentBeamNumber = 0;
//			Sguidoelement tag = guidotag::create("beamEnd");	// poor support of the begin end form in guido
//			add (tag);
			pop();
			fBeamOpened = false;
		}
	}
*/
}

//______________________________________________________________________________
void xmlpart2guido::checkStem ( const S_stem& stem ) 
{
	Sguidoelement tag;
	if (stem) {
		if (stem->getValue() == "down") {
			if (fCurrentStemDirection != kStemDown) {
				tag = guidotag::create("stemsDown");
				fCurrentStemDirection = kStemDown;
			}
		}
		else if (stem->getValue() == "up") {
			if (fCurrentStemDirection != kStemUp) {
				tag = guidotag::create("stemsUp");
				fCurrentStemDirection = kStemUp;
			}
		}
		else if (stem->getValue() == "none") {
			if (fCurrentStemDirection != kStemNone) {
				tag = guidotag::create("stemsOff");
				fCurrentStemDirection = kStemNone;
			}
		}
		else if (stem->getValue() == "double") {
		}
	}
	else if (fCurrentStemDirection != kStemUndefined) {
		tag = guidotag::create("stemsAuto");
		fCurrentStemDirection = kStemUndefined;
	}
	if (tag) add(tag);
}

//______________________________________________________________________________
int xmlpart2guido::checkArticulation ( const notevisitor& note ) 
{
	int n = 0;
	Sguidoelement tag;
	if (note.fAccent) {
		tag = guidotag::create("accent");
		push(tag);
		n++;
	}
	if (note.fStrongAccent) {
		tag = guidotag::create("marcato");
		push(tag);
		n++;
	}
	if (note.fStaccato) {
		tag = guidotag::create("stacc");
		push(tag);
		n++;
	}
	if (note.fTenuto) {
		tag = guidotag::create("ten");
		push(tag);
		n++;
	}
	return n;
}

//______________________________________________________________________________
vector<Sxmlelement> xmlpart2guido::getChord ( const S_note& elt ) 
{
	vector<Sxmlelement> v;
	ctree<xmlelement>::iterator nextnote = find(fCurrentMeasure->begin(), fCurrentMeasure->end(), elt);
	if (nextnote != fCurrentMeasure->end()) nextnote++;	// advance one step
	while (nextnote != fCurrentMeasure->end()) {
		// looking for the next note on the target voice
		if ((nextnote->getType() == k_note) && (nextnote->getIntValue(k_voice,0) == fTargetVoice)) { 
			ctree<xmlelement>::iterator iter;			// and when there is one
			iter = nextnote->find(k_chord);
			if (iter != nextnote->end())
				v.push_back(*nextnote);
			else break;
		}
		nextnote++;
	}
	return v;
}

//______________________________________________________________________________
void xmlpart2guido::checkCue (const notevisitor& nv) 
{
	if (nv.isCue()) {
		if (!fInCue) {
			fInCue = true;
			Sguidoelement tag = guidotag::create("cue");
			push(tag);
		}
	}
	else if (fInCue) {
		fInCue = false;
		pop();			
	}
}

//______________________________________________________________________________
void xmlpart2guido::checkGrace (const notevisitor& nv) 
{
	if (nv.isGrace()) {
		if (!fInGrace) {
			fInGrace = true;
			Sguidoelement tag = guidotag::create("grace");
			push(tag);
		}
	}
	else if (fInGrace) {
		fInGrace = false;
		pop();			
	}
}

//______________________________________________________________________________
int xmlpart2guido::checkFermata (const notevisitor& nv) 
{
	if (nv.inFermata()) {
		Sguidoelement tag = guidotag::create("fermata");
        push(tag);
		return 1;
	}
	return 0;
}

//______________________________________________________________________________
string xmlpart2guido::noteName ( const notevisitor& nv )
{
	string accident = alter2accident(nv.getAlter());
	string name;
	if (nv.getType() == notevisitor::kRest)
		name="_";
	else {
		name = nv.getStep();
		if (!name.empty()) name[0]=tolower(name[0]);
		else cerr << "warning: empty note name" << endl;
	}
	return name;
}

//______________________________________________________________________________
guidonoteduration xmlpart2guido::noteDuration ( const notevisitor& nv )
{
	guidonoteduration dur(0,0);
	if (nv.getType() == kRest) {
		rational r(nv.getDuration(), fCurrentDivision*4);
		r.rationalise();
		dur.set (r.getNumerator(), r.getDenominator());
	}
	else {
		rational r = NoteType::type2rational(NoteType::xml(nv.getGraphicType()));
		if (r.getNumerator() == 0) // graphic type missing or unknown
			r.set (nv.getDuration(), fCurrentDivision*4);
		r.rationalise();
		rational tm = nv.getTimeModification();
		r *= tm;
		r.rationalise();
		dur.set (r.getNumerator(), r.getDenominator(), nv.getDots());
	}
	return dur;
}

//______________________________________________________________________________
void xmlpart2guido::newNote ( const notevisitor& nv ) 
{
	checkTiedBegin (nv.getTied());

	int octave = nv.getOctave() - 3;			// octave offset between MusicXML and GUIDO is -3
	string accident = alter2accident(nv.getAlter());
	string name = noteName(nv);
	guidonoteduration dur = noteDuration(nv);
	Sguidoelement note = guidonote::create(fTargetVoice, name, octave, dur, accident);
	add (note);

	checkTiedEnd (nv.getTied());
}

//______________________________________________________________________________
void xmlpart2guido::visitEnd ( S_note& elt ) 
{
	notevisitor::visitEnd ( elt );

	if (inChord()) return;					// chord notes have already been handled

	bool scanVoice = (notevisitor::getVoice() == fTargetVoice);
	if (!isGrace()) {
		moveMeasureTime (getDuration(), scanVoice);
		checkDelayed (getDuration());		// check for delayed elements (directions with offset)
	}
	if (!scanVoice) return;

	checkStaff(notevisitor::getStaff());

	checkVoiceTime (fCurrentMeasurePosition, fCurrentVoicePosition);

	if (notevisitor::getType() != notevisitor::kRest)
		checkStem (notevisitor::fStem);
//	checkCue(*this);    // inhibited due to poor support in guido (including crashes)
	checkGrace(*this);
	checkSlurBegin (notevisitor::getSlur());
	checkBeamBegin (notevisitor::getBeam());
	
	int pendingPops  = checkFermata(*this);
	pendingPops += checkArticulation(*this);

	vector<Sxmlelement> chord = getChord(elt);
	if (chord.size()) {
		Sguidoelement chord = guidochord::create();
		push (chord);
		pendingPops++;
	}
	newNote (*this);
	for (vector<Sxmlelement>::const_iterator iter = chord.begin(); iter != chord.end(); iter++) {
		notevisitor nv;
		xml_tree_browser browser(&nv);
		Sxmlelement note = *iter;
		browser.browse(*note);
		checkStaff(nv.getStaff());
		newNote (nv);		
	} 
	while (pendingPops--) pop();
	
	checkBeamEnd (notevisitor::getBeam());
	checkSlurEnd (notevisitor::getSlur());
	if (notevisitor::fBreathMark) {
		Sguidoelement tag = guidotag::create("breathMark");
		add(tag);
	}

    fMeasureEmpty = false;
}

//______________________________________________________________________________
// time management
//______________________________________________________________________________
void xmlpart2guido::visitStart ( S_divisions& elt ) 
{
	fCurrentDivision = (long)(*elt);
}

}

