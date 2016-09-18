/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifdef VC6
# pragma warning (disable : 4786)
#endif

#include <algorithm>
#include <vector>
#include "unrolled_xml_tree_browser.h"

using namespace std;
namespace MusicXML2
{

//______________________________________________________________________________
void unrolled_xml_tree_browser::browse (xmlelement& t)				{ t.acceptIn(*this); }

void unrolled_xml_tree_browser::forwardBrowse( xmlelement& elt )
{
	enter(elt);
	ctree<xmlelement>::literator iter;
	for (iter = elt.lbegin(); iter != elt.lend(); iter++)
		forwardBrowse(**iter);
	leave(elt);
}

void unrolled_xml_tree_browser::visitStart( Sxmlelement& elt)
{
	bool forward = fForward;
	if (forward) enter(*elt);
	ctree<xmlelement>::literator iter;
	for (iter = elt->lbegin(); iter != elt->lend(); iter++)
		browse(**iter);
	if (forward) leave(*elt);
}

//______________________________________________________________________________
void unrolled_xml_tree_browser::visitStart( S_ending& elt)
{
	// first get ending attributes
	int number = elt->getAttributeIntValue("number", 1);
	string type = elt->getAttributeValue("type");

	if (type == "start") {
		// it is assumed that ending starting just after a repeat bar denotes a continuation
		// of the previous repeat, thus it is ignored whatever value it has
		if (fSectionIndex > 1) fExpectedRound.current = number;
	}
	else if ((type == "stop") || (type == "discontinue")) {
		fExpectedRound.next = kNoInstruction;			// no specific round expected at next iteration
	}
}


//______________________________________________________________________________
void unrolled_xml_tree_browser::visitStart( S_repeat& elt)
{
	string direction = elt->getAttributeValue("direction");
	int times = elt->getAttributeIntValue("times", 1);

	if (direction == "backward") {
		fRepeatMap[elt]++;		// intended to catch possible loop
		// check first for times to play the repeat section and for possible loop
		if ((fRound.current <= times) && (fRepeatMap[elt] <= times)) {
			fNextIterator = fForwardRepeat;			// jump to fForwardRepeat at next iteration
			fRound.next = fRound.current + 1;			// increments the current round at next iteration
			fExpectedRound.next = fRound.current + 1;	// and adjust the corresponding round for next iteration
		}
		else {											// jump out of the repeat section:
			fRound.next = 1;							// next round will be 1
			fExpectedRound.next = kNoInstruction;		// no specific round expected at next iteration
			fSectionIndex = 0;							// and starts a new section
		}
	}
	else if (direction == "forward") {
		// current measure iterator will be stored in the fForwardRepeat location
		fStoreIterator = &fForwardRepeat;
	}
}

//______________________________________________________________________________
void unrolled_xml_tree_browser::visitStart( S_measure& elt)
{
	Sxmlelement xml = elt;
	// first adjust the browser state
	if (fExpectedRound.next != kUndefined) {
		fExpectedRound.current = fExpectedRound.next;
		fExpectedRound.next = kUndefined;
	}
	if (fRound.next > 0) {
		fRound.current = fRound.next;
		fRound.next = kUndefined;
	}
	if (fJump.next) {
		fJump.current = fJump.next;
		fJump.next = kNoJump;
	}
	fSectionIndex++;

	// next visit the measure
	visitStart(xml);
	// and according to the current state, forward the visit to the embedded visitor
	if (fJump.current != kToCoda) {		// check that we're not looking for the coda sign
										// and check that we're playing the correct round
		if ((fExpectedRound.current == kNoInstruction) || (fRound.current == fExpectedRound.current))
			forwardBrowse (*elt);
	}
}

//______________________________________________________________________________
void unrolled_xml_tree_browser::reset()
{
	fRepeatMap.clear();								// clear the map of backward repeat measures
	fSectionIndex = 0;
	fRound.current = 1;	fRound.next = kNoInstruction;	// this is the first time we visit the part
	fExpectedRound.current = kNoInstruction;			// and there is no specific instruction for the round
	fExpectedRound.next = kUndefined;					// and there is no specific instruction for the next iteration
}

//______________________________________________________________________________
// control all of the tree browsing is in charge of the S_part visit
// it assumes that the score is a partwise score but the strategy could
// equalli work on a timewise score, provided that the jump information
// is consistent across the different parts.
//
// Jumps are under control of fNextIterator which could be modified by the
// measure visit. Anchor points for future jumps (like segno) are stored
// at S_part level using fStoreIterator pointer.
//
// visit methods can use the fStoreIterator pointer to store the current iterator
// to any ctree<xmlelement>::literator. Using fStoreDelay allows for delayed stores.
//
void unrolled_xml_tree_browser::visitStart( S_part& elt)
{
	// first initializes the iterators used to broswe the tree
	// segno and coda are initialized to the end of the measures list
	fEndIterator = elt->elements().end();
	fSegnoIterator= fCodaIterator = fEndIterator;
	// stores the first measures and makes a provision for the forward repeat location
	ctree<xmlelement>::literator iter = elt->elements().begin();
	fFirstMeasure = fForwardRepeat = iter;
	fStoreIterator = 0;
	fStoreDelay = 0;

	reset();

	enter(*elt);			// normal visit of the part (pass thru)
	fForward = false;
	// while we're not at the end location (elements().end() is checked for safety reasons only)
	while ((iter != fEndIterator) && (iter != elt->elements().end())) {
		fNextIterator = iter;
		fNextIterator++;				// default value for next iterator is the next measure
		browse(**iter);					// browse the measure
		if (fStoreIterator) {			// check if we need to store the current iterator
			if (fStoreDelay == 0) {
				*fStoreIterator = iter;
				fStoreIterator = 0;
			}
			else fStoreDelay--;			// this is actually a delayed store
		}
		iter = fNextIterator;			// switch to next iterator (which may be changed by the measure visit)
	}
	leave(*elt);			// normal visit of the part (pass thru)
}

//______________________________________________________________________________
void unrolled_xml_tree_browser::visitStart( S_sound& elt)
{
	// we first collect possible locations of the score
	if (!elt->getAttributeValue("segno").empty())	{ fStoreIterator = &fSegnoIterator; }
	if (!elt->getAttributeValue("coda").empty())	{ fJump.current = kNoJump; fStoreIterator = &fCodaIterator; }
	if (!elt->getAttributeValue("fine").empty())	{ fStoreDelay = 1; fStoreIterator = &fEndIterator; }

	// next we look for possible jumps
	// note that multiple jumps in a single sound element are not supported
	if (elt->getAttributeValue("dacapo") == "yes") {
		if (fJump.current == kNoJump) {
			fNextIterator = fFirstMeasure;
			fJump.current = kDaCapo;
			reset();
		}
	}
	else if (!elt->getAttributeValue("dalsegno").empty()) {
		if (fJump.current == kNoJump) {
			fNextIterator = fSegnoIterator;
			fJump.current = kDalSegno;
			reset();
		}
	}
	else if (!elt->getAttributeValue("tocoda").empty()) {
		if ((fJump.current == kDaCapo) || (fJump.current == kDalSegno)) {
			if (fCodaIterator != fEndIterator)	// whern the coda sign has already been seen, jump to coda
				fNextIterator = fCodaIterator;	// otherwise set the state to kToCoda
			else fJump.next = kToCoda;			// it inhibits the visit forwarding until we find the code sign
			reset();
		}
	}
}

}
