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
#include <math.h>

#ifdef WIN32
#include <windows.h>
#endif

#include "libmusicxml.h"
#include "elements.h"

using namespace MusicXML2;
using namespace std;


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

static int randomDuration() {
	return int(pow(2.f, getrandom (5)));
}

static const char* duration2type(int dur) {
	switch (dur) {
		case 1:		return "16th";
		case 2:		return "eighth";
		case 4:		return "quarter";
		case 8:		return "half";
		case 16:	return "whole";
		default:	return "unknown type";
	}
}

static TElement measure (musicxmlfactory* f, int n) {
	TElement m = n ? factoryMeasure (f, n+1)
		: factoryMeasureWithAttributes (f, n+1, "4/4", "G", 2, getrandom(9)-4, 4);
	int length = 0;
	while (length < 16) {
		TElement note = 0;
		int duration = randomDuration();
		int remain = 16 - length;
		
		if (duration <= remain) {
			note = factoryNote(f, randomNote().c_str(), float(getrandom(3)-1), 4 + getrandom(2), 
							duration, duration2type(duration));
			length += duration;
		}
		else if (remain <= 2) {
			note = factoryNote(f, randomNote().c_str(), float(getrandom(3)-1), 4 + getrandom(2), 
							remain, duration2type(remain));
			length += remain;
		}
		if (note) factoryAddElement(f, m, note);
	}
	return m;
}

static TElement dynamic (musicxmlfactory* f, int type) {
	TElement d = factoryElement (f, k_direction);
	TElement dt = factoryElement (f, k_direction_type);
	factoryAddElement (f, dt, factoryDynamic(f, type, "below"));
	factoryAddElement (f, d, dt);
	return d;
}

static TElement lastmeasure (musicxmlfactory* f, int n) {
	TElement m = factoryMeasureWithAttributes (f, n+1, 0, 0, 0, 0, 15);
	factoryAddElement (f, m, dynamic(f, k_pp));

	TElement notes[6];
	for (int i=0; i< 5; i++) {
		notes[i] = factoryNote(f, "G", 0.f, 4, 3, "16th");
		factoryArticulation (f, notes[i], factoryElement(f, k_accent));
	}
	notes[5] = 0;
	factoryTuplet (f, 5, 4, notes);
	factoryAddElements (f, m, notes);
	for (int i=0; i< 3; i++)
		notes[i] = factoryNote(f, "E", 0.f, 4, 5, "eighth");
	notes[3] = 0;
	factoryTuplet (f, 3, 2, notes);
	factoryAddElements (f, m, notes);
	notes[3] = factoryNote(f, "E", 0.f, 4, 15, "quarter");
	notes[4] = factoryNote(f, "E", 0.f, 4, 15, "quarter");
	factoryTie (f, notes[2], notes[3]);
	factoryTie (f, notes[3], notes[4]);
	factoryNotation (f, notes[4], factoryElement(f, k_fermata));
	factoryAddElements (f, m, &notes[3]);
	factoryAddElement (f, m, factoryBarline(f, "right", "light-heavy", 0));
	return m;
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
	musicxmlfactory* f = factoryOpen();
	factoryRights (f, "Copyright X 2010", 0);
	factoryCreator (f, "anonymous", "Composer");
	factoryHeader (f, 0, "worktitle", "Movement number", "Movement title");
	factoryEncoding(f, "xmlfactory sample");
	
	factoryAddPart (f, factoryScorepart (f, "P1", "My part", 0));
	TElement part = factoryPart (f, "P1");
	for (int i = 0; i < count; i++)
		factoryAddElement (f, part, measure(f, i));
	factoryAddElement (f, part, lastmeasure(f, count));
	factoryAddPart(f, part);
	factoryPrint(f, cout);
	factoryClose (f);
	cout << endl;
    return 0;
}

