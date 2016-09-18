/*

  Copyright (C) 2003-2008  Grame
  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the MusicXML Library use.
*/
	
#ifdef WIN32
# pragma warning (disable : 4786)
#endif

#include <string.h>
#include <iostream>

#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"
#include "midicontextvisitor.h"
#include "unrolled_xml_tree_browser.h"

using namespace std;
using namespace MusicXML2;

//_______________________________________________________________________________
class mymidiwriter : public midiwriter {
	public:
				 mymidiwriter() {}
		virtual ~mymidiwriter() {}
		
		virtual void startPart (int instrCount) 
			{ cout << "startPart with " << instrCount << " instrument(s)" << endl; }
		virtual void newInstrument (std::string instrName, int chan=-1)
			{ cout << "newInstrument \"" << instrName << "\" on chan " << chan << endl; }
		virtual void endPart (long date)
			{ cout << date << " endPart" << endl; }

		virtual void newNote (long date, int chan, float pitch, int vel, int dur)
			{ cout << date << " newNote [" << chan << "," << pitch << "," << vel << "," << dur << "]" << endl; }
		virtual void tempoChange (long date, int bpm)
			{ cout << date << " tempoChange " << bpm << endl; }
		virtual void pedalChange (long date, pedalType t, int value)
			{ cout << date << " pedalChange type " << t << " val " << value << endl; }

		virtual void volChange (long date, int chan, int vol)
			{ cout << date << " volChange chan  " << chan << " vol " << vol << endl; }
		virtual void bankChange (long date, int chan, int bank)
			{ cout << date << " bankChange chan " << chan << " bank " << bank << endl; }
		virtual void progChange (long date, int chan, int prog)
			{ cout << date << " progChange chan " << chan << " prog " << prog << endl; }
};

//_______________________________________________________________________________
int main(int argc, char *argv[]) {
	char * file = argv[1];
	xmlreader r;
	SXMLFile xmlfile;
	if ((argc > 1) && strcmp(file, "-"))
		xmlfile = r.read(file);
	else
		xmlfile = r.read(stdin);
	if (xmlfile) {
		Sxmlelement st = xmlfile->elements();
		if (st) {
			mymidiwriter writer;
			midicontextvisitor v(480, &writer);
			unrolled_xml_tree_browser browser(&v);
			browser.browse(*st);
		}
	}
	return 0;
}
