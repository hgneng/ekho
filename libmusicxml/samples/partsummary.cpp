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
#include "partsummary.h"
#include "smartlist.h"
#include "xml_tree_browser.h"

using namespace std;
using namespace MusicXML2;

//_______________________________________________________________________________
class mypartsummary : public partsummary
{
	public:
		virtual void visitEnd ( S_part& elt);
};


//_______________________________________________________________________________
void mypartsummary::visitEnd ( S_part& elt)
{
	cout << "summary for part " << elt->getAttributeValue("id") << endl;
	cout << "  staves count : " << countStaves() << endl;

	smartlist<int>::ptr voices;
	smartlist<int>::ptr staves = getStaves();
	for (vector<int>::const_iterator i = staves->begin(); i != staves->end(); i++) {
		cout << "    staff \"" << *i << "\": " << getStaffNotes(*i) << " notes - "
			 << countVoices(*i) << " voices [";

		bool sep = false;
		voices = getVoices(*i);
		for (vector<int>::const_iterator v = voices->begin(); v != voices->end(); v++) {
			if (sep) cout << ", ";
			else sep = true;
			cout << *v << ":" << getVoiceNotes(*i, *v);
		}
		cout << "]" << endl;
	}
	cout << "  total voices : " << countVoices() << endl;
	voices = getVoices();
	for (vector<int>::const_iterator i = voices->begin(); i != voices->end(); i++) {
		cout << "    voice \"" << *i << "\": " << getVoiceNotes(*i) << " notes - ";
		staves = getStaves(*i);
		cout << staves->size() << (staves->size() > 1 ? " staves" :" staff") << " [";
		bool sep = false;
		for (vector<int>::const_iterator s = staves->begin(); s != staves->end(); s++) {
			if (sep) cout << ", ";
			else sep = true;
			cout << *s ;
		}
		cout << "] main staff: " << getMainStaff(*i) << endl;		
	}
}

//_______________________________________________________________________________
int main(int argc, char *argv[]) 
{
	const char * file = argc > 1 ? argv[1] : "-";
	xmlreader r;
	SXMLFile xmlfile;
	if (strcmp(file, "-"))
		xmlfile = r.read(file);
	else
		xmlfile= r.read(stdin);
	if (xmlfile) {
		Sxmlelement elt = xmlfile->elements();
		if (elt) {
			mypartsummary nv;
			xml_tree_browser browser(&nv);
			browser.browse(*elt);
		}
	}
	else cerr << "error reading \"" << file << "\"" << endl;
	return 0;
}
