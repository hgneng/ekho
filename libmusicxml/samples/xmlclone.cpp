/*

  Copyright (C) 2003-2008  Grame
  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the MusicXML Library use.
*/

#ifdef WIN32
# pragma warning (disable : 4786)
#endif

#include <stdlib.h>
#include <string.h>
#include <iostream>

#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"
#include "clonevisitor.h"
#include "unrolled_clonevisitor.h"
#include "xml_tree_browser.h"
#include "unrolled_xml_tree_browser.h"

using namespace std;
using namespace MusicXML2;

//_______________________________________________________________________________
void usage() {
	cerr << "usage: xmlclone [options]  <musicxml file>" << endl;
	cerr << "option: --unroll creates an unrolled version of the score" << endl;
	exit(1);
}

//_______________________________________________________________________________
int main(int argc, char *argv[]) {
	bool unroll = false;
	char * name = argv[1];
	if (argc == 3) {
		if (!strcmp(argv[1], "--unroll")) {
			unroll = true;
			name = argv[2];
		}
		else usage();
	}
	else if (argc != 2) usage();

	xmlreader r;
	SXMLFile file = r.read(name);
	if (file) {
		Sxmlelement elts = file->elements();
		if (unroll) {
			unrolled_clonevisitor uv;
			unrolled_xml_tree_browser tb(&uv);
			tb.browse (*elts);
			elts = uv.clone();
		}
		else {
			clonevisitor cv;
			xml_tree_browser tb(&cv);
			tb.browse (*elts);
			elts = cv.clone();
		}
		file->set (elts);
		file->print (cout);
		cout << endl;
	}
	return 0;
}
