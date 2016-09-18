/*

  Copyright (C) 2007  Grame
  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the MusicXML Library use.
  
*/

#include <stdlib.h>
#include <string.h>
#include <iostream>

#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"
#include "xml_tree_browser.h"
#include "transposition.h"

using namespace std;
using namespace MusicXML2;

//------------------------------------------------------------------------
static void usage(char * name)
{
	cerr << "usage: " << name << " <interval> <file>" << endl;
	cerr << "       reads stdin when <file> is '-'" << endl;
	exit(1);
}

//------------------------------------------------------------------------
int main (int argc, char * argv[]) 
{
	if (argc != 3) usage(argv[0]);

	// transposing interval is readed from the command line
	int interval = atoi(argv[1]);
	char *path = argv[2];

	xmlreader r;
	SXMLFile file;
	if (strcmp(path, "-"))
		file = r.read(path);
	else
		file = r.read(stdin);
	if (file) {
		Sxmlelement elts = file->elements();
		transposition t(interval);
		xml_tree_browser tb(&t);
		tb.browse (*elts);
		file->print (cout);
		cout << endl;
		return 0;
	}
	cerr << "cannot read file \"" << path << "\"" << endl;
	return 1;
}


