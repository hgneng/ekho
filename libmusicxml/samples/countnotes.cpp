/*

  Copyright (C) 2008  Grame
  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the MusicXML Library use.
*/

#ifdef WIN32
# pragma warning (disable : 4786)
#endif

#include <iostream>

#include "typedefs.h"
#include "visitor.h"
#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"
#include "xml_tree_browser.h"

using namespace std;
using namespace MusicXML2;

#define use_visitor

//_______________________________________________________________________________
class countnotes : 
	public visitor<S_note>
{
	public:
		int	fCount;

				 countnotes() : fCount(0)	{}
		virtual ~countnotes() {}
		void visitStart( S_note& elt )		{ fCount++; }
};

//_______________________________________________________________________________
struct predicate {
	bool operator () (const Sxmlelement elt) const { 
		return elt->getType() == k_note;
	}
};

//_______________________________________________________________________________
static int read(FILE * fd)
{
	int count = 0;
	xmlreader r;
	SXMLFile file = r.read(fd);
	if (file) {
		Sxmlelement elt = file->elements();
		if (elt) {
#ifdef use_visitor
			countnotes v;
			xml_tree_browser browser(&v);
			browser.browse(*elt);
			count = v.fCount;
#else // use iterator
			predicate p;
			count = count_if(elt->begin(), elt->end(), p);
#endif
		}
	}
	else count = -1;
	return count;
}

//_______________________________________________________________________________
int main(int argc, char *argv[]) {
	if (argc == 1) {
		int count = read (stdin);
		if (count >= 0) cout << count << " notes" << endl;
	}
	else for (int i = 1; i < argc; i++) {
		FILE * fd = fopen(argv[i], "r");
		if (fd) {
			int count = read (fd);
			if (count >= 0) cout << argv[i] << ": " << count << " notes" << endl;
			fclose(fd);
		}
		else cerr << "error opening \"" << argv[i] << "\"" << endl;
	}
	return 0;
}
