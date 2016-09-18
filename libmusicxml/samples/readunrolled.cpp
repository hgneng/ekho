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

#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"
#include "xml_tree_browser.h"
#include "unrolled_xml_tree_browser.h"

using namespace std;
using namespace MusicXML2;


class measurevisitor : 
	public visitor<S_measure>, 
	public visitor<S_part>
{
    public:
				 measurevisitor() {}
       	virtual ~measurevisitor() {}

		virtual void visitStart( S_part& elt ) {
			cout << "part " << elt->getAttributeValue("id") << endl;
		}
		virtual void visitStart( S_measure& elt ) {
			cout << "measure " << elt->getAttributeValue("number") << endl;
		}
};


//_______________________________________________________________________________
int main(int argc, char *argv[]) {
	if (argc > 1) {
		xmlreader r;
		SXMLFile file = r.read(argv[1]);
		if (file) {
			Sxmlelement elts = file->elements();
			measurevisitor mv;
			cout << ">>>>>>>>>>>> Rolled score <<<<<<<<<<<<" << endl;
			xml_tree_browser tb(&mv);
			tb.browse (*elts);
			cout << ">>>>>>>>>>>> Unrolled score <<<<<<<<<<<<" << endl;
			unrolled_xml_tree_browser utb(&mv);
			utb.browse (*elts);
		}
	}
	return 0;
}
