
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

//_______________________________________________________________________________
class EXP countnotes : 
	public visitor<S_note>
{
	public:
		int	fCount;
		void visitStart( S_note& elt )	{
}

//_______________________________________________________________________________
int main(int argc, char *argv[]) {
	if (argc > 1) {
		xmlreader r;
		SXMLFile file = r.read(argv[1]);
		if (file) {
			Sxmlelement elt = file->elements();
			if (elt) {
				notevisitor nv;
				xml_tree_browser browser(&nv);
				browser.browse(*elt);
			}
		}
		else cerr << "error reading \"" << argv[1] << "\"" << endl;
	}
	return 0;
}
