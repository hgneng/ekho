
#ifdef WIN32
# pragma warning (disable : 4786)
#endif

#include <algorithm>
/*

  Copyright (C) 2003-2008  Grame
  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the MusicXML Library use.
*/

#include <iostream>

#include "elements.h"
#include "factory.h"
#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"

using namespace std;
using namespace MusicXML2;

#define debug	0

//_______________________________________________________________________________
class predicate {
	public:
		int fType;
			 predicate(int type) : fType(type) {}
	virtual ~predicate() {}
	virtual bool operator () (const Sxmlelement elt) const { 
		return elt->getType() == fType;
	}
};

//_______________________________________________________________________________
static void count(Sxmlelement elt, int type)
{
	predicate p(type);
	cerr << "  count of type " << type << " elements: " 
		 << count_if(elt->begin(), elt->end(), p) << endl;
}

//_______________________________________________________________________________
static void test1(Sxmlelement elt)
{
	cerr << "test1: iterate thru the tree" << endl;
	ctree<xmlelement>::iterator iter = elt->begin();
	cerr << "=> test1: iterate thru the tree" << endl;

	while (iter != elt->end()) {
		Sxmlelement xml = *iter;
		if (xml)
			cerr << "  element type " << xml->getType() 
				 << " - " << xml->getName()
				 << " - size: " << xml->size() << endl;
		else
			cerr << "iterate thru unknown element type " << endl;
		iter++;
	}

}

//_______________________________________________________________________________
static void test2(Sxmlelement elt)
{
	cerr << "test2: erasing all the par measures" << endl;
	ctree<xmlelement>::iterator next, iter = elt->begin();
	int measure=1;
	while (iter != elt->end()) {
		Sxmlelement xml = *iter;
		next = iter;
		next++;
		assert (xml);
		if (xml->getType() == k_software) {
				next = elt->erase(iter);			
		}
		else if (xml->getType() == k_measure) {
			if (!(measure & 1)) {
				next = elt->erase(iter);
			}
			measure++;
		}
		iter = next;
	}
}

//_______________________________________________________________________________
static void test3(Sxmlelement elt)
{
	cerr << "test3: insert a note before the par notes" << endl;
	ctree<xmlelement>::iterator next, iter = elt->begin();
	int note=1;
	while (iter != elt->end()) {
		Sxmlelement xml = *iter;
		assert (xml);
		if (xml->getType() == k_note) {
			if (!(note & 1)) {
				Sxmlelement note = factory::instance().create(k_note);
				iter = elt->insert(iter, note);
				iter++;
			}
			note++;
		}
		iter++;
	}
}

//_______________________________________________________________________________
int main(int argc, char *argv[]) {
#if debug
	char *path = "rm.xml";
	argc = 2;
#else
	char *path = argv[1];
#endif
	if (argc > 1) {
		xmlreader r;
		SXMLFile file = r.read(path);
		if (file) {
			Sxmlelement st = file->elements();
			if (st) {
				test1(st);
				count(st, k_measure);
				count(st, k_note);

				test2(st);
				count(st, k_measure);
				count(st, k_note);

				test3(st);
				count(st, k_measure);
				count(st, k_note);

				file->print (cout);
				cout << endl;		
			}
		}
	}
	return 0;
}
