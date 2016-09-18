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
#include <iostream>

#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"

using namespace std;
using namespace MusicXML2;

//#define STRREAD

//_______________________________________________________________________________
#ifdef STRREAD
static char * read(const char* file)
{
	FILE * fd = fopen(file, "rb");
	if (!fd) return 0;
	fseek(fd, 0, SEEK_END);
	long size = ftell (fd);
	char * buff = (char*)malloc(size+1);
	fseek(fd, 0, SEEK_SET );
	fread(buff, size, 1, fd);
	buff[size] = 0;
	fclose(fd);
	return buff;
}
#endif

//_______________________________________________________________________________
int main(int argc, char *argv[]) 
{
	for (int i=1; i<argc; i++) {
		xmlreader r;
#ifdef STRREAD
		char * buff = read(argv[i]);
		SXMLFile file = r.readbuff(buff);
#else
		SXMLFile file = r.read(argv[i]);
#endif
		if (file) {
			Sxmlelement st = file->elements();
			file->print(cout);
			cout << endl;
		}
	}
	return 0;
}
