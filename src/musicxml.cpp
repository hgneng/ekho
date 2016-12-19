#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#include <stdlib.h>
#include <iostream>

#include "elements.h"
#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"

using namespace std;
using namespace MusicXML2;

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; i++) {
    xmlreader r;
    SXMLFile file = r.read(argv[i]);

    if (file) {
      Sxmlelement st = file->elements();
      ctree<xmlelement>::iterator note = st->find(k_note);

      while (note != st->end()) {
        ctree<xmlelement>::branchs branchs = note->elements();
        vector<Sxmlelement>::iterator elem = branchs.begin();

        vector<Sxmlelement>::iterator pitch = branchs.begin();
        vector<Sxmlelement>::iterator duration = branchs.begin();

        string lyric = "";
        for (; elem != branchs.end(); elem++) {
          string name = (*elem)->getName();
          cout << name;

          if (name == "pitch") {
            pitch = elem;
          } else if (name == "lyric") {
            ctree<xmlelement>::iterator text = (*elem)->find(k_text);

            lyric = text->getValue();
            cout << "lyric:" << lyric << endl;
          }
        }

        if (!lyric.empty()) {
          cout << lyric << "(" << (*pitch)->getValue() << ")" << endl;
        }

        ++note;
        if (note != st->end()) {
          note = st->find(k_note, note);
        }
      }

      cout << endl;
    }
  }
  return 0;
}
