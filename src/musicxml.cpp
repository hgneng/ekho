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

// gcc musicxml.cpp -g -I../libmusicxml/src/elements -I../libmusicxml/src/lib
// -I../libmusicxml/src/visitors -I../libmusicxml/src/files
// -I../libmusicxml/src/parser ../libmusicxml/libmusicxml2.a -lstdc++ && ./a.out
// demo.xml
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
        string lyric = "";
        string step = "";
        string alter = "";
        string octave = "";
        string duration = "";

        for (; elem != branchs.end(); elem++) {
          string name = (*elem)->getName();
          // cout << name;

          if (name == "pitch") {
            ctree<xmlelement>::iterator step_elem = (*elem)->find(k_step);
            if (step_elem != (*elem)->end()) {
              step = step_elem->getValue();
            }

            ctree<xmlelement>::iterator alter_elem = (*elem)->find(k_alter);
            if (alter_elem != (*elem)->end()) {
              alter = alter_elem->getValue();
            }

            ctree<xmlelement>::iterator octave_elem = (*elem)->find(k_octave);
            if (octave_elem != (*elem)->end()) {
              octave = octave_elem->getValue();
            }
          } else if (name == "duration") {
            duration = (*elem)->getValue();
          } else if (name == "lyric") {
            ctree<xmlelement>::iterator text = (*elem)->find(k_text);
            lyric = text->getValue();
          }
        }

        if (!lyric.empty()) {
          cout << lyric << "(step=" << step << ",alter=" << alter
               << ",octave=" << octave << ",duration=" << duration << ")"
               << endl;
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
