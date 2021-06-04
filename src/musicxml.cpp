#include "config.h"

#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#include <stdlib.h>
#include <iostream>

#include "character.h"
#include "ekho_impl.h"
#include "ekho.h"
#include "elements.h"
#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"

using namespace std;
using namespace MusicXML2;

namespace ekho {
// gcc musicxml.cpp -g -I../libmusicxml/src/elements -I../libmusicxml/src/lib
// -I../libmusicxml/src/visitors -I../libmusicxml/src/files
// -I../libmusicxml/src/parser ../libmusicxml/libmusicxml2.a -lstdc++ && ./a.out
// demo.xml
void Ekho::singMusicXml(const string filepath) {
  xmlreader r;
  SXMLFile file = r.read(filepath.c_str());

#ifdef HAVE_PULSEAUDIO
  if (this->m_pImpl->initSound() < 0) {
    cerr << "Fail to init sound." << endl;
    return;
  }
#endif

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
             << ",octave=" << octave << ",duration=" << duration << ")" << endl;
        this->singCharacter(Character(lyric));
      }

      ++note;
      if (note != st->end()) {
        note = st->find(k_note, note);
      }
    }

    cout << endl;
  }
}

void Ekho::singCharacter(const Character &c) {
  cerr << "singCharacter: " << c.getUtf8() << endl;
  PhoneticSymbol *ps = this->m_pImpl->mDict.lookup(c);
  int size = 0;
  const char *pcm = ps->getPcm(this->m_pImpl->mDict.mVoiceFile, size);
#ifdef HAVE_PULSEAUDIO
  int error;
  int ret = pa_simple_write(this->m_pImpl->stream, pcm, size * 2, &error);
  //cerr << "size: " << size << ", error:" << error << endl;
#endif
}
}  // end of namespace ekho