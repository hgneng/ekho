#include "config.h"

#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#include <stdlib.h>
#include <iostream>

#include "character.h"
#include "ekho_impl.h"
#include "audio.h"
#include "ekho.h"
#include "elements.h"
#include "xml.h"
#include "xmlfile.h"
#include "xmlreader.h"

#ifdef ENABLE_MUSICXML
#include <pitch_detection.h>
#endif

using namespace std;
using namespace MusicXML2;

namespace ekho {
// gcc musicxml.cpp -g -I../libmusicxml/src/elements -I../libmusicxml/src/lib
// -I../libmusicxml/src/visitors -I../libmusicxml/src/files
// -I../libmusicxml/src/parser ../libmusicxml/libmusicxml2.a -lstdc++ && ./a.out
// demo.xml
void Ekho::singMusicXml(const string xmlFile, const string outputFile) {
  xmlreader r;
  SXMLFile file = r.read(xmlFile.c_str());

  if (!outputFile.empty()) {
    SF_INFO sfinfo;
    memcpy(&sfinfo, &this->m_pImpl->mDict.mSfinfo, sizeof(SF_INFO));
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    sndFile = sf_open(outputFile.c_str(), SFM_WRITE, &sfinfo);
  }

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
      int duration = 0;

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
          duration = atoi((*elem)->getValue().c_str());
        } else if (name == "lyric") {
          ctree<xmlelement>::iterator text = (*elem)->find(k_text);
          lyric = text->getValue();
        }
      }

      cout << lyric << "(step=" << step << ",alter=" << alter
           << ",octave=" << octave << ",duration=" << duration << ")" << endl;
      if (!lyric.empty()) {
        this->singCharacter(Character(lyric), duration);
      } else {
        this->singSilence(duration);
      }

      ++note;
      if (note != st->end()) {
        note = st->find(k_note, note);
      }
    }

    cout << endl;
  }

  if (!outputFile.empty()) {
    sf_close(sndFile);
  }
}

/* 降b大调，2个降号，<fifths>-2</fifths>
  <part id="P1">
    <measure number="1" width="0.00">
      <print>
        <system-layout>
          <system-margins>
            <left-margin>-0.00</left-margin>
            <right-margin>0.00</right-margin>
            </system-margins>
          <top-system-distance>105.00</top-system-distance>
          </system-layout>
        </print>
      <attributes>
        <divisions>12</divisions>
        <key>
          <fifths>-2</fifths>
          </key>
*/

void Ekho::singSilence(int duration) {
  cerr << "silent: " << duration << endl;

  float targetSeconds = (float)duration * 120 / 24 / this->musicxmlMinuteRate;
  int size = (int)(this->m_pImpl->mDict.mSfinfo.samplerate * targetSeconds) * 2;
  char *pcm = new char[size];
  memset(pcm, 0, size);
  if (this->sndFile) {
    sf_writef_short(this->sndFile, (const short*)pcm, size / 2);
  } else {
#ifdef HAVE_PULSEAUDIO
    int error;
    int ret = pa_simple_write(this->m_pImpl->stream, pcm, size, &error);
#endif
  }
}

void Ekho::singCharacter(const Character &c, int duration) {
  cerr << "singCharacter: " << c.getUtf8() << endl;
  PhoneticSymbol *ps = this->m_pImpl->mDict.lookup(c);
  int size = 0;
  const char *pcm = ps->getPcm(this->m_pImpl->mDict.mVoiceFile, size);
  int size2 = 0;
  const char *pcm2 = this->convertDuration(pcm, size, duration, size2);
  cerr << "size=" << size << ", size2=" << size2 << endl;
  if (this->sndFile) {
    sf_writef_short(this->sndFile, (const short*)pcm2, size2 / 2);
  } else {
#ifdef HAVE_PULSEAUDIO
    int error;
    int ret = pa_simple_write(this->m_pImpl->stream, pcm2, size2, &error);
    //cerr << "size: " << size << ", error:" << error << endl;
#endif
  }
  delete[] pcm2;
  pcm2 = NULL;
}

// caller should delete return pcm array.
char* Ekho::convertDuration(const char *pcm, int size,
    int duration, int &convertedSize) {
  // suppose total duraion is 48. duration of 1/16 note is 3.
  // suppose speed is 120 * 1/4 per minute
  // music note duration seconds = 60s / 120 / 12 * duration 
  // = duration / 24 (s)

  // compute current pcm time
  float sourceSeconds = (float)size / 2 / this->m_pImpl->mDict.mSfinfo.samplerate;
  float targetSeconds = (float)duration * 120 / 24 / this->musicxmlMinuteRate;

  int sampleRate = this->m_pImpl->mDict.mSfinfo.samplerate;
  this->detectPitch((const short*)pcm, size / 2, sampleRate);

  Audio *audio = new Audio();
  audio->initProcessor(sampleRate, 1);
  audio->setTempoFloat(sourceSeconds / targetSeconds);
  audio->writeShortFrames((short*)pcm, size / 2);
  int targetSize = size * targetSeconds / sourceSeconds;
  short *targetPcm = new short[targetSize / 2];
  convertedSize = 2 * audio->readShortFrames(targetPcm, targetSize / 2);
  cerr << "sourceSeconds=" << sourceSeconds << ", targetSeconds="
    << targetSeconds << ", convertedSize=" << convertedSize << endl;
  delete audio;
  audio = NULL;

  return (char*)targetPcm;
}

double Ekho::detectPitch(const short *pcm, int size, int samepleRate) {
#ifdef ENABLE_MUSICXML
  std::vector<double> audioBuffer(size);
  for (int i = 0; i < size; i++) {
    audioBuffer[i] = (double)pcm[i] / 32768;
  }

  double pitchYin = pitch::yin<double>(audioBuffer, samepleRate);
  double pitchMpm = pitch::mpm<double>(audioBuffer, samepleRate);
  double pitchPyin = pitch::pyin<double>(audioBuffer, samepleRate);
  double pitchPmpm = pitch::pmpm<double>(audioBuffer, samepleRate);
  double pitchSwipe = pitch::swipe<double>(audioBuffer, samepleRate);

  cerr << "pitch: yin=" << pitchYin
      << ", mpm=" << pitchMpm
      << ", Pyin=" << pitchPyin
      << ", pitchPmpm=" << pitchPmpm
      << ", pitchSwipe=" << pitchSwipe
      << endl;

  return pitchYin;
#else
  return 0;
#endif
}

}  // end of namespace ekho