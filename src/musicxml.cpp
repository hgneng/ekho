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
    this->loadPitchFile();

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
  if (this->pitchMap.find(ps->symbol) != this->pitchMap.end()) {
    cerr << ps->symbol << " pitch=" << this->pitchMap[ps->symbol] << endl;
  }

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

int Ekho::loadPitchFile() {
  cerr << "loadPitchFile...";
  string pitchFilePath;
  if (this->m_pImpl->mDict.getLanguage() == CANTONESE) {
    pitchFilePath = this->m_pImpl->mDict.mDataPath + "/jyutping.pitch";
  }

  if (pitchFilePath.empty()) {
    cerr << "no pitch file";
    return -1;
  }

  int count = 0;
  string line;
  ifstream pitchFile;
  pitchFile.open(pitchFilePath);

  if (!pitchFile.is_open()) {
    cerr << "file to open " << pitchFilePath << endl;
  }

  while (getline(pitchFile, line)) {
    int pos = line.find('=', 0);
    pitchMap[line.substr(0, pos)] = atof(line.substr(pos + 1, line.length() - pos).c_str());
    //cerr << line.substr(0, pos) << "->" << pitchMap[line.substr(0, pos)] << endl;
  }

  cerr << "finish" << endl;
  return count;
}

double Ekho::detectPitch(const short *pcm, int size, int sampleRate) {
  return 0;
/*
#ifdef ENABLE_MUSICXML
  // slide intot 0.1s chunks
  int chunkSize = sampleRate / 100;
  int chunkCount = size / chunkSize;
  std::vector<double> chunks[chunkCount];

  for (int i = 0; i < chunkCount; i++) {
    chunks[i].resize(chunkSize);
    for (int j = 0; j < chunkSize; j++) {
      chunks[i][j] = (double)pcm[i * chunkSize + j] / 32768;
    }

    double pitchYin = pitch::yin<double>(chunks[i], sampleRate);
    double pitchMpm = pitch::mpm<double>(chunks[i], sampleRate);
    double pitchPyin = pitch::pyin<double>(chunks[i], sampleRate);
    double pitchPmpm = pitch::pmpm<double>(chunks[i], sampleRate);
    double pitchSwipe = pitch::swipe<double>(chunks[i], sampleRate);

    cerr << "pitch: yin=" << pitchYin
        << ", mpm=" << pitchMpm
        << ", Pyin=" << pitchPyin
        << ", pitchPmpm=" << pitchPmpm
        << ", pitchSwipe=" << pitchSwipe
        << endl;
  }

  return 0;
#else
#endif
*/
}

}  // end of namespace ekho