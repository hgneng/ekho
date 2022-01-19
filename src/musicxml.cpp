#include "config.h"

#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cctype>

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
#ifdef ENABLE_MUSICXML
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
    this->loadPianoPitch();
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
      string accidental = "";
      string pitch = "";
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
        } else if (name == "accidental") {
          accidental = (*elem)->getValue();
        }
      }

      if (step.length() > 0) {
        string stepLower = step;
        stepLower[0] = tolower(step[0]);
        pitch = stepLower +
            (accidental == "sharp" ? "#" : (accidental == "flat" ? "b" : "")) +
            octave;
      }
      cerr << lyric << "(step=" << step << ",alter=" << alter <<
           ",octave=" << octave << ",duration=" << duration <<
           ",accidental=" << accidental <<
           ",pitch=" << pitch << ")" << endl;

      if (!lyric.empty()) {
        this->singCharacter(Character(lyric), duration, pitch);
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
#endif
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
    pa_simple_write(this->m_pImpl->stream, pcm, size, &error);
#endif
  }
}


/**
 * suppose total duraion is 48. duration of 1/16 note is 3.
 * suppose speed is 120 * 1/4 per minute
 * music note duration seconds = 60s / 120 / 12 * duration 
 * = duration / 24 (s)
 * 
 * pitch is in "c#4" format
 */ 
void Ekho::singCharacter(const Character &c, int duration, string pitch) {
  cerr << "singCharacter: " << c.getUtf8() << endl;
  PhoneticSymbol *ps = this->m_pImpl->mDict.lookup(c);
  int size = 0;
  const char *pcm = ps->getPcm(this->m_pImpl->mDict.mVoiceFile, size);
  int size2 = 0;

  // compute current pcm time
  int sampleRate = this->m_pImpl->mDict.mSfinfo.samplerate;
  float sourceSeconds = (float)size / 2 / sampleRate;
  float targetSeconds = (float)duration * 120 / 24 / this->musicxmlMinuteRate;

  float pitchFactor = 1;
  if (this->pitchMap.find(ps->symbol) != this->pitchMap.end()) {
    double sourcePitch = this->pitchMap[ps->symbol];
    double targetPitch = this->pianoPitchMap[pitch];
    pitchFactor = targetPitch / sourcePitch;
  }

  const char *pcm2 = this->convertDurationAndPitch(
      pcm, size, sourceSeconds / targetSeconds,
      pitchFactor, size2);
  cerr << "pitch=" << pitch << ", pitchFactor=" << pitchFactor <<
      ",size=" << size << ", size2=" << size2 << endl;
  if (this->pitchMap.find(ps->symbol) != this->pitchMap.end()) {
    cerr << ps->symbol << " pitch=" << this->pitchMap[ps->symbol] << endl;
  }

  if (this->sndFile) {
    sf_writef_short(this->sndFile, (const short*)pcm2, size2 / 2);
  } else {
#ifdef HAVE_PULSEAUDIO
    int error;
    /*int ret =*/ pa_simple_write(this->m_pImpl->stream, pcm2, size2, &error);
    //cerr << "size: " << size << ", error:" << error << endl;
#endif
  }
  delete[] pcm2;
  pcm2 = NULL;
}

// caller should delete return pcm array.
char* Ekho::convertDurationAndPitch(const char *pcm, int size,
    float tempo, float pitch, int &convertedSize) {
  Audio *audio = new Audio();
  int sampleRate = this->m_pImpl->mDict.mSfinfo.samplerate;
  audio->initProcessor(sampleRate, 1);
  audio->setTempoFloat(tempo);
  audio->setPitchFloat(pitch);
  audio->writeShortFrames((short*)pcm, size / 2);
  int targetSize = size / tempo;
  short *targetPcm = new short[targetSize / 2];
  convertedSize = 2 * audio->readShortFrames(targetPcm, targetSize / 2);
  cerr << "tempo=" << tempo << ", pitch="
    << pitch << ", convertedSize=" << convertedSize << endl;
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

void Ekho::loadPianoPitch() {
  this->pianoPitchMap = {
    {"a0", 27.5000000000},
    {"a#0", 29.1352350949},
    {"bb0", 29.1352350949},
    {"b0", 30.8677063285},
    {"c1", 32.7031956626},
    {"c#1", 34.6478288721},
    {"db1", 34.6478288721},
    {"d1", 36.7080959897},
    {"d#1", 38.8908729653},
    {"eb1", 38.8908729653},
    {"e1", 41.2034446141},
    {"f1", 43.6535289291},
    {"f#1", 46.2493028389},
    {"gb1", 46.2493028389},
    {"g1", 48.9994294977},
    {"g#1", 51.9130871975},
    {"ab1", 51.9130871975},
    {"a1", 55.0000000000},
    {"a#1", 58.2704701898},
    {"bb1", 58.2704701898},
    {"b1", 61.7354126570},
    {"c2", 65.4063913251},
    {"c#2", 69.2956577442},
    {"db1", 69.2956577442},
    {"d2", 73.4161919793},
    {"d#2", 77.7817459305},
    {"eb2", 77.7817459305},
    {"e2", 82.4068892282},
    {"f2", 87.3070578582},
    {"f#2", 92.4986056779},
    {"gb2", 92.4986056779},
    {"g2", 97.9988589954},
    {"g#2", 103.8261743950},
    {"ab2", 103.8261743950},
    {"a2", 110.0000000000},
    {"a#2", 116.5409403795},
    {"bb2", 116.5409403795},
    {"b2", 123.4708253140},
    {"c3", 130.8127826503},
    {"c#3", 138.5913154884},
    {"db3", 138.5913154884},
    {"d3", 146.8323839587},
    {"d#3", 155.5634918610},
    {"eb3", 155.5634918610},
    {"e3", 164.8137784564},
    {"f3", 174.6141157165},
    {"f#3", 184.9972113558},
    {"gb3", 184.9972113558},
    {"g3", 195.9977179909},
    {"g#3", 207.6523487900},
    {"ab3", 207.6523487900},
    {"a3", 220.0000000000},
    {"a#3", 233.0818807590},
    {"bb3", 233.0818807590},
    {"b3", 246.9416506281},
    {"c4", 261.6255653006},
    {"c#4", 277.1826309769},
    {"db4", 277.1826309769},
    {"d4", 293.6647679174},
    {"d#4", 311.1269837221},
    {"eb4", 311.1269837221},
    {"e4", 329.6275569129},
    {"f4", 349.2282314330},
    {"f#4", 369.9944227116},
    {"gb4", 369.9944227116},
    {"g4", 391.9954359817},
    {"g#4", 415.3046975799},
    {"ab4", 415.3046975799},
    {"a4", 440.0000000000},
    {"a#4", 466.1637615181},
    {"bb4", 466.1637615181},
    {"b4", 493.8833012561},
    {"c5", 523.2511306012},
    {"c#5", 554.3652619538},
    {"db5", 554.3652619538},
    {"d5", 587.3295358348},
    {"d#5", 622.2539674442},
    {"eb5", 622.2539674442},
    {"e5", 659.2551138258},
    {"f5", 698.4564628660},
    {"f#5", 739.9888454233},
    {"gb5", 739.9888454233},
    {"g5", 783.9908719635},
    {"g#5", 830.6093951599},
    {"ab5", 830.6093951599},
    {"a5", 880.0000000000},
    {"a#5", 932.3275230362},
    {"bb5", 932.3275230362},
    {"b5", 987.7666025123},
    {"c6", 1046.5022612025},
    {"c#6", 1108.7305239076},
    {"db6", 1108.7305239076},
    {"d6", 1174.6590716697},
    {"d#6", 1244.5079348884},
    {"eb6", 1244.5079348884},
    {"e6", 1318.5102276516},
    {"f6", 1396.9129257321},
    {"f#6", 1479.9776908467},
    {"gb6", 1479.9776908467},
    {"g6", 1567.9817439272},
    {"g#6", 1661.2187903200},
    {"ab6", 1661.2187903200},
    {"a6", 1760.0000000002},
    {"a#6", 1864.6550460726},
    {"bb6", 1864.6550460726},
    {"b6", 1975.5332050247},
    {"c7", 2093.0045224050},
    {"c#7", 2217.4610478153},
    {"db7", 2217.4610478153},
    {"d7", 2349.3181433396},
    {"d#7", 2489.0158697770},
    {"eb7", 2489.0158697770},
    {"e7", 2637.0204553033},
    {"f7", 2793.8258514644},
    {"f#7", 2959.9553816935},
    {"g7", 3135.9634878545},
    {"g#7", 3322.4375806401},
    {"ab7", 3322.4375806401},
    {"a7", 3520.0000000006},
    {"a#7", 3729.3100921453},
    {"b7", 3951.0664100497},
    {"c8", 4186.0090448103},
  };
}

}  // end of namespace ekho