/*
DSP.CPP -- framework for digital signal processing in C++
by Ed Kiser

------------

This file is part of the sr-convert sample rate conversion utility
Copyright (c) 2002, 2003 by Edward Kiser

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/

#ifndef __DSP_H
#include "dsp.h"
#endif

const int N_BITS_PER_INT=32; // improve this on other systems.

/*** sample giver ***/

bool sample_giver::give(float array[], int howmany)
{ if (howmany==0) return true;
  if (howmany<0) return false;
  mgave=0;
  bool failedyet=false; // failed yet?
  for (int i=0; i<howmany; i++)
  { failedyet = failedyet || !give(array[i]); // depends on short-circuit
    if (!failedyet) mgave++;
    else array[i]=0.;
  }
  return !failedyet;
};

bool sample_giver::skip(int howmany)
{ if (howmany==0) return true;
  if (howmany<0) return false;
  mgave=0;
  bool failedyet=false;
  float dummy;
  for (int i=0; i<howmany; i++)
  { failedyet = failedyet || !give(dummy);
    if (!failedyet) mgave++;
  }
  return !failedyet;
};

/*** sample taker ***/

void sample_taker::take(float array[], int howmany)
{ if (howmany<=0) return;
  for (int i=0; i<howmany; i++)
  { take(array[i]);
  }
};

void sample_taker::close(void)
{ // do nothing
};

/*** wav file helpers (private) ***/

struct wavhdr
{ void set(int srate, bool bits16, int chans, int datlen);
  bool is_valid() const;

  int riff;
  int totallength;
  int wave;
  int fmt;
  int formatlength;
  unsigned short pcm, channels;
  int samplerate, bytespersec;
  unsigned short samplebytes, samplebits;
  int data;
  int datalength;

  static const int riffmagic,wavemagic,fmtmagic,datamagic,badmagic;
  static const short pcmmagic;
  static const int sizeofhdr, origin;
};

const int wavhdr::riffmagic=0x46464952, // "RIFF"
          wavhdr::wavemagic=0x45564157, // "WAVE"
          wavhdr::fmtmagic =0x20746d66, // "fmt "
          wavhdr::datamagic=0x61746164, // "data"
          wavhdr::badmagic =0x1a444142, // "BAD^Z"

          wavhdr::sizeofhdr = 36, // was 40 (incorrectly) in some old code!
          wavhdr::origin = 8; // used for file measuring, was 0 (incorrectly)

const short wavhdr::pcmmagic = 1;

void wavhdr::set(int srate, bool bits16, int chans, int datlen)
{ riff=riffmagic;
  totallength=datlen+sizeofhdr;
  wave=wavemagic;
  fmt=fmtmagic;
  formatlength=16;
  pcm=pcmmagic;
  channels=chans;
  samplebytes=(bits16?2:1)*channels;
  samplebits=bits16?16:8;
  samplerate=srate;
  bytespersec=srate*samplebytes; // fixed Oct 3, 1997 (late!!!)
  data=datamagic;
  datalength=datlen;
};

/* This is a hack to use the old wavhdr structure with new wave files that
   have comments or markers in them. It still expects the data block to follow
   the format block. It also still expects PCM format. */

static bool readheader(istream & in, wavhdr & out)
{ wavhdr temp;
  char * buf=(char*)&temp;
  in.read(buf,12);
  if ((temp.riff!=wavhdr::riffmagic) || (temp.wave!=wavhdr::wavemagic))
   { cout << "0" << endl; return false; };
  int type=0, length=0;
  bool done=false;
  do
  { in.read((char*)&type,4);
    in.read((char*)&length,4);
    if (type!=wavhdr::fmtmagic)
    { cout << "skipping type 0x" << hex << type << dec << " length "
           << length << endl;
      in.seekg((length+1)&(~1),ios::cur);
    } else done=true;
  } while ((!done) && (!in.fail()));
  if (in.fail()) { cout << "1" << endl; return false; };
  temp.fmt=type;
  temp.formatlength=length;
  in.read(buf+20,16);
  if (temp.pcm!=wavhdr::pcmmagic) { cout << "3" << endl; return false; };
  if (temp.formatlength>16) in.seekg(temp.formatlength-16,ios::cur);
  done=false;
  do
  { in.read((char*)&type,4);
    in.read((char*)&length,4);
    if (type!=wavhdr::datamagic)
    { cout << "skipping type 0x" << hex << type << dec << " length "
           << length << endl;
      in.seekg((length+1)&(~1),ios::cur);
    } else done=true;
  } while ((!done) && (!in.fail()));
  if (in.fail()) return false;
  temp.data=type;
  temp.datalength=length;
  out=temp; return true;
};

static short sampcvt16(float f, bool * clipflag = 0)
{ static bool dummy;
  if (clipflag == 0) clipflag = &dummy;
  float g=f*32768.;
  if (g>32767.) { g=32767.; *clipflag = true; }
  if (g<-32768.) { g=-32768.; *clipflag = true; }
  short v=(short)g;
  return v;
};

static float sampcvt16(short s)
{ float f=s/32768.;
  return f;
};

static short sampcvt8(float f, bool * clipflag = 0)
{ static bool dummy;
  if (clipflag == 0) clipflag = &dummy;
  float g=f*128.;
  if (g>127.) { g=127.; *clipflag = true; }
  if (g<-128.) { g=-128.; *clipflag = true; }
  short v=(short)g;
  v=(v+0x80)&0xFF;
  return v;
};

static float sampcvt8(short s)
{ float f=((s&0xFF)-0x80)/128.;
  return f;
};

static short sampcvt16ecd(float f, float & error, bool * clipflag = 0)
{ float f_corrected = f - error;
  short q = sampcvt16(f, clipflag);
  float f_quantized = sampcvt16(q);
  error = f_quantized - f_corrected;
  return q;
};

static short sampcvt8ecd(float f, float & error, bool * clipflag = 0)
{ float f_corrected = f - error;
  short q = sampcvt8(f, clipflag); 
  float f_quantized = sampcvt8(q);
  error = f_quantized - f_corrected;
  return q;
};

/*** wav_file_giver ***/

wav_file_giver::wav_file_giver(const char * name)
{ reader=new ifstream(name,ios::in | ios::binary);
  if (reader->fail())
  { cerr << "***** ERROR *****" << endl
         << "WAV file giver at " << this << " failed to open WAV file \""
         << name << "\"." << endl;
    delete reader;
    reader=0;
    return;
  }
  wavhdr w;
  bool gotheader=readheader(*reader,w);
  if ((!gotheader) || reader->fail() /*|| (w.channels!=1)*/)
  { cerr << "***** ERROR *****" << endl
         << "WAV file giver at " << this << " failed to read a" << endl
      << "  valid WAV file header from the file \"" << name << "\"." << endl;
    /*
    if (w.channels!=1)
     cerr << name << ": expected 1 channel, got " << w.channels << endl;
    */
    delete reader;
    reader=0;
    return;
  }
  int start=reader->tellg();
  end_offset=start+w.datalength;
  mrate=w.samplerate;
  mbits=w.samplebits;
  mchans=w.channels;
};

wav_file_giver::~wav_file_giver()
{ if (reader) delete reader;
};

int wav_file_giver::samprate() const
{ return mrate;
};

int wav_file_giver::bits() const
{ return mbits;
};

int wav_file_giver::channels() const
{ return mchans;
};

bool wav_file_giver::failed() const
{ return (reader==0) || (reader->fail());
};

bool wav_file_giver::give(float & s)
{ float ts;
  if (mbits==8)
  { unsigned char cs; // fixed Dec 20, 1997 (way too late!)
    reader->read(reinterpret_cast<char *>(&cs),1);
    if (reader->fail() || (reader->tellg() > end_offset)) return false;
    ts=sampcvt8((short)cs);
  } else
  { short cs;
    reader->read((char*)&cs,2);
    if (reader->fail() || (reader->tellg() > end_offset)) return false;
    ts=sampcvt16((short)cs);
  }
  s=ts; return true;
};

/*** wav_file_taker ***/

wav_file_taker::wav_file_taker
( const char * name, int samprate, int bits, int channels
)
: mclipped(false), mrate(samprate), mbits(bits),
  mchans(channels), chan_now(0), writer(0), _have_min_max(false)
{ if ((mrate<1) || (mrate>2097152)) return;
  if ((mbits!=8) && (mbits!=16)) return;
  if ((mchans<1) || (mchans>64)) return;
  error = new float[mchans];
  writer=new ofstream(name, ios::out | ios::binary);
  if (!writer->fail())
  { wavhdr w;
    w.riff=wavhdr::badmagic;
    writer->write((char*)&w,sizeof(w));
    start_offset=writer->tellp();
    bytes_written=0;
  } else
  { start_offset=-1;
    delete writer; writer=0;
  }
};

wav_file_taker::~wav_file_taker()
{ if (writer)
  { while (chan_now!=0) take(0.);
    write_header();
    delete writer;
  }
  delete[] error;
};

bool wav_file_taker::failed() const
{ return (writer==0) || (writer->fail());
};

void wav_file_taker::take(float s)
{ if (!writer) return;
  if (mbits==16)
  { short sig=sampcvt16ecd(s, error[chan_now], &mclipped);
    writer->write((char*)&sig,sizeof(sig));
  } else
  { char sig=sampcvt8ecd(s, error[chan_now], &mclipped);
    writer->write(&sig,1);
  }
  chan_now++;
  if (chan_now==mchans)
  { chan_now=0;
    int z=writer->tellp();
    if (z - start_offset >= 3000000)
    { write_header();
      writer->seekp(z);
      start_offset=z;
    }
  } 
  if (!_have_min_max)
  { _min = s; _max = s; _have_min_max = true;
  }
  else
  { if (_min > s) _min = s;
    if (_max < s) _max = s;
  }
};

bool wav_file_taker::min_max_exists() const
{ return _have_min_max;
};

float wav_file_taker::max() const
{ return _max;
};

float wav_file_taker::min() const
{ return _min;
};

bool wav_file_taker::clipped() const { return mclipped; };

void wav_file_taker::clear_clip() { mclipped = false; };

void wav_file_taker::write_header(void)
{ bytes_written = static_cast<int>(writer->tellp()) - wavhdr::origin;
/*  cout << endl << "(wav_file_taker at " << this
       << " wrote header for " << bytes_written - wavhdr::sizeofhdr
       << " data bytes.)";*/
  writer->seekp(0);
  wavhdr w;
  w.set(mrate,mbits==16,mchans,bytes_written - wavhdr::sizeofhdr);
  writer->write((char*)&w,sizeof(w));
};

/*** pump samples ***/

int pump_samples(sample_giver * a, sample_taker * b, int limit,
  wav_file_taker * clipmon /* = 0 */)
{ bool done=false;
  int samples=0;
  while (!done)
  { float t;
    done=!a->give(t);
    if (!done)
    { b->take(t);
      samples++;
    }
    if ((samples & 0xFFFF) == 0)
    { bool clipped = false;
      if (clipmon)
      { if (clipmon->clipped())
        { clipped = true;
          clipmon->clear_clip();
        }
      }
      cout << ((clipped)?"!":".") << flush;
      if ((limit > 0) && (samples >= limit)) done=true;
      //if (kbhit())
      //{ getxkey();
      //  cout << endl << "Paused. Press a key to continue." << endl;
      //  getxkey();
      //  cout << "Running." << endl;
      //}
    }
  }
  //cout << endl;
  return samples;
};

