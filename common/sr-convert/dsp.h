/*
DSP.H -- framework for digital signal processing in C++
by Ed Kiser

The notations "abstract" and "final" are borrowed from Java.

An "abstract" class cannot be instantiated because some virtual function(s) is
(are) not defined. Pointers to an abstract class can exist, however. To use
such a pointer, you must point it to a descendant that overrides the undefined
virtual functions.

C++ determines whether a class is abstract or not by looking inside; this
notation saves YOU the trouble.

A "final" class is one which was not designed for other classes to derive from
it. Java actually enforces this for security reasons; C++ doesn't enforce it.
I use it as a recommendation: make this class a data member, not an ancestor.

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
#define __DSP_H
#else
#error Multiple includes of DSP.H are not allowed
#endif

#include <fstream>
#include <iomanip>
#include <iostream>
#include <cmath>
#include <cstdlib>

using namespace std;

#define M_PI 3.14159265358979323846

#include "edeque.h"

/*abstract*/ class sample_giver
{ public:
   virtual ~sample_giver() {};
   virtual bool give(float & s) = 0;
   // gives ONE sample. Returns FALSE if failed.
   virtual bool give(float array[], int howmany);
   // get a whole bunch of samples. Default calls single give repeatedly.
   virtual bool skip(int howmany=1);
   // skip samples. Override to save the trouble of generating them.
   virtual bool failed(void) const = 0; // TRUE if you just read past the end.
   virtual int gave(void) const { return mgave; };
   // if failed(), how many samples you actually got.

  protected:
   int mgave;
};

/*abstract*/ class sample_taker
{ public:
   virtual ~sample_taker() {};
   virtual void take(float s) = 0; // take one sample.
   virtual void take(float array[], int howmany); // take a whole bunch.
   virtual void close(void); // indicates we're done.
};

/*final*/ class wav_file_giver: public sample_giver
{ public:
   wav_file_giver(const char * name);
   ~wav_file_giver();

   int samprate() const;
   int bits() const;
   int channels() const;
   virtual bool failed() const;
   virtual bool give(float & s);

  private:
   wav_file_giver(wav_file_giver const &); // undefined
   wav_file_giver const & operator=(wav_file_giver const &); // undefined

   int end_offset;
   int mrate, mbits, mchans;
   istream * reader;
};

/*final*/ class wav_file_taker: public sample_taker
{ public:
   wav_file_taker(const char * name, int samprate=44100, int bits=16, int channels=1);
   ~wav_file_taker();

   bool failed() const;
   virtual void take(float s);

   bool clipped() const;
   void clear_clip();

   bool min_max_exists() const;
   float max() const;
   float min() const;
  private:
   wav_file_taker(wav_file_taker const &); // undefined
   wav_file_taker const & operator=(wav_file_taker const &); // undefined

   void write_header(void);

   bool mclipped;
   int start_offset;
   int bytes_written;
   int mrate, mbits, mchans;
   int chan_now;
   ostream * writer;

   bool _have_min_max;
   float _min, _max;

   float * error;
};

int pump_samples(sample_giver * a, sample_taker * b, int limit=-1,
  wav_file_taker * clipmon = 0);
// same but for samples.

