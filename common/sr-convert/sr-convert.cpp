/*

sr-convert sample rate conversion utility
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

#include <iostream>
#include <iomanip>
#include <cstdlib>

using namespace std;

#include "dsp.h"

#ifndef OUTPUT8BIT
#ifndef OUTPUT16BIT
#error OUTPUT8BIT or OUTPUT16BIT must be defined!
#endif
#endif

float sinc(float k) // nulls are at multiples of PI
{ return (k==0.)?1.:(sin(k)/k);
};

float blackman(float j) // domain: 0 to 2*PI
{ return 0.42 - 0.5*cos(j) + 0.08*cos(2*j) ;
};

void impulse_response_func(int length, int null_spacing, edeque<float> & dest)
{ float fac1=M_PI/(float)null_spacing;
  float fac2=M_PI*2./(float)length;
  for (int i=0; i<length; i++)
  { float j=i-(length*0.5);
    float k=sinc(j*fac1)*blackman(i*fac2);
    dest.push_back(k);
  }
};

inline float * align16bytes(float * f)
{
#ifdef _x86_64
   unsigned long long i = reinterpret_cast<unsigned long long>(f);
#else
   unsigned int i = reinterpret_cast<unsigned int>(f);
#endif
  i = (i + 15) & ~15;
  return reinterpret_cast<float *>(i);
};

class sr_converter_impulse_response
{ public:
    sr_converter_impulse_response(int skip_in, int skip_out, edeque<float> const & ir)
    { strips = skip_in;
      stripsize = ((ir.size() / skip_in) + 4) & ~3;
      //cout << "strips == " << strips << ", stripsize == " << stripsize << endl;
      advances = new int[strips];
      stripdata_u = new float[strips * stripsize + 4];
      stripdata = align16bytes(stripdata_u);
      max_advance = -1;
      int * advance_out = advances;
      float * strip_out = stripdata;

      int offset_v = 0;
      //int strip_num = 0;
      do
      { //cout << "offset_v == " << offset_v << ", strip_num = " << strip_num << endl;
        int offset_h = offset_v;
        for (int i = 0; i < stripsize; ++i)
        { *strip_out++ = (offset_h >= ir.size()) ? 0.f : ir[offset_h];
          offset_h += skip_in;
        }
        
        offset_v -= skip_out;
        int av = 0;
        while (offset_v < 0)
        { offset_v += skip_in; ++av;
        }
        if (av > max_advance) max_advance = av;
        *advance_out++ = av;
        //++strip_num;
      } while (offset_v != 0);
    }

    ~sr_converter_impulse_response()
    { delete[] advances;
      delete[] stripdata_u;
    }

    int strips; // number of strips
    int stripsize; // size of each strip
    int max_advance; // maximum number of samples advanced on any strip
    int * advances; // how many samples to discard after each strip
    float * stripdata; // strip data
    float * stripdata_u; // unaligned memory block for strip data
};

struct madd_params
{ float * s1; // source 1 -- MUST BE 16-ALIGNED FOR SSE
  float * s2; // source 2 -- need not be aligned
  unsigned int u; // length of both -- MUST BE A MULTIPLE OF 4 FOR SSE
  float result; // result gets stored here
};

typedef void (*madd_func)(madd_params * mp);

void madd_c(madd_params * mp) // non-SSE version
{ //cout << "madd_c" << endl;
  //cout << " : s1 (must be aligned) == " << (void*)(mp->s1) << endl;
  //cout << " : s2 == " << (void*)(mp->s2) << endl;
  //cout << " : u == " << mp->u << endl;
  float result = 0.f;
  float * s1 = mp->s1;
  float * s2 = mp->s2;
  unsigned int u = mp->u;
  while(u != 0u)
  { result += (*s1) * (*s2);
    ++s1; ++s2; --u;
  }
  mp->result = result;
};

const float SSE_ZERO[4] = { 0.f, 0.f, 0.f, 0.f };

void madd_sse(madd_params * mp) // SSE version
{ //cout << "madd_sse" << endl;
  //cout << " : s1 (must be aligned) == " << (void*)(mp->s1) << endl;
  //cout << " : s2 == " << (void*)(mp->s2) << endl;
  //cout << " : u == " << mp->u << endl;
#ifdef NO_SSE
#else
  const float * s = SSE_ZERO;
  asm volatile
  (        "movl (%%edi), %%esi\n" // ESI = source 1 (16-aligned)
    "\t"   "movl 4(%%edi), %%edx\n" // EDX = source 2
    "\t"   "movl 8(%%edi), %%ecx\n" // ECX = count
    "\t"   "andl $0xFFFFFFFC, %%ecx\n" // paranoia
    "\t"   "movups (%%eax), %%xmm0\n" // XMM0 = sum
    "\t"   "jecxz 1f\n" // if ECX==0, we're done
    "0:\t" "movaps (%%esi), %%xmm1\n"
    "\t"   "movups (%%edx), %%xmm2\n"
    "\t"   "mulps %%xmm2, %%xmm1\n"
    "\t"   "addps %%xmm1, %%xmm0\n"
    "\t"   "addl $16, %%esi\n"
    "\t"   "addl $16, %%edx\n"
    "\t"   "subl $4, %%ecx\n"
    "\t"   "jnz 0b\n"
    "1:\t" "movaps %%xmm0, %%xmm1\n"
    "\t"   "shufps $78, %%xmm1, %%xmm1\n" // 1032 base 4
    "\t"   "addps %%xmm0, %%xmm1\n" // now xmm1 has (3+1) (2+0) (3+1) (2+0)
    "\t"   "movaps %%xmm1, %%xmm0\n"
    "\t"   "shufps $57, %%xmm0, %%xmm0\n" // 0321 base 4
    "\t"   "addps %%xmm1, %%xmm0\n" // now xmm0 has four copies of (3+2+1+0)
    "\t"   "movss %%xmm0, 12(%%edi)" // store result
    : /* outputs */
    : "a" (s), "D" (mp) /* inputs */
    : "memory", "%esi", "%edx", "%ecx" /* clobbers */
  );
#endif
};

struct madd2_params
{ float * s1; // impulse response strip, MUST BE 16-ALIGNED FOR SSE
  float * s2_st; // samples, stereo interleaved (any alignment)
  unsigned int u; // length of impulse response strip; stereo must be double this; MULTIPLE OF 4
  float result_l;
  float result_r;
};

typedef void (*madd2_func)(madd2_params * mp);

void madd2_c(madd2_params * mp)
{ float result_l = 0.f;
  float result_r = 0.f;
  float * s1 = mp->s1;
  float * s2_st = mp->s2_st;
  unsigned int u = mp->u;
  while (u != 0u)
  { result_l += (*s1) * (*s2_st); ++s2_st;
    result_r += (*s1) * (*s2_st); ++s2_st; ++s1; --u;
  }
  mp->result_l = result_l;
  mp->result_r = result_r;
};

/*
  SHUFPS -- generally shufps $const, S, D
   basically D[0] = D[const & 3]
             D[1] = D[(const >> 2) & 3]
             D[2] = S[(const >> 4) & 3]
             D[3] = S[(const >> 6) & 3]
*/

void madd2_sse(madd2_params * mp) // HIGHLY PARALLELIZABLE!
{ const float * s = SSE_ZERO;
#ifdef NO_SSE
#else
  asm volatile
  (        "movl (%%edi), %%esi\n" // ESI = source 1 (16-aligned)
    "\t"   "movl 4(%%edi), %%edx\n" // EDX = source 2 (st interleaved)
    "\t"   "movl 8(%%edi), %%ecx\n" // ECX = count
    "\t"   "andl $0xFFFFFFFC, %%ecx\n" // paranoia
    "\t"   "movups (%%eax), %%xmm0\n" // left sums
    "\t"   "movups (%%eax), %%xmm1\n" // right sums
    "\t"   "jecxz 1f\n" // if ECX == 0, we're done
    "0:\t" "movaps (%%esi), %%xmm2\n" // get impulse response strip

    "\t"   "movups (%%edx), %%xmm3\n"
    "\t"   "movups 16(%%edx), %%xmm4\n" // get interleaved inputs
    "\t"   "movaps %%xmm3, %%xmm5\n"
    "\t"   "movaps %%xmm3, %%xmm6\n"
    "\t"   "shufps $136, %%xmm4, %%xmm5\n" // 2020 == left channel
    "\t"   "shufps $221, %%xmm4, %%xmm6\n" // 3131 == right channel
    "\t"   "mulps %%xmm2, %%xmm5\n"
    "\t"   "mulps %%xmm2, %%xmm6\n" // multiply
    "\t"   "addps %%xmm5, %%xmm0\n"
    "\t"   "addps %%xmm6, %%xmm1\n" // add
    "\t"   "addl $16, %%esi\n"
    "\t"   "addl $32, %%edx\n"
    "\t"   "subl $4, %%ecx\n"
    "\t"   "jnz 0b\n"
    "1:\t" "movaps %%xmm0, %%xmm2\n"
    "\t"   "movaps %%xmm1, %%xmm3\n"
    "\t"   "shufps $78, %%xmm2, %%xmm2\n"
    "\t"   "shufps $78, %%xmm3, %%xmm3\n" // 1032
    "\t"   "addps %%xmm0, %%xmm2\n"
    "\t"   "addps %%xmm1, %%xmm3\n"
    "\t"   "movaps %%xmm2, %%xmm0\n"
    "\t"   "movaps %%xmm3, %%xmm1\n"
    "\t"   "shufps $57, %%xmm0, %%xmm0\n" 
    "\t"   "shufps $57, %%xmm1, %%xmm1\n" // 0321
    "\t"   "addps %%xmm2, %%xmm0\n"
    "\t"   "addps %%xmm3, %%xmm1\n"
    "\t"   "movss %%xmm0, 12(%%edi)\n" // store left result 
    "\t"   "movss %%xmm1, 16(%%edi)" // store right result

    : /* outputs */
    : "a" (s), "D" (mp) /* inputs */
    : "memory", "%esi", "%edx", "%ecx" /* clobbers */
  );
#endif
};

struct mmov_params
{ float * dest_aligned; // MUST BE 16-ALIGNED FOR SSE
  float * src; // need not be aligned
  unsigned int u; // length of both -- MUST BE A MULTIPLE OF 4 FOR SSE
};

typedef void (*mmov_func)(mmov_params * mp);

void mmov_c(mmov_params * mp) // non-SSE version
{ //cout << "mmov_c" << endl;
  //cout << " : dest_aligned == " << (void*)(mp->dest_aligned) << endl;
  //cout << " : src == " << (void*)(mp->src) << endl;
  //cout << " : u == " << mp->u << endl;
  float * d = mp->dest_aligned;
  float * s = mp->src;
  unsigned int u = mp->u;
  while (u != 0u)
  { *d++ = *s++; --u;
  }
};

void mmov_sse(mmov_params * mp)
{ //cout << "mmov_sse" << endl;
  //cout << " : dest_aligned == " << (void*)(mp->dest_aligned) << endl;
  //cout << " : src == " << (void*)(mp->src) << endl;
  //cout << " : u == " << mp->u << endl;
#ifdef NO_SSE
#else
  asm volatile
  (        "movl (%%edi), %%esi\n" // ESI = dest
    "\t"   "movl 4(%%edi), %%edx\n" // EDX = src
    "\t"   "movl 8(%%edi), %%ecx\n" // ECX = count
    "\t"   "andl $0xFFFFFFFC, %%ecx\n" // paranoia
    "\t"   "jecxz 1f\n"
    "0:\t" "movups (%%edx), %%xmm0\n"
    "\t"   "movaps %%xmm0, (%%esi)\n"
    "\t"   "addl $16, %%esi\n"
    "\t"   "addl $16, %%edx\n"
    "\t"   "subl $4, %%ecx\n"
    "\t"   "jnz 0b\n"
    "1:\t"
    : /* outputs */
    : "D" (mp) /* inputs */
    : "memory", "%esi", "%edx", "%ecx" /* clobbers */
  );
#endif
};

bool have_sse() // check to see if we have SSE on this computer
{
#ifdef NO_SSE
  return false;
#else
  int features;
  asm volatile
  (      "movl $1, %%eax\n"
    "\t" "cpuid"
    : "=d" (features) /* outputs */
    : /* inputs */
    : "%eax", "%ebx", "%ecx" /* clobbers */
  );
  return (features & (1 << 25)) != 0;
#endif
};

class sr_input_buffer
{ public:
    sr_input_buffer
    ( int _strip_length,
      int _max_advance,
      sample_giver * _src,
      mmov_func _mmov
    ) : strip_length(_strip_length),
        max_advance(_max_advance),
        src(_src),
        mmov(_mmov)
    { buf_u = new float[2 * strip_length + max_advance + 4];
      buf = align16bytes(buf_u);
      zero_fill_size = 0;
      strip_begin = buf;
      strip_end = buf;
      while (size() < strip_length)
      { *strip_end = 0.f; ++strip_end;
      }
    }

    ~sr_input_buffer()
    { delete[] buf_u;
      delete src;
    }

    float * lock()
    { return strip_begin;
    }

    bool is_all_filler() const
    { return zero_fill_size >= strip_length;
    }

    void unlock(int adv)
    { strip_begin += adv;
      refill();
    };

  private:

    void fetch()
    { bool b = src->give(*strip_end);
      if (!b)
      { *strip_end = 0.f;
        zero_fill_size++;
      }
      strip_end++;
    };

    int size() const
    { return strip_end - strip_begin;
    };

    bool should_copy_back()
    { return strip_begin >= (buf + strip_length);
    }

    void copy_back()
    { mmov_params mp;
      mp.dest_aligned = buf;
      mp.src = strip_begin;
      mp.u = strip_length;
      mmov(&mp);
      strip_begin = buf;
      strip_end = buf + strip_length;
    }

    void refill()
    { while (size() < strip_length) fetch();
      if (should_copy_back()) copy_back();
    }

    int strip_length;
    int max_advance;
    sample_giver * src;
    mmov_func mmov;
    int zero_fill_size;
    float * buf_u;
    float * buf;
    float * strip_begin;
    float * strip_end;
};

class sr_converter: public sample_giver
{ public:
    sr_converter
    ( sample_giver * _src,
      sr_converter_impulse_response * _scir,
      madd_func _madd, mmov_func _mmov
    )
    : sib(_scir->stripsize, _scir->max_advance, _src, _mmov),
      scir(_scir), madd(_madd)
    { adv = scir->advances;
      strip = scir->stripdata;
    }

    virtual ~sr_converter()
    { delete scir;
    }

    virtual bool give(float & s)
    { if (sib.is_all_filler()) return false;
      madd_params mp;
      mp.s1 = strip;
      mp.s2 = sib.lock();
      mp.u = scir->stripsize;
      madd(&mp);
      sib.unlock(*adv);
      s = mp.result;
      ++adv;
      strip += scir->stripsize;
      if (adv == scir->advances + scir->strips)
      { adv = scir->advances;
        strip = scir->stripdata;
      }
      return true;
    };

    virtual bool failed() const
    { return sib.is_all_filler();
    };

  private:
    sr_input_buffer sib;
    sr_converter_impulse_response * scir;
    madd_func madd;
    int * adv; 
    float * strip;
};
    
class stereo_sr_converter: public sample_giver
{ public:
    stereo_sr_converter
    ( sample_giver * _src,
      sr_converter_impulse_response * _scir,
      madd2_func _madd2, mmov_func _mmov
    )
    : sib(_scir->stripsize * 2, _scir->max_advance * 2, _src, _mmov),
      scir(_scir), madd2(_madd2),
      r_is_holding(false) // OOOOOPS! This had been omitted...
    { adv = scir->advances;
      strip = scir->stripdata;
    }

    virtual ~stereo_sr_converter()
    { delete scir;
    }

    virtual bool give(float & s)
    { if (sib.is_all_filler()) return false;
      if (r_is_holding)
      { s = r_hold;
        r_is_holding = false;
        return true;
      }
      else 
      { madd2_params mp;
        mp.s1 = strip;
        mp.s2_st = sib.lock();
        mp.u = scir->stripsize;
        madd2(&mp);
        sib.unlock(2 * (*adv));
        s = mp.result_l;
        r_is_holding = true;
        r_hold = mp.result_r;
        ++adv;
        strip += scir->stripsize;
        if (adv == scir->advances + scir->strips)
        { adv = scir->advances;
          strip = scir->stripdata;
        }
        return true;
      }
    };

    virtual bool failed() const
    { return sib.is_all_filler();
    };

  private:
    sr_input_buffer sib;
    sr_converter_impulse_response * scir;
    madd2_func madd2;
    int * adv; 
    float * strip;
    bool r_is_holding;
    double r_hold;
};

struct converter_spec
{ bool use_sse;
  bool stereo;
  int nullcount;
  int skip_in;
  int skip_out;
};

sample_giver * make_converter(converter_spec const & cs, sample_giver * src)
{ int nullspace = (cs.skip_in > cs.skip_out) ? cs.skip_in : cs.skip_out;
  int irlength = cs.nullcount * nullspace + 1;
//  cout << "Length of impulse response: " << irlength << endl;
  edeque<float> ir; impulse_response_func(irlength, nullspace, ir);

  /*
  if (diffnullcount > 0)
  { edeque<float> dr; impulse_response_func(diffnullcount*nullspace+1, nullspace, dr);
    cout << "Length of subtrahend response: " << dr.size() << endl;
    int offset = ((nullcount - diffnullcount)*nullspace)/2;
    cout << "Offset into difference response: " << offset << endl;
    for (int i=0; i<dr.size(); i++)
    { ir[i+offset] -= dr[i];
    }
  }
  */

  if (cs.skip_out > cs.skip_in)
  { // Gain normalization for downsampling
    float gain = ((float)cs.skip_in) / ((float)cs.skip_out);
    for (int i=0; i<ir.size(); i++) ir[i] *= gain;
  }
  { // display gain (unconditional)
    float allgains = 0.f;
    for (int i = 0; i<ir.size(); ++i) allgains += ir[i];
//    cout << "Gain of impulse response (should be 1.000): " << allgains / cs.skip_in << endl;
  }

  madd_func madd;
  mmov_func mmov;
  madd2_func madd2;
/*
  if (cs.use_sse)
  { madd = madd_sse; mmov = mmov_sse; madd2 = madd2_sse;
  }
  else
  { madd = madd_c; mmov = mmov_c; madd2 = madd2_c;
  }
*/
  madd = madd_c; mmov = mmov_c; madd2 = madd2_c;
  
  sr_converter_impulse_response * scir =
    new sr_converter_impulse_response(cs.skip_in, cs.skip_out, ir);

  //cout << "Optimized impulse response at " << (void*)scir->stripdata
    //   << ", 16-aligned from " << (void*)scir->stripdata_u << "." << endl;

  //cout << scir->strips << " strips at " << scir->stripsize << " samples each." << endl;

  if (cs.stereo)
  { //cout << "Using stereo." << endl;
    return new stereo_sr_converter(src, scir, madd2, mmov);
  }
  else
  { //cout << "Using mono." << endl;
    return new sr_converter(src, scir, madd, mmov);
  }
};

#include "skips.out"

void string_to_int(const char * s, int & out)
{
#if 1
  // changed by Cameron Wong for removing strsteam dependence.
  out = atoi(s);
#else
  // original code
  bool valid = false;
  istrstream q(s); q >> out; valid = (!q.fail());
#ifndef DISABLE_EXCEPTIONS
  if (!valid) throw 0;
#endif
#endif
};

int rate_lookup(int rate)
{ for (int index = 0; index < rate_count; ++index)
  { if (rate == rates[index]) return index;
  }
#ifndef DISABLE_EXCEPTIONS
  throw 1;
#endif
};

void print_supported_rates(void)
{ for (int i = 0; i < rate_count; ++i)
  { cout << setw(9) << rates[i];
    if ((i+1) != rate_count) cout << " ";
  }
  cout << setw(0) << endl;
};

void print_program_name()
{
#ifdef OUTPUT8BIT
  cout << "sr-convert 0.83, 8-bit, " << __DATE__ << " " << __TIME__ << endl;
#else
  cout << "sr-convert 0.83, 16-bit, " << __DATE__ << " " << __TIME__ << endl;
#endif
};

void print_license_info(void)
{ print_program_name();
  cout << "Copyright (C) 2005 by Edward Kiser. Licensed under GPL." << endl
       << endl
       << "This program is free software; you can redistribute it and/or modify" << endl
       << "it under the terms of the GNU General Public License as published by" << endl
       << "the Free Software Foundation; either version 2 of the License, or" << endl
       << "(at your option) any later version." << endl
       << endl
       << "This program is distributed in the hope that it will be useful," << endl
       << "but WITHOUT ANY WARRANTY; without even the implied warranty of" << endl
       << "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" << endl
       << "GNU General Public License for more details." << endl
       << endl
       << "You should have received a copy of the GNU General Public License" << endl
       << "along with this program; if not, write to the Free Software" << endl
       << "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA" << endl;
};
 
//int main(int argc, char * argv[])
int sr_convert(const char *infile, int inrate, const char *outfile, int outrate)
{
  int argc = 5;
  const char *argv[5];
  char inrate_buf[10];
  sprintf(inrate_buf, "%d", inrate);
  char outrate_buf[10];
  sprintf(outrate_buf, "%d", outrate);
  argv[1] = infile;
  argv[2] = inrate_buf;
  argv[3] = outfile;
  argv[4] = outrate_buf;
#ifndef DISABLE_EXCEPTIONS
  try
  {
    if ((argc != 5) && (argc != 6)) throw 0;
#endif
    
    bool sse_avail = have_sse();
//    cout << "Streaming SIMD Extensions (SSE) " << (sse_avail ? "" : "not ") << "detected." << endl;

    const char * src = argv[1];

    wav_file_giver * awav = new wav_file_giver(src);
    if (awav->failed())
    { cout << "Input source failed!" << endl;
      return -1;
    }

    int chans = awav->channels();
    if ((chans != 1) && (chans != 2))
    { cout << "Sorry, " << chans << "-channel input is not supported in this version." << endl;
      return -1;
    }

    int src_rate;
    if (strcmp(argv[2], "-") == 0) src_rate = awav->samprate();
    else string_to_int(argv[2], src_rate);

    int src_rate_index = rate_lookup(src_rate);

    const char * dest = argv[3];
    int dest_rate; string_to_int(argv[4], dest_rate);
    int dest_rate_index = rate_lookup(dest_rate);

#ifdef OUTPUT8BIT
    wav_file_taker * wft = new wav_file_taker(dest, dest_rate, 8, awav->channels());
#else
    wav_file_taker * wft = new wav_file_taker(dest, dest_rate, 16, awav->channels());
#endif

    sample_giver * aa = 0;

    if (dest_rate == src_rate)
    { aa = awav;
      //cout << "Sample rates are the same; copying \"" << src
        //   << "\" to \"" << dest << "\"." << endl;
    }
    else
    { int nulls = 500;
      if (argc == 6) string_to_int(argv[5], nulls);

      int skip_in = skips[ dest_rate_index * rate_count + src_rate_index ];
      if (skip_in < 0)
      { int mid_rate_index = -1 - skip_in;
        if (mid_rate_index >= rate_count)
        { mid_rate_index -= rate_count;
          int mid_rate_index_1 = mid_rate_index % rate_count;
          int mid_rate_index_2 = mid_rate_index / rate_count;
          int mid_rate_1 = rates[mid_rate_index_1];
          int mid_rate_2 = rates[mid_rate_index_2];
          
          int skip_in_1 = skips[ mid_rate_index_1 * rate_count + src_rate_index ];
          int skip_in_2 = skips[ mid_rate_index_2 * rate_count + mid_rate_index_1 ];
          int skip_in_3 = skips[ dest_rate_index * rate_count + mid_rate_index_2 ];
          int skip_out_1 = skips[ src_rate_index * rate_count + mid_rate_index_1 ];
          int skip_out_2 = skips[ mid_rate_index_1 * rate_count + mid_rate_index_2 ];
          int skip_out_3 = skips[ mid_rate_index_2 * rate_count + dest_rate_index ];
/*
          cout << "Converting \"" << src << "\" at " << src_rate << endl
               << " to intermediate at " << mid_rate_1 << endl
               << " then to intermediate at " << mid_rate_2 << endl
               << " then to \"" << dest << "\" at " << dest_rate << endl
               << " using skip_in_1 == " << skip_in_1
               << " and skip_out_1 == " << skip_out_1 << endl
               << " then skip_in_2 == " << skip_in_2
               << " and skip_out_2 == " << skip_out_2 << endl
               << " then skip_in_3 == " << skip_in_3
               << " and skip_out_3 == " << skip_out_3 << endl;
          cout << "First stage:" << endl;
*/
          converter_spec s;
          s.use_sse = sse_avail;
          s.stereo = (chans == 2);
          s.nullcount = nulls;
          s.skip_in = skip_in_1;
          s.skip_out = skip_out_1;
          aa = make_converter(s, awav);
          //cout << "Second stage:" << endl;
          s.skip_in = skip_in_2;
          s.skip_out = skip_out_2;
          aa = make_converter(s, aa);
          //cout << "Third stage:" << endl;
          s.skip_in = skip_in_3;
          s.skip_out = skip_out_3;
          aa = make_converter(s, aa);
        }
        else
        {
          int mid_rate = rates[mid_rate_index];
          int skip_in_1 = skips[ mid_rate_index * rate_count + src_rate_index ];
          int skip_in_2 = skips[ dest_rate_index * rate_count + mid_rate_index ];
          int skip_out_1 = skips[ src_rate_index * rate_count + mid_rate_index ];
          int skip_out_2 = skips[ mid_rate_index * rate_count + dest_rate_index ];
/*
          cout << "Converting \"" << src << "\" at " << src_rate << endl
               << " to intermediate at " << mid_rate << endl
               << " then to \"" << dest << "\" at " << dest_rate << endl
               << " using skip_in_1 == " << skip_in_1
               << " and skip_out_1 == " << skip_out_1 << endl
               << " then skip_in_2 == " << skip_in_2
               << " and skip_out_2 == " << skip_out_2 << endl;
          cout << "First stage:" << endl;
*/
          converter_spec s;
          s.use_sse = sse_avail;
          s.stereo = (chans == 2);
          s.nullcount = nulls;
          s.skip_in = skip_in_1;
          s.skip_out = skip_out_1;
          aa = make_converter(s, awav);
          //cout << "Second stage:" << endl;
          s.skip_in = skip_in_2;
          s.skip_out = skip_out_2;
          aa = make_converter(s, aa);
        }
      }
      else
      { int skip_out = skips [ src_rate_index * rate_count + dest_rate_index ];

        /*cout << "Converting \"" << src << "\" at " << src_rate << endl
             << " to \"" << dest << "\" at " << dest_rate << endl
             << " using skip_in == " << skip_in
             << " and skip_out == " << skip_out << endl
             << " and " << nulls << " nulls" << endl;
*/
        converter_spec s;
        s.use_sse = sse_avail;
        s.stereo = (chans == 2);
        s.nullcount = nulls;
        s.skip_in = skip_in; 
        s.skip_out = skip_out;
        aa = make_converter(s, awav);
      }
    }

    pump_samples(aa, wft, -1, wft);
    if (wft->min_max_exists())
    { float min = wft->min();
      float max = wft->max();
      //cout << "Minimum: " << min << endl;
      //cout << "Maximum: " << max << endl;
      //cout << "Middle: " << (min+max)*0.5f << endl;
      //cout << "Range: " << (max - min) << endl;
      float scale = 2.f/(max-min);
      //cout << "Scale: " << scale << endl;
      //cout << "Offset (after scale): " << (-1.f-min*scale) << endl;
    }
    delete aa;
    delete wft;
#ifndef DISABLE_EXCEPTIONS
  }
  catch(int x)
  { if (x == 0)
    { 
      if ((argc >= 2) && (strcmp(argv[1], "--license") == 0))
      { print_license_info();
      }
      else
      {        //         1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
               //1   5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
        print_program_name();
        cout << "Copyright (C) 2005 by Edward Kiser. Licensed under GPL." << endl
             << endl
             << "Syntax: sr-convert --license    (for licensing info)" << endl
             << "        sr-convert <srcfile> <inrate> <destfile> <outrate> [nulls]" << endl
             << endl
             << "If <inrate> is \"-\", then the input sampling rate will be" << endl
             << "read from the input file. Supported sampling rates are as follows:"
             << endl << endl;
        print_supported_rates();
        cout << endl
             << "Some sampling rates are interpreted as nearby fractional rates. See instruction" << endl
             << "manual for details." << endl;
      }
    }
    else // ( x == 1)
    { cout << "Invalid sampling rate. Supported sampling rates are: " << endl;
      print_supported_rates();
    }
  }
#endif

  return 0;
};
