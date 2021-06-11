/*
 
 This file is part of FFTS -- The Fastest Fourier Transform in the South
  
 Copyright (c) 2013, Michael J. Cree <mcree@orcon.net.nz> 
 Copyright (c) 2012, 2013, Anthony M. Blake <amb@anthonix.com>
 
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 	* Redistributions of source code must retain the above copyright
 		notice, this list of conditions and the following disclaimer.
 	* Redistributions in binary form must reproduce the above copyright
 		notice, this list of conditions and the following disclaimer in the
 		documentation and/or other materials provided with the distribution.
 	* Neither the name of the organization nor the
	  names of its contributors may be used to endorse or promote products
 		derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL ANTHONY M. BLAKE BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#ifndef __MACROS_ALTIVEC_H__
#define __MACROS_ALTIVEC_H__

#include <math.h>
#include <altivec.h>

#define restrict

typedef vector float V;
typedef vector unsigned char VUC;

#define VLIT4(f0,f1,f2,f3) ((V){f0, f1, f2, f3})

#define VADD(x,y) vec_add(x,y)
#define VSUB(x,y) vec_sub(x,y)
#define VMUL(x,y) vec_madd(x,y,(V){0})
#define VMULADD(x,y,z) vec_madd(x,y,z)
#define VNMULSUB(x,y,z) vec_nmsub(x,y,z)
#define VXOR(x,y) vec_xor((x),(y))
#define VSWAPPAIRS(x)						\
    vec_perm(x,x,(VUC){0x04,0x05,0x06,0x07,0x00,0x01,0x02,0x03,	\
		       0x0c,0x0d,0x0e,0x0f,0x08,0x09,0x0a,0x0b})

#define VBLEND(x,y)						\
    vec_perm(x,y,(VUC){0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,	\
		       0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f})

#define VUNPACKHI(x,y)						\
    vec_perm(x,y,(VUC){0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,	\
		       0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f})

#define VUNPACKLO(x,y)						\
    vec_perm(x,y,(VUC){0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,	\
		       0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17})

#define VDUPRE(x)						\
    vec_perm(x,x,(VUC){0x00,0x01,0x02,0x03,0x00,0x01,0x02,0x03,	\
		       0x18,0x19,0x1a,0x1b,0x18,0x19,0x1a,0x1b})

#define VDUPIM(x)						\
    vec_perm(x,x,(VUC){0x04,0x05,0x06,0x07,0x04,0x05,0x06,0x07,	\
		       0x1c,0x1d,0x1e,0x1f,0x1c,0x1d,0x1e,0x1f})


static inline V IMUL(V d, V re, V im)
{
    im = VMUL(im, VSWAPPAIRS(d));
    re = VMUL(re, d);
    return VSUB(re, im);  
}


static inline V IMULJ(V d, V re, V im)
{
    im = VMUL(im, VSWAPPAIRS(d));
    return VMULADD(re, d, im);
}

#ifndef __GNUC__
/* gcc (4.6 and 4.7) ICEs on this code! */
static inline V MULI(int inv, V x)
{
    return VXOR(x, inv ? VLIT4(-0.0f,0.0f,-0.0f,0.0f) : VLIT4(0.0f,-0.0f,0.0f,-0.0f));
}
#else
/* but compiles this fine... */
static inline V MULI(int inv, V x)
{
    V t;
    t = inv ? VLIT4(-0.0f,0.0f,-0.0f,0.0f) : VLIT4(0.0f,-0.0f,0.0f,-0.0f);
    return VXOR(x, t);
}
#endif


static inline V IMULI(int inv, V x)
{
    return VSWAPPAIRS(MULI(inv, x));
}


static inline V VLD(const void *s)
{
    V *d = (V *)s;
    return *d;
}


static inline void VST(void *d, V s)
{
    V *r = (V *)d;
    *r = s;
}
#endif
// vim: set autoindent noexpandtab tabstop=3 shiftwidth=3:
