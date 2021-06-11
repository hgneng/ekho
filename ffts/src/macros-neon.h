/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

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

#ifndef FFTS_MACROS_NEON_H
#define FFTS_MACROS_NEON_H

#include <arm_neon.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

typedef float32x4_t   V4SF;
typedef float32x4x2_t V4SF2;

#define V4SF_ADD vaddq_f32
#define V4SF_SUB vsubq_f32
#define V4SF_MUL vmulq_f32

#define V4SF_XOR(x,y) \
    (vreinterpretq_f32_u32(veorq_u32(vreinterpretq_u32_f32(x), vreinterpretq_u32_f32(y))))

#define V4SF_ST vst1q_f32
#define V4SF_LD vld1q_f32

#define V4SF_SWAP_PAIRS(x) \
    (vrev64q_f32(x))

#define V4SF_UNPACK_HI(a,b) \
    (vcombine_f32(vget_high_f32(a), vget_high_f32(b)))

#define V4SF_UNPACK_LO(a,b) \
    (vcombine_f32(vget_low_f32(a), vget_low_f32(b)))

#define V4SF_BLEND(x,y) \
    (vcombine_f32(vget_low_f32(x), vget_high_f32(y)))

static FFTS_ALWAYS_INLINE V4SF
V4SF_LIT4(float f3, float f2, float f1, float f0)
{
    float FFTS_ALIGN(16) d[4] = {f0, f1, f2, f3};
    return V4SF_LD(d);
}

#define V4SF_DUPLICATE_RE(r) \
    vcombine_f32(vdup_lane_f32(vget_low_f32(r),0), vdup_lane_f32(vget_high_f32(r),0))

#define V4SF_DUPLICATE_IM(r) \
    vcombine_f32(vdup_lane_f32(vget_low_f32(r),1), vdup_lane_f32(vget_high_f32(r),1))

static FFTS_ALWAYS_INLINE V4SF
V4SF_IMULI(int inv, V4SF a)
{
    if (inv) {
        return V4SF_SWAP_PAIRS(V4SF_XOR(a, V4SF_LIT4(0.0f, -0.0f, 0.0f, -0.0f)));
    } else {
        return V4SF_SWAP_PAIRS(V4SF_XOR(a, V4SF_LIT4(-0.0f, 0.0f, -0.0f, 0.0f)));
    }
}

static FFTS_ALWAYS_INLINE V4SF
V4SF_IMUL(V4SF d, V4SF re, V4SF im)
{
  re = V4SF_MUL(re, d);
  im = V4SF_MUL(im, V4SF_SWAP_PAIRS(d));
  return V4SF_SUB(re, im);
}

static FFTS_ALWAYS_INLINE V4SF
V4SF_IMULJ(V4SF d, V4SF re, V4SF im)
{
  re = V4SF_MUL(re, d);
  im = V4SF_MUL(im, V4SF_SWAP_PAIRS(d));
  return V4SF_ADD(re, im);
}

#define V4SF2_ST vst2q_f32
#define V4SF2_LD vld2q_f32

static FFTS_ALWAYS_INLINE void
V4SF2_STORE_SPR(float *addr, V4SF2 p)
{
    vst1q_f32(addr, p.val[0]);
    vst1q_f32(addr + 4, p.val[1]);
}

#endif /* FFTS_MACROS_NEON_H */
