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

#ifndef FFTS_MACROS_H
#define FFTS_MACROS_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#ifdef HAVE_NEON
#include "macros-neon.h"
#elif HAVE_SSE
#include "macros-sse.h"
// NOTE: AltiVec support disabled until updated to provide new V4SF variable type
//#elif __powerpc__
//#include "macros-altivec.h"
#else
#include "macros-alpha.h"
#endif

static FFTS_INLINE void
V4SF_TX2(V4SF *a, V4SF *b)
{
    V4SF t0 = V4SF_UNPACK_LO(*a, *b);
    V4SF t1 = V4SF_UNPACK_HI(*a, *b);
    *a = t0;
    *b = t1;
}

static FFTS_INLINE void
V4SF_K_N(int inv,
         V4SF re,
         V4SF im,
         V4SF *r0,
         V4SF *r1,
         V4SF *r2,
         V4SF *r3)
{
    V4SF uk, uk2, zk_p, zk_n, zk, zk_d;

    uk  = *r0;
    uk2 = *r1;

    zk_p = V4SF_IMUL(*r2, re, im);
    zk_n = V4SF_IMULJ(*r3, re, im);

    zk   = V4SF_ADD(zk_p, zk_n);
    zk_d = V4SF_IMULI(inv, V4SF_SUB(zk_p, zk_n));

    *r2 = V4SF_SUB(uk, zk);
    *r0 = V4SF_ADD(uk, zk);
    *r3 = V4SF_ADD(uk2, zk_d);
    *r1 = V4SF_SUB(uk2, zk_d);
}

static FFTS_INLINE void
V4SF_L_2_4(int inv,
           const float *FFTS_RESTRICT i0,
           const float *FFTS_RESTRICT i1,
           const float *FFTS_RESTRICT i2,
           const float *FFTS_RESTRICT i3,
           V4SF *r0,
           V4SF *r1,
           V4SF *r2,
           V4SF *r3)
{
    V4SF t0, t1, t2, t3, t4, t5, t6, t7;

    t0 = V4SF_LD(i0);
    t1 = V4SF_LD(i1);
    t2 = V4SF_LD(i2);
    t3 = V4SF_LD(i3);

    t4 = V4SF_ADD(t0, t1);
    t5 = V4SF_SUB(t0, t1);
    t6 = V4SF_ADD(t2, t3);
    t7 = V4SF_SUB(t2, t3);

    *r0 = V4SF_UNPACK_LO(t4, t5);
    *r1 = V4SF_UNPACK_LO(t6, t7);

    t5 = V4SF_IMULI(inv, t5);

    t0 = V4SF_ADD(t6, t4);
    t2 = V4SF_SUB(t6, t4);
    t1 = V4SF_SUB(t7, t5);
    t3 = V4SF_ADD(t7, t5);

    *r3 = V4SF_UNPACK_HI(t0, t1);
    *r2 = V4SF_UNPACK_HI(t2, t3);
}

static FFTS_INLINE void
V4SF_L_4_4(int inv,
           const float *FFTS_RESTRICT i0,
           const float *FFTS_RESTRICT i1,
           const float *FFTS_RESTRICT i2,
           const float *FFTS_RESTRICT i3,
           V4SF *r0,
           V4SF *r1,
           V4SF *r2,
           V4SF *r3)
{
    V4SF t0, t1, t2, t3, t4, t5, t6, t7;

    t0 = V4SF_LD(i0);
    t1 = V4SF_LD(i1);
    t2 = V4SF_LD(i2);
    t3 = V4SF_LD(i3);

    t4 = V4SF_ADD(t0, t1);
    t5 = V4SF_SUB(t0, t1);
    t6 = V4SF_ADD(t2, t3);

    t7 = V4SF_IMULI(inv, V4SF_SUB(t2, t3));

    t0 = V4SF_ADD(t4, t6);
    t2 = V4SF_SUB(t4, t6);
    t1 = V4SF_SUB(t5, t7);
    t3 = V4SF_ADD(t5, t7);

    V4SF_TX2(&t0, &t1);
    V4SF_TX2(&t2, &t3);

    *r0 = t0;
    *r2 = t1;
    *r1 = t2;
    *r3 = t3;
}

static FFTS_INLINE void
V4SF_L_4_2(int inv,
           const float *FFTS_RESTRICT i0,
           const float *FFTS_RESTRICT i1,
           const float *FFTS_RESTRICT i2,
           const float *FFTS_RESTRICT i3,
           V4SF *r0,
           V4SF *r1,
           V4SF *r2,
           V4SF *r3)
{
    V4SF t0, t1, t2, t3, t4, t5, t6, t7;

    t0 = V4SF_LD(i0);
    t1 = V4SF_LD(i1);
    t6 = V4SF_LD(i2);
    t7 = V4SF_LD(i3);

    t2 = V4SF_BLEND(t6, t7);
    t3 = V4SF_BLEND(t7, t6);

    t4 = V4SF_ADD(t0, t1);
    t5 = V4SF_SUB(t0, t1);
    t6 = V4SF_ADD(t2, t3);
    t7 = V4SF_SUB(t2, t3);

    *r2 = V4SF_UNPACK_HI(t4, t5);
    *r3 = V4SF_UNPACK_HI(t6, t7);

    t7 = V4SF_IMULI(inv, t7);

    t0 = V4SF_ADD(t4, t6);
    t2 = V4SF_SUB(t4, t6);
    t1 = V4SF_SUB(t5, t7);
    t3 = V4SF_ADD(t5, t7);

    *r0 = V4SF_UNPACK_LO(t0, t1);
    *r1 = V4SF_UNPACK_LO(t2, t3);
}

#define V4SF_S_4(r0, r1, r2, r3, o0, o1, o2, o3) \
    V4SF_ST(o0, r0); V4SF_ST(o1, r1); V4SF_ST(o2, r2); V4SF_ST(o3, r3);

#endif /* FFTS_MACROS_H */
