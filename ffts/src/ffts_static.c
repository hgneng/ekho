/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2012, Anthony M. Blake <amb@anthonix.com>
Copyright (c) 2012, The University of Waikato

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

#include "ffts_static.h"

#include "ffts_internal.h"
#include "macros.h"

#if defined(HAVE_NEON)
#include "neon.h"
#endif

#include <assert.h>

static const FFTS_ALIGN(16) float ffts_constants_small_32f[24] = {
     1.0f,
     1.0f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

    -0.0f,
     0.0f,
    -0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

     1.0f,
     1.0f,
     0.9238795325112867561281831893967882868224166258636425f,
     0.9238795325112867561281831893967882868224166258636425f,

    -0.0f,
     0.0f,
    -0.3826834323650897717284599840303988667613445624856270f,
     0.3826834323650897717284599840303988667613445624856270f,

     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.3826834323650897717284599840303988667613445624856270f,
     0.3826834323650897717284599840303988667613445624856270f,

    -0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
    -0.9238795325112867561281831893967882868224166258636425f,
     0.9238795325112867561281831893967882868224166258636425f
};

static const FFTS_ALIGN(16) double ffts_constants_small_64f[24] = {
     1.0,
     1.0,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

    -0.0,
     0.0,
    -0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

     1.0,
     1.0,
     0.9238795325112867561281831893967882868224166258636425,
     0.9238795325112867561281831893967882868224166258636425,

    -0.0,
     0.0,
    -0.3826834323650897717284599840303988667613445624856270,
     0.3826834323650897717284599840303988667613445624856270,

     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
     0.3826834323650897717284599840303988667613445624856270,
     0.3826834323650897717284599840303988667613445624856270,

    -0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
    -0.9238795325112867561281831893967882868224166258636425,
     0.9238795325112867561281831893967882868224166258636425
};

static const FFTS_ALIGN(16) float ffts_constants_small_inv_32f[24] = {
     1.0f,
     1.0f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

     0.0f,
    -0.0f,
     0.7071067811865475244008443621048490392848359376884740f,
    -0.7071067811865475244008443621048490392848359376884740f,

     1.0f,
     1.0f,
     0.9238795325112867561281831893967882868224166258636425f,
     0.9238795325112867561281831893967882868224166258636425f,

     0.0f,
    -0.0f,
     0.3826834323650897717284599840303988667613445624856270f,
    -0.3826834323650897717284599840303988667613445624856270f,

     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.3826834323650897717284599840303988667613445624856270f,
     0.3826834323650897717284599840303988667613445624856270f,

     0.7071067811865475244008443621048490392848359376884740f,
    -0.7071067811865475244008443621048490392848359376884740f,
     0.9238795325112867561281831893967882868224166258636425f,
    -0.9238795325112867561281831893967882868224166258636425f
};

static const FFTS_ALIGN(16) double ffts_constants_small_inv_64f[24] = {
     1.0,
     1.0,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

     0.0,
    -0.0,
     0.7071067811865475244008443621048490392848359376884740,
    -0.7071067811865475244008443621048490392848359376884740,

     1.0,
     1.0,
     0.9238795325112867561281831893967882868224166258636425,
     0.9238795325112867561281831893967882868224166258636425,

     0.0,
    -0.0,
     0.3826834323650897717284599840303988667613445624856270,
    -0.3826834323650897717284599840303988667613445624856270,

     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
     0.3826834323650897717284599840303988667613445624856270,
     0.3826834323650897717284599840303988667613445624856270,

     0.7071067811865475244008443621048490392848359376884740,
    -0.7071067811865475244008443621048490392848359376884740,
     0.9238795325112867561281831893967882868224166258636425,
    -0.9238795325112867561281831893967882868224166258636425
};

static const FFTS_ALIGN(16) float ffts_constants_32f[16] = {
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

    -0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
    -0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

     1.0f,
     1.0f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

     0.0f,
     0.0f,
    -0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f
};

static const FFTS_ALIGN(16) double ffts_constants_64f[16] = {
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

    -0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
    -0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

     1.0,
     1.0,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

     0.0,
     0.0,
    -0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740
};

static const FFTS_ALIGN(16) float ffts_constants_inv_32f[16] = {
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

     0.7071067811865475244008443621048490392848359376884740f,
    -0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,
    -0.7071067811865475244008443621048490392848359376884740f,

     1.0f,
     1.0f,
     0.7071067811865475244008443621048490392848359376884740f,
     0.7071067811865475244008443621048490392848359376884740f,

     0.0f,
     0.0f,
     0.7071067811865475244008443621048490392848359376884740f,
    -0.7071067811865475244008443621048490392848359376884740f
};

static const FFTS_ALIGN(16) double ffts_constants_inv_64f[16] = {
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

     0.7071067811865475244008443621048490392848359376884740,
    -0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,
    -0.7071067811865475244008443621048490392848359376884740,

     1.0,
     1.0,
     0.7071067811865475244008443621048490392848359376884740,
     0.7071067811865475244008443621048490392848359376884740,

     0.0,
     0.0,
     0.7071067811865475244008443621048490392848359376884740,
    -0.7071067811865475244008443621048490392848359376884740
};

static FFTS_INLINE void
V4SF_K_0(int inv,
         V4SF *r0,
         V4SF *r1,
         V4SF *r2,
         V4SF *r3)
{
    V4SF t0, t1, t2, t3;

    t0 = *r0;
    t1 = *r1;

    t2 = V4SF_ADD(*r2, *r3);
    t3 = V4SF_IMULI(inv, V4SF_SUB(*r2, *r3));

    *r0 = V4SF_ADD(t0, t2);
    *r2 = V4SF_SUB(t0, t2);
    *r1 = V4SF_SUB(t1, t3);
    *r3 = V4SF_ADD(t1, t3);
}

static FFTS_INLINE void
V4SF_L_2(const float *FFTS_RESTRICT i0,
         const float *FFTS_RESTRICT i1,
         const float *FFTS_RESTRICT i2,
         const float *FFTS_RESTRICT i3,
         V4SF *r0,
         V4SF *r1,
         V4SF *r2,
         V4SF *r3)
{
    V4SF t0, t1, t2, t3;

    t0 = V4SF_LD(i0);
    t1 = V4SF_LD(i1);
    t2 = V4SF_LD(i2);
    t3 = V4SF_LD(i3);

    *r0 = V4SF_ADD(t0, t1);
    *r1 = V4SF_SUB(t0, t1);
    *r2 = V4SF_ADD(t2, t3);
    *r3 = V4SF_SUB(t2, t3);
}

static FFTS_INLINE void
V4SF_L_4(int inv,
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

    *r0 = V4SF_ADD(t4, t6);
    *r2 = V4SF_SUB(t4, t6);
    *r1 = V4SF_SUB(t5, t7);
    *r3 = V4SF_ADD(t5, t7);
}

static FFTS_INLINE void
V4SF_LEAF_EE(float *const FFTS_RESTRICT out,
             const ptrdiff_t *FFTS_RESTRICT os,
             const float     *FFTS_RESTRICT in,
             const ptrdiff_t *FFTS_RESTRICT is,
             int inv)
{
    const float *FFTS_RESTRICT LUT = inv ? ffts_constants_inv_32f : ffts_constants_32f;

    V4SF r0, r1, r2, r3, r4, r5, r6, r7;

    float *out0 = out + os[0];
    float *out1 = out + os[1];

    V4SF_L_4(inv, in + is[0], in + is[1], in + is[2], in + is[3], &r0, &r1, &r2, &r3);
    V4SF_L_2(in + is[4], in + is[5], in + is[6], in + is[7], &r4, &r5, &r6, &r7);

    V4SF_K_0(inv, &r0, &r2, &r4, &r6);
    V4SF_K_N(inv, V4SF_LD(LUT + 0), V4SF_LD(LUT + 4), &r1, &r3, &r5, &r7);
    V4SF_TX2(&r0, &r1);
    V4SF_TX2(&r2, &r3);
    V4SF_TX2(&r4, &r5);
    V4SF_TX2(&r6, &r7);

    V4SF_S_4(r0, r2, r4, r6, out0 + 0, out0 + 4, out0 + 8, out0 + 12);
    V4SF_S_4(r1, r3, r5, r7, out1 + 0, out1 + 4, out1 + 8, out1 + 12);
}

static FFTS_INLINE void
V4SF_LEAF_EE2(float *const FFTS_RESTRICT out,
              const ptrdiff_t *FFTS_RESTRICT os,
              const float *FFTS_RESTRICT in,
              const ptrdiff_t *FFTS_RESTRICT is,
              int inv)
{
    const float *FFTS_RESTRICT LUT = inv ? ffts_constants_inv_32f : ffts_constants_32f;

    V4SF r0, r1, r2, r3, r4, r5, r6, r7;

    float *out0 = out + os[0];
    float *out1 = out + os[1];

    V4SF_L_4(inv, in + is[6], in + is[7], in + is[4], in + is[5], &r0, &r1, &r2, &r3);
    V4SF_L_2(in + is[0], in + is[1], in + is[3], in + is[2], &r4, &r5, &r6, &r7);

    V4SF_K_0(inv, &r0, &r2, &r4, &r6);
    V4SF_K_N(inv, V4SF_LD(LUT + 0), V4SF_LD(LUT + 4), &r1, &r3, &r5, &r7);
    V4SF_TX2(&r0, &r1);
    V4SF_TX2(&r2, &r3);
    V4SF_TX2(&r4, &r5);
    V4SF_TX2(&r6, &r7);

    V4SF_S_4(r0, r2, r4, r6, out0 + 0, out0 + 4, out0 + 8, out0 + 12);
    V4SF_S_4(r1, r3, r5, r7, out1 + 0, out1 + 4, out1 + 8, out1 + 12);
}

static FFTS_INLINE void
V4SF_LEAF_EO(float *const FFTS_RESTRICT out,
             const ptrdiff_t *FFTS_RESTRICT os,
             const float *FFTS_RESTRICT in,
             const ptrdiff_t *FFTS_RESTRICT is,
             int inv)
{
    const float *FFTS_RESTRICT LUT = inv ? ffts_constants_inv_32f : ffts_constants_32f;

    V4SF r0, r1, r2, r3, r4, r5, r6, r7;

    float *out0 = out + os[0];
    float *out1 = out + os[1];

    V4SF_L_4_4(inv, in + is[0], in + is[1], in + is[2], in + is[3], &r0, &r1, &r2, &r3);
    V4SF_L_2_4(inv, in + is[4], in + is[5], in + is[6], in + is[7], &r4, &r5, &r6, &r7);

    V4SF_S_4(r2, r3, r7, r6, out1 + 0, out1 + 4, out1 + 8, out1 + 12);
    V4SF_K_N(inv, V4SF_LD(LUT + 8), V4SF_LD(LUT + 12), &r0, &r1, &r4, &r5);
    V4SF_S_4(r0, r1, r4, r5, out0 + 0, out0 + 4, out0 + 8, out0 + 12);
}

static FFTS_INLINE void
V4SF_LEAF_OE(float *const FFTS_RESTRICT out,
             const ptrdiff_t *FFTS_RESTRICT os,
             const float *FFTS_RESTRICT in,
             const ptrdiff_t *FFTS_RESTRICT is,
             int inv)
{
    const float *FFTS_RESTRICT LUT = inv ? ffts_constants_inv_32f : ffts_constants_32f;

    V4SF r0, r1, r2, r3, r4, r5, r6, r7;

    float *out0 = out + os[0];
    float *out1 = out + os[1];

    V4SF_L_4_2(inv, in + is[0], in + is[1], in + is[2], in + is[3], &r0, &r1, &r2, &r3);
    V4SF_L_4_4(inv, in + is[6], in + is[7], in + is[4], in + is[5], &r4, &r5, &r6, &r7);

    V4SF_S_4(r0, r1, r4, r5, out0 + 0, out0 + 4, out0 + 8, out0 + 12);
    V4SF_K_N(inv, V4SF_LD(LUT + 8), V4SF_LD(LUT + 12), &r6, &r7, &r2, &r3);
    V4SF_S_4(r6, r7, r2, r3, out1 + 0, out1 + 4, out1 + 8, out1 + 12);
}

static FFTS_INLINE void
V4SF_LEAF_OO(float *const FFTS_RESTRICT out,
             const ptrdiff_t *FFTS_RESTRICT os,
             const float *FFTS_RESTRICT in,
             const ptrdiff_t *FFTS_RESTRICT is,
             int inv)
{
    V4SF r0, r1, r2, r3, r4, r5, r6, r7;

    float *out0 = out + os[0];
    float *out1 = out + os[1];

    V4SF_L_4_4(inv, in + is[0], in + is[1], in + is[2], in + is[3], &r0, &r1, &r2, &r3);
    V4SF_L_4_4(inv, in + is[6], in + is[7], in + is[4], in + is[5], &r4, &r5, &r6, &r7);

    V4SF_S_4(r0, r1, r4, r5, out0 + 0, out0 + 4, out0 + 8, out0 + 12);
    V4SF_S_4(r2, r3, r6, r7, out1 + 0, out1 + 4, out1 + 8, out1 + 12);
}

static FFTS_INLINE void
V4SF_X_4(int inv,
         float *FFTS_RESTRICT data,
         size_t N,
         const float *FFTS_RESTRICT LUT)
{
    size_t i;

    for (i = 0; i < N/8; i++) {
        V4SF r0 = V4SF_LD(data);
        V4SF r1 = V4SF_LD(data + 2*N/4);
        V4SF r2 = V4SF_LD(data + 4*N/4);
        V4SF r3 = V4SF_LD(data + 6*N/4);

        V4SF_K_N(inv, V4SF_LD(LUT), V4SF_LD(LUT + 4), &r0, &r1, &r2, &r3);

        V4SF_ST(data        , r0);
        V4SF_ST(data + 2*N/4, r1);
        V4SF_ST(data + 4*N/4, r2);
        V4SF_ST(data + 6*N/4, r3);

        LUT += 8;
        data += 4;
    }
}

static FFTS_INLINE void
V4SF_X_8(int inv,
         float *FFTS_RESTRICT data0,
         size_t N,
         const float *FFTS_RESTRICT LUT)
{
    float *data1 = data0 + 1*N/4;
    float *data2 = data0 + 2*N/4;
    float *data3 = data0 + 3*N/4;
    float *data4 = data0 + 4*N/4;
    float *data5 = data0 + 5*N/4;
    float *data6 = data0 + 6*N/4;
    float *data7 = data0 + 7*N/4;
    size_t i;

    for (i = 0; i < N/16; i++) {
        V4SF r0, r1, r2, r3, r4, r5, r6, r7;

        r0 = V4SF_LD(data0);
        r1 = V4SF_LD(data1);
        r2 = V4SF_LD(data2);
        r3 = V4SF_LD(data3);

        V4SF_K_N(inv, V4SF_LD(LUT), V4SF_LD(LUT + 4), &r0, &r1, &r2, &r3);
        r4 = V4SF_LD(data4);
        r6 = V4SF_LD(data6);

        V4SF_K_N(inv, V4SF_LD(LUT + 8), V4SF_LD(LUT + 12), &r0, &r2, &r4, &r6);
        r5 = V4SF_LD(data5);
        r7 = V4SF_LD(data7);

        V4SF_K_N(inv, V4SF_LD(LUT + 16), V4SF_LD(LUT + 20), &r1, &r3, &r5, &r7);
        LUT += 24;

        V4SF_ST(data0, r0);
        data0 += 4;

        V4SF_ST(data1, r1);
        data1 += 4;

        V4SF_ST(data2, r2);
        data2 += 4;

        V4SF_ST(data3, r3);
        data3 += 4;

        V4SF_ST(data4, r4);
        data4 += 4;

        V4SF_ST(data5, r5);
        data5 += 4;

        V4SF_ST(data6, r6);
        data6 += 4;

        V4SF_ST(data7, r7);
        data7 += 4;
    }
}

static FFTS_INLINE void
ffts_static_firstpass_odd_32f(float *const FFTS_RESTRICT out,
                              const float *FFTS_RESTRICT in,
                              const ffts_plan_t *FFTS_RESTRICT p,
                              int inv)
{
    size_t i, i0 = p->i0, i1 = p->i1;
    const ptrdiff_t *is = (const ptrdiff_t*) p->is;
    const ptrdiff_t *os = (const ptrdiff_t*) p->offsets;

    for (i = i0; i > 0; --i) {
        V4SF_LEAF_EE(out, os, in, is, inv);
        in += 4;
        os += 2;
    }

    for (i = i1; i > 0; --i) {
        V4SF_LEAF_OO(out, os, in, is, inv);
        in += 4;
        os += 2;
    }

    V4SF_LEAF_OE(out, os, in, is, inv);
    in += 4;
    os += 2;

    for (i = i1; i > 0; --i) {
        V4SF_LEAF_EE2(out, os, in, is, inv);
        in += 4;
        os += 2;
    }
}

void
ffts_small_2_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *din = (const float*) in;
    float *dout = (float*) out;
    ffts_cpx_32f t0, t1, r0, r1;

    /* unreferenced parameter */
    (void) p;

    t0[0] = din[0];
    t0[1] = din[1];
    t1[0] = din[2];
    t1[1] = din[3];

    r0[0] = t0[0] + t1[0];
    r0[1] = t0[1] + t1[1];
    r1[0] = t0[0] - t1[0];
    r1[1] = t0[1] - t1[1];

    dout[0] = r0[0];
    dout[1] = r0[1];
    dout[2] = r1[0];
    dout[3] = r1[1];
}

void
ffts_small_2_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
    ffts_cpx_64f t0, t1, r0, r1;

    /* unreferenced parameter */
    (void) p;

    t0[0] = din[0];
    t0[1] = din[1];
    t1[0] = din[2];
    t1[1] = din[3];

    r0[0] = t0[0] + t1[0];
    r0[1] = t0[1] + t1[1];
    r1[0] = t0[0] - t1[0];
    r1[1] = t0[1] - t1[1];

    dout[0] = r0[0];
    dout[1] = r0[1];
    dout[2] = r1[0];
    dout[3] = r1[1];
}

void
ffts_small_forward4_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *din = (const float*) in;
    float *dout = (float*) out;
    ffts_cpx_32f t0, t1, t2, t3, t4, t5, t6, t7;

    /* unreferenced parameter */
    (void) p;

    t0[0] = din[0];
    t0[1] = din[1];
    t1[0] = din[4];
    t1[1] = din[5];
    t2[0] = din[2];
    t2[1] = din[3];
    t3[0] = din[6];
    t3[1] = din[7];

    t4[0] = t0[0] + t1[0];
    t4[1] = t0[1] + t1[1];
    t5[0] = t0[0] - t1[0];
    t5[1] = t0[1] - t1[1];
    t6[0] = t2[0] + t3[0];
    t6[1] = t2[1] + t3[1];
    t7[0] = t2[0] - t3[0];
    t7[1] = t2[1] - t3[1];

    dout[0] = t4[0] + t6[0];
    dout[1] = t4[1] + t6[1];
    dout[4] = t4[0] - t6[0];
    dout[5] = t4[1] - t6[1];
    dout[2] = t5[0] + t7[1];
    dout[3] = t5[1] - t7[0];
    dout[6] = t5[0] - t7[1];
    dout[7] = t5[1] + t7[0];
}

void
ffts_small_forward4_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
    ffts_cpx_64f t0, t1, t2, t3, t4, t5, t6, t7;

    /* unreferenced parameter */
    (void) p;

    t0[0] = din[0];
    t0[1] = din[1];
    t1[0] = din[4];
    t1[1] = din[5];
    t2[0] = din[2];
    t2[1] = din[3];
    t3[0] = din[6];
    t3[1] = din[7];

    t4[0] = t0[0] + t1[0];
    t4[1] = t0[1] + t1[1];
    t5[0] = t0[0] - t1[0];
    t5[1] = t0[1] - t1[1];
    t6[0] = t2[0] + t3[0];
    t6[1] = t2[1] + t3[1];
    t7[0] = t2[0] - t3[0];
    t7[1] = t2[1] - t3[1];

    dout[0] = t4[0] + t6[0];
    dout[1] = t4[1] + t6[1];
    dout[4] = t4[0] - t6[0];
    dout[5] = t4[1] - t6[1];
    dout[2] = t5[0] + t7[1];
    dout[3] = t5[1] - t7[0];
    dout[6] = t5[0] - t7[1];
    dout[7] = t5[1] + t7[0];
}

void
ffts_small_backward4_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *din = (const float*) in;
    float *dout = (float*) out;
    ffts_cpx_32f t0, t1, t2, t3, t4, t5, t6, t7;

    /* unreferenced parameter */
    (void) p;

    t0[0] = din[0];
    t0[1] = din[1];
    t1[0] = din[4];
    t1[1] = din[5];
    t2[0] = din[2];
    t2[1] = din[3];
    t3[0] = din[6];
    t3[1] = din[7];

    t4[0] = t0[0] + t1[0];
    t4[1] = t0[1] + t1[1];
    t5[0] = t0[0] - t1[0];
    t5[1] = t0[1] - t1[1];
    t6[0] = t2[0] + t3[0];
    t6[1] = t2[1] + t3[1];
    t7[0] = t2[0] - t3[0];
    t7[1] = t2[1] - t3[1];

    dout[0] = t4[0] + t6[0];
    dout[1] = t4[1] + t6[1];
    dout[4] = t4[0] - t6[0];
    dout[5] = t4[1] - t6[1];
    dout[2] = t5[0] - t7[1];
    dout[3] = t5[1] + t7[0];
    dout[6] = t5[0] + t7[1];
    dout[7] = t5[1] - t7[0];
}

void
ffts_small_backward4_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
    ffts_cpx_64f t0, t1, t2, t3, t4, t5, t6, t7;

    /* unreferenced parameter */
    (void) p;

    t0[0] = din[0];
    t0[1] = din[1];
    t1[0] = din[4];
    t1[1] = din[5];
    t2[0] = din[2];
    t2[1] = din[3];
    t3[0] = din[6];
    t3[1] = din[7];

    t4[0] = t0[0] + t1[0];
    t4[1] = t0[1] + t1[1];
    t5[0] = t0[0] - t1[0];
    t5[1] = t0[1] - t1[1];
    t6[0] = t2[0] + t3[0];
    t6[1] = t2[1] + t3[1];
    t7[0] = t2[0] - t3[0];
    t7[1] = t2[1] - t3[1];

    dout[0] = t4[0] + t6[0];
    dout[1] = t4[1] + t6[1];
    dout[4] = t4[0] - t6[0];
    dout[5] = t4[1] - t6[1];
    dout[2] = t5[0] - t7[1];
    dout[3] = t5[1] + t7[0];
    dout[6] = t5[0] + t7[1];
    dout[7] = t5[1] - t7[0];
}

void
ffts_small_forward8_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *FFTS_RESTRICT lut = ffts_constants_small_32f;
    const float *din = (const float*) in;
    float *dout = (float*) out;
    V4SF r0_1, r2_3, r4_5, r6_7;
    
    /* unreferenced parameter */
    (void) p;

    V4SF_L_4_2(0, din, din+8, din+4, din+12, &r0_1, &r2_3, &r4_5, &r6_7);
    V4SF_K_N(0, V4SF_LD(lut), V4SF_LD(lut + 4), &r0_1, &r2_3, &r4_5, &r6_7);
    V4SF_S_4(r0_1, r2_3, r4_5, r6_7, dout+0, dout+4, dout+8, dout+12);
}

void
ffts_small_forward8_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
//  V4SF r0_1, r2_3, r4_5, r6_7;
//  double *LUT8 = (double*) p->ws + p->ws_is[0];
    (void) p;
    (void) din;
    (void) dout;

#if MACROS_READY
    L_4_2(0, din, din+8, din+4, din+12, &r0_1, &r2_3, &r4_5, &r6_7);
    K_N(0, VLD(LUT8), VLD(LUT8+4), &r0_1, &r2_3, &r4_5, &r6_7);
    S_4(r0_1, r2_3, r4_5, r6_7, dout+0, dout+4, dout+8, dout+12);
#endif
}

void
ffts_small_backward8_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *FFTS_RESTRICT lut = ffts_constants_small_inv_32f;
    const float *din = (const float*) in;
    float *dout = (float*) out;
    V4SF r0_1, r2_3, r4_5, r6_7;

    /* unreferenced parameter */
    (void) p;

    V4SF_L_4_2(1, din, din+8, din+4, din+12, &r0_1, &r2_3, &r4_5, &r6_7);
    V4SF_K_N(1, V4SF_LD(lut), V4SF_LD(lut+4), &r0_1, &r2_3, &r4_5, &r6_7);
    V4SF_S_4(r0_1, r2_3, r4_5, r6_7, dout+0, dout+4, dout+8, dout+12);
}

void
ffts_small_backward8_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
//  V4SF r0_1, r2_3, r4_5, r6_7;
//  double *LUT8 = (double*) p->ws + p->ws_is[0];
    (void) p;
    (void) din;
    (void) dout;


#if MACROS_READY
    L_4_2(1, din, din+8, din+4, din+12, &r0_1, &r2_3, &r4_5, &r6_7);
    K_N(1, VLD(LUT8), VLD(LUT8+4), &r0_1, &r2_3, &r4_5, &r6_7);
    S_4(r0_1, r2_3, r4_5, r6_7, dout+0, dout+4, dout+8, dout+12);
#endif
}

void
ffts_small_forward16_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *FFTS_RESTRICT lut = ffts_constants_small_32f;
    const float *din = (const float*) in;
    float *dout = (float*) out;
    V4SF r0_1, r2_3, r4_5, r6_7, r8_9, r10_11, r12_13, r14_15;

    /* unreferenced parameter */
    (void) p;

    V4SF_L_4_4(0, din+0, din+16, din+8, din+24, &r0_1, &r2_3, &r8_9, &r10_11);
    V4SF_L_2_4(0, din+4, din+20, din+28, din+12, &r4_5, &r6_7, &r14_15, &r12_13);
    V4SF_K_N(0, V4SF_LD(lut), V4SF_LD(lut+4), &r0_1, &r2_3, &r4_5, &r6_7);
    V4SF_K_N(0, V4SF_LD(lut+8), V4SF_LD(lut+12), &r0_1, &r4_5, &r8_9, &r12_13);
    V4SF_S_4(r0_1, r4_5, r8_9, r12_13, dout+0, dout+8, dout+16, dout+24);
    V4SF_K_N(0, V4SF_LD(lut+16), V4SF_LD(lut+20), &r2_3, &r6_7, &r10_11, &r14_15);
    V4SF_S_4(r2_3, r6_7, r10_11, r14_15, dout+4, dout+12, dout+20, dout+28);
}

void
ffts_small_forward16_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
//  double *LUT8 = (double*) p->ws;
//  V4SF r0_1, r2_3, r4_5, r6_7, r8_9, r10_11, r12_13, r14_15;
    (void) p;
    (void) din;
    (void) dout;

#ifdef MACROS_READY
    L_4_4(0, din+0, din+16, din+8, din+24, &r0_1, &r2_3, &r8_9, &r10_11);
    L_2_4(0, din+4, din+20, din+28, din+12, &r4_5, &r6_7, &r14_15, &r12_13);
    K_N(0, VLD(LUT8), VLD(LUT8+4), &r0_1, &r2_3, &r4_5, &r6_7);
    K_N(0, VLD(LUT8+8), VLD(LUT8+12), &r0_1, &r4_5, &r8_9, &r12_13);
    S_4(r0_1, r4_5, r8_9, r12_13, dout+0, dout+8, dout+16, dout+24);
    K_N(0, VLD(LUT8+16), VLD(LUT8+20), &r2_3, &r6_7, &r10_11, &r14_15);
    S_4(r2_3, r6_7, r10_11, r14_15, dout+4, dout+12, dout+20, dout+28);
#endif
}

void
ffts_small_backward16_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *FFTS_RESTRICT lut = ffts_constants_small_inv_32f;
    const float *din = (const float*) in;
    float *dout = (float*) out;
    V4SF r0_1, r2_3, r4_5, r6_7, r8_9, r10_11, r12_13, r14_15;
    
    /* unreferenced parameter */
    (void) p;

    V4SF_L_4_4(1, din+0, din+16, din+8, din+24, &r0_1, &r2_3, &r8_9, &r10_11);
    V4SF_L_2_4(1, din+4, din+20, din+28, din+12, &r4_5, &r6_7, &r14_15, &r12_13);
    V4SF_K_N(1, V4SF_LD(lut), V4SF_LD(lut+4), &r0_1, &r2_3, &r4_5, &r6_7);
    V4SF_K_N(1, V4SF_LD(lut+8), V4SF_LD(lut+12), &r0_1, &r4_5, &r8_9, &r12_13);
    V4SF_S_4(r0_1, r4_5, r8_9, r12_13, dout+0, dout+8, dout+16, dout+24);
    V4SF_K_N(1, V4SF_LD(lut+16), V4SF_LD(lut+20), &r2_3, &r6_7, &r10_11, &r14_15);
    V4SF_S_4(r2_3, r6_7, r10_11, r14_15, dout+4, dout+12, dout+20, dout+28);
}

void
ffts_small_backward16_64f(ffts_plan_t *p, const void *in, void *out)
{
    const double *din = (const double*) in;
    double *dout = (double*) out;
//  double *LUT8 = (double*) p->ws;
//  V4SF r0_1, r2_3, r4_5, r6_7, r8_9, r10_11, r12_13, r14_15;
    (void) p;
    (void) din;
    (void) dout;

#ifdef MACROS_READY
    L_4_4(1, din+0, din+16, din+8, din+24, &r0_1, &r2_3, &r8_9, &r10_11);
    L_2_4(1, din+4, din+20, din+28, din+12, &r4_5, &r6_7, &r14_15, &r12_13);
    K_N(1, VLD(LUT8), VLD(LUT8+4), &r0_1, &r2_3, &r4_5, &r6_7);
    K_N(1, VLD(LUT8+8), VLD(LUT8+12),&r0_1, &r4_5, &r8_9, &r12_13);
    S_4(r0_1, r4_5, r8_9, r12_13, dout+0, dout+8, dout+16, dout+24);
    K_N(1, VLD(LUT8+16), VLD(LUT8+20), &r2_3, &r6_7, &r10_11, &r14_15);
    S_4(r2_3, r6_7, r10_11, r14_15, dout+4, dout+12, dout+20, dout+28);
#endif
}

static FFTS_INLINE void
ffts_static_firstpass_even_32f(float *FFTS_RESTRICT out,
                               const float *FFTS_RESTRICT in,
                               const ffts_plan_t *FFTS_RESTRICT p,
                               int inv)
{
    size_t i, i0 = p->i0, i1 = p->i1;
    const ptrdiff_t *is = (const ptrdiff_t*) p->is;
    const ptrdiff_t *os = (const ptrdiff_t*) p->offsets;

    for(i = i0; i > 0; --i) {
        V4SF_LEAF_EE(out, os, in, is, inv);
        in += 4;
        os += 2;
    }

    V4SF_LEAF_EO(out, os, in, is, inv);
    in += 4;
    os += 2;

    for (i = i1; i > 0; --i) {
        V4SF_LEAF_OO(out, os, in, is, inv);
        in += 4;
        os += 2;
    }

    for (i = i1; i > 0; --i) {
        V4SF_LEAF_EE2(out, os, in, is, inv);
        in += 4;
        os += 2;
    }
}

static void
ffts_static_rec_f_32f(const ffts_plan_t *p, float *data, size_t N)
{
    const float *ws = (const float*) p->ws;

#if defined(HAVE_NEON) && defined(DYNAMIC_DISABLED)
    if (N > 128) {
        const size_t N1 = N >> 1;
        const size_t N2 = N >> 2;
        const size_t N3 = N >> 3;

        ffts_static_rec_f_32f(p, data              , N2);
        ffts_static_rec_f_32f(p, data     + N1     , N3);
        ffts_static_rec_f_32f(p, data     + N1 + N2, N3);
        ffts_static_rec_f_32f(p, data + N          , N2);
        ffts_static_rec_f_32f(p, data + N + N1     , N2);

        neon_static_x8_f(data, N, ws + (p->ws_is[ffts_ctzl(N) - 4] << 1));
    } else if (N == 128) {
        const float *ws1 = ws + (p->ws_is[1] << 1);

        neon_static_x8_f(data,  32, ws1);

        neon_static_x4_f(data +  64, ws);
        neon_static_x4_f(data +  96, ws);

        neon_static_x8_f(data + 128,  32, ws1);
        neon_static_x8_f(data + 192,  32, ws1);

        neon_static_x8_f(data, 128, ws + (p->ws_is[3] << 1));
    } else if (N == 64) {
        neon_static_x4_f(data     , ws);
        neon_static_x4_f(data + 64, ws);
        neon_static_x4_f(data + 96, ws);

        neon_static_x8_f(data, 64, ws + (p->ws_is[2] << 1));
    } else {
        assert(N == 32);
        neon_static_x8_f(data, 32, ws + (p->ws_is[1] << 1));
    }
#else
    if (N > 128) {
        const size_t N1 = N >> 1;
        const size_t N2 = N >> 2;
        const size_t N3 = N >> 3;

        ffts_static_rec_f_32f(p, data              , N2);
        ffts_static_rec_f_32f(p, data +     N1     , N3);
        ffts_static_rec_f_32f(p, data +     N1 + N2, N3);
        ffts_static_rec_f_32f(p, data + N          , N2);
        ffts_static_rec_f_32f(p, data + N + N1     , N2);

        V4SF_X_8(0, data, N, ws + (p->ws_is[ffts_ctzl(N) - 4] << 1));
    } else if (N == 128) {
        const float *ws1 = ws + (p->ws_is[1] << 1);

        V4SF_X_8(0, data +   0,  32, ws1);
        V4SF_X_4(0, data +  64,  16, ws);
        V4SF_X_4(0, data +  96,  16, ws);
        V4SF_X_8(0, data + 128,  32, ws1);
        V4SF_X_8(0, data + 192,  32, ws1);

        V4SF_X_8(0, data, 128, ws + (p->ws_is[3] << 1));
    } else if (N == 64) {
        V4SF_X_4(0, data +  0, 16, ws);
        V4SF_X_4(0, data + 64, 16, ws);
        V4SF_X_4(0, data + 96, 16, ws);

        V4SF_X_8(0, data, 64, ws + (p->ws_is[2] << 1));
    } else {
        assert(N == 32);
        V4SF_X_8(0, data, 32, ws + (p->ws_is[1] << 1));
    }
#endif
}

static void
ffts_static_rec_i_32f(const ffts_plan_t *p, float *data, size_t N)
{
    const float *ws = (const float*) p->ws;

#if defined(HAVE_NEON) && defined(DYNAMIC_DISABLED)
    if (N > 128) {
        const size_t N1 = N >> 1;
        const size_t N2 = N >> 2;
        const size_t N3 = N >> 3;

        ffts_static_rec_i_32f(p, data              , N2);
        ffts_static_rec_i_32f(p, data     + N1     , N3);
        ffts_static_rec_i_32f(p, data     + N1 + N2, N3);
        ffts_static_rec_i_32f(p, data + N          , N2);
        ffts_static_rec_i_32f(p, data + N + N1     , N2);

        neon_static_x8_i(data, N, ws + (p->ws_is[ffts_ctzl(N) - 4] << 1));
    } else if (N == 128) {
        const float *ws1 = ws + (p->ws_is[1] << 1);

        neon_static_x8_i(data,  32, ws1);

        neon_static_x4_i(data +  64, ws);
        neon_static_x4_i(data +  96, ws);

        neon_static_x8_i(data + 128,  32, ws1);
        neon_static_x8_i(data + 192,  32, ws1);

        neon_static_x8_i(data, 128, ws + (p->ws_is[3] << 1));
    } else if (N == 64) {
        neon_static_x4_i(data     , ws);
        neon_static_x4_i(data + 64, ws);
        neon_static_x4_i(data + 96, ws);

        neon_static_x8_i(data, 64, ws + (p->ws_is[2] << 1));
    } else {
        assert(N == 32);
        neon_static_x8_i(data, 32, ws + (p->ws_is[1] << 1));
    }
#else
    if (N > 128) {
        const size_t N1 = N >> 1;
        const size_t N2 = N >> 2;
        const size_t N3 = N >> 3;

        ffts_static_rec_i_32f(p, data              , N2);
        ffts_static_rec_i_32f(p, data +     N1     , N3);
        ffts_static_rec_i_32f(p, data +     N1 + N2, N3);
        ffts_static_rec_i_32f(p, data + N          , N2);
        ffts_static_rec_i_32f(p, data + N + N1     , N2);

        V4SF_X_8(1, data, N, ws + (p->ws_is[ffts_ctzl(N) - 4] << 1));
    } else if (N == 128) {
        const float *ws1 = ws + (p->ws_is[1] << 1);

        V4SF_X_8(1, data +   0, 32, ws1);
        V4SF_X_4(1, data +  64, 16, ws);
        V4SF_X_4(1, data +  96, 16, ws);
        V4SF_X_8(1, data + 128, 32, ws1);
        V4SF_X_8(1, data + 192, 32, ws1);

        V4SF_X_8(1, data, 128, ws + (p->ws_is[3] << 1));
    } else if (N == 64) {
        V4SF_X_4(1, data +  0, 16, ws);
        V4SF_X_4(1, data + 64, 16, ws);
        V4SF_X_4(1, data + 96, 16, ws);

        V4SF_X_8(1, data, 64, ws + (p->ws_is[2] << 1));
    } else {
        assert(N == 32);
        V4SF_X_8(1, data, 32, ws + (p->ws_is[1] << 1));
    }
#endif
}

void
ffts_static_transform_f_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *din = (const float*) in;
    float *dout = (float*) out;

    const size_t N = p->N;
    const int N_log_2 = ffts_ctzl(N);

#if defined(HAVE_NEON) && defined(DYNAMIC_DISABLED)
    const float *ws = (const float*) p->ws;

    if (N_log_2 & 1) {
        neon_static_o_f(p, din, dout);
    } else {
        neon_static_e_f(p, din, dout);
    }

    if (N > 128) {
        const size_t N1 = N >> 1;
        const size_t N2 = N >> 2;
        const size_t N3 = N >> 3;

        ffts_static_rec_f_32f(p, dout              , N2);
        ffts_static_rec_f_32f(p, dout     + N1     , N3);
        ffts_static_rec_f_32f(p, dout     + N1 + N2, N3);
        ffts_static_rec_f_32f(p, dout + N          , N2);
        ffts_static_rec_f_32f(p, dout + N + N1     , N2);

        neon_static_x8_t_f(dout, N, ws + (p->ws_is[N_log_2 - 4] << 1));
    } else if (N == 128) {
        neon_static_x8_f(dout, 32, ws + 8);

        neon_static_x4_f(dout +  64, ws);
        neon_static_x4_f(dout +  96, ws);

        neon_static_x8_f(dout + 128, 32, ws + 8);
        neon_static_x8_f(dout + 192, 32, ws + 8);

        neon_static_x8_t_f(dout, 128, ws + 80);
    } else if (N == 64) {
        neon_static_x4_f(dout     , ws);
        neon_static_x4_f(dout + 64, ws);
        neon_static_x4_f(dout + 96, ws);

        neon_static_x8_t_f(dout, 64, ws + 32);
    } else {
        assert(N == 32);
        neon_static_x8_t_f(dout, 32, ws + 8);
    }
#else
    if (N_log_2 & 1) {
        ffts_static_firstpass_odd_32f(dout, din, p, 0);
    } else {
        ffts_static_firstpass_even_32f(dout, din, p, 0);
    }

    ffts_static_rec_f_32f(p, dout, N);
#endif
}

void
ffts_static_transform_i_32f(ffts_plan_t *p, const void *in, void *out)
{
    const float *din = (const float*) in;
    float *dout = (float*) out;

    const size_t N = p->N;
    const int N_log_2 = ffts_ctzl(N);

#if defined(HAVE_NEON) && defined(DYNAMIC_DISABLED)
    const float *ws = (const float*) p->ws;

    if (N_log_2 & 1) {
        neon_static_o_i(p, din, dout);
    } else {
        neon_static_e_i(p, din, dout);
    }

    if (N > 128) {
        const size_t N1 = N >> 1;
        const size_t N2 = N >> 2;
        const size_t N3 = N >> 3;

        ffts_static_rec_i_32f(p, dout              , N2);
        ffts_static_rec_i_32f(p, dout     + N1     , N3);
        ffts_static_rec_i_32f(p, dout     + N1 + N2, N3);
        ffts_static_rec_i_32f(p, dout + N          , N2);
        ffts_static_rec_i_32f(p, dout + N + N1     , N2);

        neon_static_x8_t_i(dout, N, ws + (p->ws_is[N_log_2 - 4] << 1));
    } else if (N == 128) {
        neon_static_x8_i(dout, 32, ws + 8);

        neon_static_x4_i(dout +  64, ws);
        neon_static_x4_i(dout +  96, ws);

        neon_static_x8_i(dout + 128, 32, ws + 8);
        neon_static_x8_i(dout + 192, 32, ws + 8);

        neon_static_x8_t_i(dout, 128, ws + 80);
    } else if (N == 64) {
        neon_static_x4_i(dout     , ws);
        neon_static_x4_i(dout + 64, ws);
        neon_static_x4_i(dout + 96, ws);

        neon_static_x8_t_i(dout, 64, ws + 32);
    } else {
        assert(N == 32);
        neon_static_x8_t_i(dout, 32, ws + 8);
    }
#else
    if (N_log_2 & 1) {
        ffts_static_firstpass_odd_32f(dout, din, p, 1);
    } else {
        ffts_static_firstpass_even_32f(dout, din, p, 1);
    }

    ffts_static_rec_i_32f(p, dout, N);
#endif
}