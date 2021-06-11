/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2016, Jukka Ojanen <jukka.ojanen@kolumbus.fi>
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

#include "ffts_transpose.h"
#include "ffts_internal.h"

#ifdef HAVE_NEON
#include "neon.h"
#include <arm_neon.h>
#elif HAVE_SSE2
#include <emmintrin.h>
#endif

#define TSIZE 8

void
ffts_transpose(uint64_t *in, uint64_t *out, int w, int h)
{
#ifdef HAVE_NEON
#if 0
    neon_transpose4(in, out, w, h);
#else
    neon_transpose8(in, out, w, h);
#endif
#elif HAVE_SSE2
    uint64_t FFTS_ALIGN(64) tmp[TSIZE*TSIZE];
    int tx, ty;
    /* int x; */
    int y;
    int tw = w / TSIZE;
    int th = h / TSIZE;

    for (ty = 0; ty < th; ty++) {
        for (tx = 0; tx < tw; tx++) {
            uint64_t *ip0 = in + w*TSIZE*ty + tx * TSIZE;
            uint64_t *op0 = tmp; /* out + h*TSIZE*tx + ty*TSIZE; */

            /* copy/transpose to tmp */
            for (y = 0; y < TSIZE; y += 2) {
                /* for (x=0;x<TSIZE;x+=2) {
                   op[x*TSIZE] = ip[x];
                */
                __m128d q0 = _mm_load_pd((double*)(ip0 + 0*w));
                __m128d q1 = _mm_load_pd((double*)(ip0 + 1*w));
                __m128d q2 = _mm_load_pd((double*)(ip0 + 2*w));
                __m128d q3 = _mm_load_pd((double*)(ip0 + 3*w));
                __m128d q4 = _mm_load_pd((double*)(ip0 + 4*w));
                __m128d q5 = _mm_load_pd((double*)(ip0 + 5*w));
                __m128d q6 = _mm_load_pd((double*)(ip0 + 6*w));
                __m128d q7 = _mm_load_pd((double*)(ip0 + 7*w));

                __m128d t0 = _mm_shuffle_pd(q0, q1, _MM_SHUFFLE2(0, 0));
                __m128d t1 = _mm_shuffle_pd(q0, q1, _MM_SHUFFLE2(1, 1));
                __m128d t2 = _mm_shuffle_pd(q2, q3, _MM_SHUFFLE2(0, 0));
                __m128d t3 = _mm_shuffle_pd(q2, q3, _MM_SHUFFLE2(1, 1));
                __m128d t4 = _mm_shuffle_pd(q4, q5, _MM_SHUFFLE2(0, 0));
                __m128d t5 = _mm_shuffle_pd(q4, q5, _MM_SHUFFLE2(1, 1));
                __m128d t6 = _mm_shuffle_pd(q6, q7, _MM_SHUFFLE2(0, 0));
                __m128d t7 = _mm_shuffle_pd(q6, q7, _MM_SHUFFLE2(1, 1));

                ip0 += 2;
                /* _mm_store_pd((double *)(op0 + y*h + x), t0);
                   _mm_store_pd((double *)(op0 + y*h + x + h), t1);
                   */

                _mm_store_pd((double*)(op0 + 0        ), t0);
                _mm_store_pd((double*)(op0 + 0 + TSIZE), t1);
                _mm_store_pd((double*)(op0 + 2        ), t2);
                _mm_store_pd((double*)(op0 + 2 + TSIZE), t3);
                _mm_store_pd((double*)(op0 + 4        ), t4);
                _mm_store_pd((double*)(op0 + 4 + TSIZE), t5);
                _mm_store_pd((double*)(op0 + 6        ), t6);
                _mm_store_pd((double*)(op0 + 6 + TSIZE), t7);
                /* } */

                op0 += 2*TSIZE;
            }

            op0 = out + h*tx*TSIZE + ty*TSIZE;
            ip0 = tmp;
            for (y = 0; y < TSIZE; y += 1) {
                /* memcpy(op0, ip0, TSIZE * sizeof(*ip0)); */

                __m128d q0 = _mm_load_pd((double*)(ip0 + 0));
                __m128d q1 = _mm_load_pd((double*)(ip0 + 2));
                __m128d q2 = _mm_load_pd((double*)(ip0 + 4));
                __m128d q3 = _mm_load_pd((double*)(ip0 + 6));

                _mm_store_pd((double*)(op0 + 0), q0);
                _mm_store_pd((double*)(op0 + 2), q1);
                _mm_store_pd((double*)(op0 + 4), q2);
                _mm_store_pd((double*)(op0 + 6), q3);

                op0 += h;
                ip0 += TSIZE;
            }
        }
    }
    /*
    size_t i,j;
    for(i=0;i<w;i+=2) {
    for(j=0;j<h;j+=2) {
    //		out[i*h + j] = in[j*w + i];
    __m128d q0 = _mm_load_pd((double *)(in + j*w + i));
    __m128d q1 = _mm_load_pd((double *)(in + j*w + i + w));
    __m128d t0 = _mm_shuffle_pd(q0, q1, _MM_SHUFFLE2(0, 0));
    __m128d t1 = _mm_shuffle_pd(q0, q1, _MM_SHUFFLE2(1, 1));
    _mm_store_pd((double *)(out + i*h + j), t0);
    _mm_store_pd((double *)(out + i*h + j + h), t1);
    }
    }
    */
#else
    const int bw = 1;
    const int bh = 8;
    int i = 0, j = 0;

    for (; i <= h - bh; i += bh) {
        for (j = 0; j <= w - bw; j += bw) {
            uint64_t const *ib = &in[w*i + j];
            uint64_t *ob = &out[h*j + i];

            uint64_t s_0_0 = ib[0*w + 0];
            uint64_t s_1_0 = ib[1*w + 0];
            uint64_t s_2_0 = ib[2*w + 0];
            uint64_t s_3_0 = ib[3*w + 0];
            uint64_t s_4_0 = ib[4*w + 0];
            uint64_t s_5_0 = ib[5*w + 0];
            uint64_t s_6_0 = ib[6*w + 0];
            uint64_t s_7_0 = ib[7*w + 0];

            ob[0*h + 0] = s_0_0;
            ob[0*h + 1] = s_1_0;
            ob[0*h + 2] = s_2_0;
            ob[0*h + 3] = s_3_0;
            ob[0*h + 4] = s_4_0;
            ob[0*h + 5] = s_5_0;
            ob[0*h + 6] = s_6_0;
            ob[0*h + 7] = s_7_0;
        }
    }

    if (i < h) {
        int i1;

        for (i1 = 0; i1 < w; i1++) {
            for (j = i; j < h; j++) {
                out[i1*h + j] = in[j*w + i1];
            }
        }
    }

    if (j < w) {
        int j1;

        for (i = j; i < w; i++) {
            for (j1 = 0; j1 < h; j1++) {
                out[i*h + j1] = in[j1*w + i];
            }
        }
    }
#endif
}