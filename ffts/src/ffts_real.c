/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2012, Anthony M. Blake <amb@anthonix.com>
Copyright (c) 2012, The University of Waikato
Copyright (c) 2015, Jukka Ojanen <jukka.ojanen@kolumbus.fi>

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

#include "ffts_real.h"
#include "ffts_internal.h"
#include "ffts_trig.h"

#ifdef HAVE_NEON
#include <arm_neon.h>
#elif HAVE_SSE
#include <xmmintrin.h>

/* check if have SSE3 intrinsics */
#ifdef HAVE_PMMINTRIN_H
#include <pmmintrin.h>
#elif HAVE_INTRIN_H
#include <intrin.h>
#else
/* avoid using negative zero as some configurations have problems with those */
static const FFTS_ALIGN(16) unsigned int sign_mask_even[4] = {
    0x80000000, 0, 0x80000000, 0
};
static const FFTS_ALIGN(16) unsigned int sign_mask_odd[4] = {
    0, 0x80000000, 0, 0x80000000
};
#endif
#endif

static void
ffts_free_1d_real(ffts_plan_t *p)
{
    if (p->B) {
        ffts_aligned_free(p->B);
    }

    if (p->A) {
        ffts_aligned_free(p->A);
    }

    if (p->buf) {
        ffts_aligned_free(p->buf);
    }

    if (p->plans[0]) {
        ffts_free(p->plans[0]);
    }

    free(p);
}

static void
ffts_execute_1d_real(ffts_plan_t *p, const void *input, void *output)
{
    float *const FFTS_RESTRICT out =
        (float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_16(output);
    float *const FFTS_RESTRICT buf =
        (float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_32(p->buf);
    const float *const FFTS_RESTRICT A =
        (const float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_32(p->A);
    const float *const FFTS_RESTRICT B =
        (const float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_32(p->B);
    const int N = (const int) p->N;
    int i;

#ifdef __ARM_NEON__
    float *p_buf0 = buf;
    float *p_buf1 = buf + N - 2;
    float *p_out = out;
#endif

    /* we know this */
    FFTS_ASSUME(N/2 > 0);

    p->plans[0]->transform(p->plans[0], input, buf);

#ifndef HAVE_SSE
    buf[N + 0] = buf[0];
    buf[N + 1] = buf[1];
#endif

#ifdef __ARM_NEON__
    for (i = 0; i < N; i += 4) {
        __asm__ __volatile__ (
            "vld1.32 {q8},  [%[pa]]!\n\t"
            "vld1.32 {q9},  [%[pb]]!\n\t"
            "vld1.32 {q10}, [%[buf0]]!\n\t"
            "vld1.32 {q11}, [%[buf1]]\n\t"
            "sub %[buf1], %[buf1], #16\n\t"

            "vdup.32 d26, d16[1]\n\t"
            "vdup.32 d27, d17[1]\n\t"
            "vdup.32 d24, d16[0]\n\t"
            "vdup.32 d25, d17[0]\n\t"

            "vdup.32 d30, d23[1]\n\t"
            "vdup.32 d31, d22[1]\n\t"
            "vdup.32 d28, d23[0]\n\t"
            "vdup.32 d29, d22[0]\n\t"

            "vmul.f32 q13, q13, q10\n\t"
            "vmul.f32 q15, q15, q9\n\t"
            "vmul.f32 q12, q12, q10\n\t"
            "vmul.f32 q14, q14, q9\n\t"
            "vrev64.f32 q13, q13\n\t"
            "vrev64.f32 q15, q15\n\t"

            "vtrn.32 d26, d27\n\t"
            "vtrn.32 d30, d31\n\t"
            "vneg.f32 d26, d26\n\t"
            "vneg.f32 d31, d31\n\t"
            "vtrn.32 d26, d27\n\t"
            "vtrn.32 d30, d31\n\t"

            "vadd.f32 q12, q12, q14\n\t"
            "vadd.f32 q13, q13, q15\n\t"
            "vadd.f32 q12, q12, q13\n\t"
            "vst1.32 {q12}, [%[pout]]!\n\t"
            : [buf0] "+r" (p_buf0), [buf1] "+r" (p_buf1), [pout] "+r" (p_out)
            : [pa] "r" (A), [pb] "r" (B) 
            : "memory", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15"
        );
    }
#elif HAVE_SSE3
    if (FFTS_UNLIKELY(N <= 8)) {
        __m128 t0 = _mm_load_ps(buf);
        __m128 t1 = _mm_load_ps(buf + N - 4);
        __m128 t2 = _mm_load_ps(A);
        __m128 t3 = _mm_load_ps(B);

        _mm_store_ps(out, _mm_add_ps(_mm_addsub_ps(
            _mm_mul_ps(t0, _mm_moveldup_ps(t2)),
            _mm_mul_ps(_mm_shuffle_ps(t0, t0, _MM_SHUFFLE(2,3,0,1)),
            _mm_movehdup_ps(t2))), _mm_addsub_ps(
            _mm_mul_ps(_mm_shuffle_ps(t0, t1, _MM_SHUFFLE(3,3,1,1)),
            _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,3,0,1))),
            _mm_mul_ps(_mm_shuffle_ps(t0, t1, _MM_SHUFFLE(2,2,0,0)), t3))));

        if (N == 8) {
            t2 = _mm_load_ps(A + 4);
            t3 = _mm_load_ps(B + 4);

            _mm_store_ps(out + 4, _mm_add_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t2)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t2))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t1, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t0, _MM_SHUFFLE(2,2,0,0)), t3))));
        }
    } else {
        __m128 t0 = _mm_load_ps(buf);

        for (i = 0; i < N; i += 16) {
            __m128 t1 = _mm_load_ps(buf + i);
            __m128 t2 = _mm_load_ps(buf + N - i - 4);
            __m128 t3 = _mm_load_ps(A + i);
            __m128 t4 = _mm_load_ps(B + i);

            _mm_store_ps(out + i, _mm_add_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4))));

            t0 = _mm_load_ps(buf + N - i - 8);
            t1 = _mm_load_ps(buf + i + 4);
            t3 = _mm_load_ps(A + i + 4);
            t4 = _mm_load_ps(B + i + 4);

            _mm_store_ps(out + i + 4, _mm_add_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)), t4))));

            t1 = _mm_load_ps(buf + i + 8);
            t2 = _mm_load_ps(buf + N - i - 12);
            t3 = _mm_load_ps(A + i + 8);
            t4 = _mm_load_ps(B + i + 8);

            _mm_store_ps(out + i + 8, _mm_add_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4))));

            t0 = _mm_load_ps(buf + N - i - 16);
            t1 = _mm_load_ps(buf + i + 12);
            t3 = _mm_load_ps(A + i + 12);
            t4 = _mm_load_ps(B + i + 12);

            _mm_store_ps(out + i + 12, _mm_add_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)), t4))));
        }
    }
#elif HAVE_SSE
    if (FFTS_UNLIKELY(N <= 8)) {
        __m128 c0 = _mm_load_ps((const float*) sign_mask_even);
        __m128 t0 = _mm_load_ps(buf);
        __m128 t1 = _mm_load_ps(buf + N - 4);
        __m128 t2 = _mm_load_ps(A);
        __m128 t3 = _mm_load_ps(B);

        _mm_store_ps(out, _mm_add_ps(_mm_add_ps(_mm_add_ps(
            _mm_mul_ps(t0, _mm_shuffle_ps(t2, t2, _MM_SHUFFLE(2,2,0,0))),
            _mm_mul_ps(_mm_shuffle_ps(t0, t0, _MM_SHUFFLE(2,3,0,1)),
            _mm_xor_ps(_mm_shuffle_ps(t2, t2, _MM_SHUFFLE(3,3,1,1)), c0))),
            _mm_mul_ps(_mm_shuffle_ps(t0, t1, _MM_SHUFFLE(2,2,0,0)), t3)),
            _mm_mul_ps(_mm_shuffle_ps(t0, t1, _MM_SHUFFLE(3,3,1,1)),
            _mm_shuffle_ps(_mm_xor_ps(t3, c0), _mm_xor_ps(t3, c0),
            _MM_SHUFFLE(2,3,0,1)))));

        if (N == 8) {
            t2 = _mm_load_ps(A + 4);
            t3 = _mm_load_ps(B + 4);

            _mm_store_ps(out + 4, _mm_add_ps(_mm_add_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t2, t2, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t2, t2, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t0, _MM_SHUFFLE(2,2,0,0)), t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(_mm_xor_ps(t3, c0), _mm_xor_ps(t3, c0),
                _MM_SHUFFLE(2,3,0,1)))));
        }
    } else {
        __m128 c0 = _mm_load_ps((const float*) sign_mask_even);
        __m128 t0 = _mm_load_ps(buf);

        for (i = 0; i < N; i += 16) {
            __m128 t1 = _mm_load_ps(buf + i);
            __m128 t2 = _mm_load_ps(buf + N - i - 4);
            __m128 t3 = _mm_load_ps(A + i);
            __m128 t4 = _mm_load_ps(B + i);

            _mm_store_ps(out + i, _mm_add_ps(_mm_add_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4)),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(_mm_xor_ps(t4, c0), _mm_xor_ps(t4, c0),
                _MM_SHUFFLE(2,3,0,1)))));

            t0 = _mm_load_ps(buf + N - i - 8);
            t1 = _mm_load_ps(buf + i + 4);
            t3 = _mm_load_ps(A + i + 4);
            t4 = _mm_load_ps(B + i + 4);

            _mm_store_ps(out + i + 4, _mm_add_ps(_mm_add_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)), t4)),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(_mm_xor_ps(t4, c0), _mm_xor_ps(t4, c0),
                _MM_SHUFFLE(2,3,0,1)))));

            t1 = _mm_load_ps(buf + i + 8);
            t2 = _mm_load_ps(buf + N - i - 12);
            t3 = _mm_load_ps(A + i + 8);
            t4 = _mm_load_ps(B + i + 8);

            _mm_store_ps(out + i + 8, _mm_add_ps(_mm_add_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4)),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(_mm_xor_ps(t4, c0), _mm_xor_ps(t4, c0),
                _MM_SHUFFLE(2,3,0,1)))));

            t0 = _mm_load_ps(buf + N - i - 16);
            t1 = _mm_load_ps(buf + i + 12);
            t3 = _mm_load_ps(A + i + 12);
            t4 = _mm_load_ps(B + i + 12);

            _mm_store_ps(out + i + 12, _mm_add_ps(_mm_add_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)), t4)),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(_mm_xor_ps(t4, c0), _mm_xor_ps(t4, c0),
                _MM_SHUFFLE(2,3,0,1)))));
        }
    }
#else
    for (i = 0; i < N/2; i++) {
        out[2*i + 0] =
            buf[    2*i + 0] * A[2*i + 0] - buf[    2*i + 1] * A[2*i + 1] +
            buf[N - 2*i + 0] * B[2*i + 0] + buf[N - 2*i + 1] * B[2*i + 1];
        out[2*i + 1] =
            buf[    2*i + 1] * A[2*i + 0] + buf[    2*i + 0] * A[2*i + 1] +
            buf[N - 2*i + 0] * B[2*i + 1] - buf[N - 2*i + 1] * B[2*i + 0];
    }
#endif

    out[N + 0] = buf[0] - buf[1];
    out[N + 1] = 0.0f;
}

static void
ffts_execute_1d_real_inv(ffts_plan_t *p, const void *input, void *output)
{
    float *const FFTS_RESTRICT in =
        (float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_16(input);
    float *const FFTS_RESTRICT buf =
        (float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_32(p->buf);
    const float *const FFTS_RESTRICT A =
        (const float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_32(p->A);
    const float *const FFTS_RESTRICT B =
        (const float *const FFTS_RESTRICT) FFTS_ASSUME_ALIGNED_32(p->B);
    const int N = (const int) p->N;
    int i;

#ifdef __ARM_NEON__
    float *p_buf0 = in;
    float *p_buf1 = in + N - 2;
    float *p_out = buf;
#endif

    /* we know this */
    FFTS_ASSUME(N/2 > 0);

#ifdef __ARM_NEON__
    for (i = 0; i < N/2; i += 2) {
        __asm__ __volatile__ (
            "vld1.32 {q8},  [%[pa]]!\n\t"
            "vld1.32 {q9},  [%[pb]]!\n\t"
            "vld1.32 {q10}, [%[buf0]]!\n\t"
            "vld1.32 {q11}, [%[buf1]]\n\t"
            "sub %[buf1], %[buf1], #16\n\t"

            "vdup.32 d26, d16[1]\n\t"
            "vdup.32 d27, d17[1]\n\t"
            "vdup.32 d24, d16[0]\n\t"
            "vdup.32 d25, d17[0]\n\t"

            "vdup.32 d30, d23[1]\n\t"
            "vdup.32 d31, d22[1]\n\t"
            "vdup.32 d28, d23[0]\n\t"
            "vdup.32 d29, d22[0]\n\t"

            "vmul.f32 q13, q13, q10\n\t"
            "vmul.f32 q15, q15, q9\n\t"
            "vmul.f32 q12, q12, q10\n\t"
            "vmul.f32 q14, q14, q9\n\t"
            "vrev64.f32 q13, q13\n\t"
            "vrev64.f32 q15, q15\n\t"

            "vtrn.32 d26, d27\n\t"
            "vtrn.32 d28, d29\n\t"
            "vneg.f32 d27, d27\n\t"
            "vneg.f32 d29, d29\n\t"
            "vtrn.32 d26, d27\n\t"
            "vtrn.32 d28, d29\n\t"

            "vadd.f32 q12, q12, q14\n\t"
            "vsub.f32 q13, q13, q15\n\t"
            "vadd.f32 q12, q12, q13\n\t"
            "vst1.32 {q12}, [%[pout]]!\n\t"
            : [buf0] "+r" (p_buf0), [buf1] "+r" (p_buf1), [pout] "+r" (p_out)
            : [pa] "r" (A), [pb] "r" (B)
            : "memory", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15"
        );
    }
#elif HAVE_SSE3
    if (FFTS_UNLIKELY(N <= 8)) {
        __m128 t0 = _mm_loadl_pi(_mm_setzero_ps(), (const __m64*) &in[N]);
        __m128 t1 = _mm_load_ps(in);
        __m128 t2 = _mm_load_ps(in + N - 4);
        __m128 t3 = _mm_load_ps(A);
        __m128 t4 = _mm_load_ps(B);

        _mm_store_ps(buf, _mm_sub_ps(_mm_addsub_ps(
            _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
            _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
            _mm_movehdup_ps(t3))), _mm_addsub_ps(
            _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
            _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
            _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4))));

        if (N == 8) {
            t3 = _mm_load_ps(A + 4);
            t4 = _mm_load_ps(B + 4);

            _mm_store_ps(buf + 4, _mm_sub_ps(_mm_addsub_ps(
                _mm_mul_ps(t2, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t2, t2, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t2, t1, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t1, _MM_SHUFFLE(2,2,0,0)), t4))));
        }
    } else {
        __m128 t0 = _mm_loadl_pi(_mm_setzero_ps(), (const __m64*) &in[N]);

        for (i = 0; i < N; i += 16) {
            __m128 t1 = _mm_load_ps(in + i);
            __m128 t2 = _mm_load_ps(in + N - i - 4);
            __m128 t3 = _mm_load_ps(A + i);
            __m128 t4 = _mm_load_ps(B + i);

            _mm_store_ps(buf + i, _mm_sub_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4))));

            t0 = _mm_load_ps(in + N - i - 8);
            t1 = _mm_load_ps(in + i + 4);
            t3 = _mm_load_ps(A + i + 4);
            t4 = _mm_load_ps(B + i + 4);

            _mm_store_ps(buf + i + 4, _mm_sub_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)), t4))));

            t1 = _mm_load_ps(in + i + 8);
            t2 = _mm_load_ps(in + N - i - 12);
            t3 = _mm_load_ps(A + i + 8);
            t4 = _mm_load_ps(B + i + 8);

            _mm_store_ps(buf + i + 8, _mm_sub_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)), t4))));

            t0 = _mm_load_ps(in + N - i - 16);
            t1 = _mm_load_ps(in + i + 12);
            t3 = _mm_load_ps(A + i + 12);
            t4 = _mm_load_ps(B + i + 12);

            _mm_store_ps(buf + i + 12, _mm_sub_ps(_mm_addsub_ps(
                _mm_mul_ps(t1, _mm_moveldup_ps(t3)),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_movehdup_ps(t3))), _mm_addsub_ps(
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)), t4))));
        }
    }
#elif HAVE_SSE
    if (FFTS_UNLIKELY(N <= 8)) {
        __m128 c0 = _mm_load_ps((const float*) sign_mask_odd);
        __m128 t0 = _mm_loadl_pi(_mm_setzero_ps(), (const __m64*) &in[N]);
        __m128 t1 = _mm_load_ps(in);
        __m128 t2 = _mm_load_ps(in + N - 4);
        __m128 t3 = _mm_load_ps(A);
        __m128 t4 = _mm_load_ps(B);

        _mm_store_ps(buf, _mm_add_ps(_mm_sub_ps(_mm_add_ps(
            _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
            _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
            _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
            _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
            _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1)))),
            _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)),
            _mm_xor_ps(t4, c0))));

        if (N == 8) {
            t3 = _mm_load_ps(A + 4);
            t4 = _mm_load_ps(B + 4);

            _mm_store_ps(buf + 4, _mm_add_ps(_mm_sub_ps(_mm_add_ps(
                _mm_mul_ps(t2, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t2, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t1, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1)))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t1, _MM_SHUFFLE(2,2,0,0)),
                _mm_xor_ps(t4, c0))));
        }
    } else {
        __m128 c0 = _mm_load_ps((const float*) sign_mask_odd);
        __m128 t0 = _mm_loadl_pi(_mm_setzero_ps(), (const __m64*) &in[N]);

        for (i = 0; i < N; i += 16) {
            __m128 t1 = _mm_load_ps(in + i);
            __m128 t2 = _mm_load_ps(in + N - i - 4);
            __m128 t3 = _mm_load_ps(A + i);
            __m128 t4 = _mm_load_ps(B + i);

            _mm_store_ps(buf + i, _mm_add_ps(_mm_sub_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1)))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)),
                _mm_xor_ps(t4, c0))));

            t0 = _mm_load_ps(in + N - i - 8);
            t1 = _mm_load_ps(in + i + 4);
            t3 = _mm_load_ps(A + i + 4);
            t4 = _mm_load_ps(B + i + 4);

            _mm_store_ps(buf + i + 4, _mm_add_ps(_mm_sub_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1)))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)),
                _mm_xor_ps(t4, c0))));

            t1 = _mm_load_ps(in + i + 8);
            t2 = _mm_load_ps(in + N - i - 12);
            t3 = _mm_load_ps(A + i + 8);
            t4 = _mm_load_ps(B + i + 8);

            _mm_store_ps(buf + i + 8, _mm_add_ps(_mm_sub_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1)))),
                _mm_mul_ps(_mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,2,0,0)),
                _mm_xor_ps(t4, c0))));

            t0 = _mm_load_ps(in + N - i - 16);
            t1 = _mm_load_ps(in + i + 12);
            t3 = _mm_load_ps(A + i + 12);
            t4 = _mm_load_ps(B + i + 12);

            _mm_store_ps(buf + i + 12, _mm_add_ps(_mm_sub_ps(_mm_add_ps(
                _mm_mul_ps(t1, _mm_shuffle_ps(t3, t3, _MM_SHUFFLE(2,2,0,0))),
                _mm_mul_ps(_mm_shuffle_ps(t1, t1, _MM_SHUFFLE(2,3,0,1)),
                _mm_xor_ps(_mm_shuffle_ps(t3, t3, _MM_SHUFFLE(3,3,1,1)), c0))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(3,3,1,1)),
                _mm_shuffle_ps(t4, t4, _MM_SHUFFLE(2,3,0,1)))),
                _mm_mul_ps(_mm_shuffle_ps(t2, t0, _MM_SHUFFLE(2,2,0,0)),
                _mm_xor_ps(t4, c0))));
        }
    }
#else
    for (i = 0; i < N/2; i++) {
        buf[2*i + 0] =
            in[    2*i + 0] * A[2*i + 0] + in[    2*i + 1] * A[2*i + 1] +
            in[N - 2*i + 0] * B[2*i + 0] - in[N - 2*i + 1] * B[2*i + 1];
        buf[2*i + 1] =
            in[    2*i + 1] * A[2*i + 0] - in[    2*i + 0] * A[2*i + 1] -
            in[N - 2*i + 0] * B[2*i + 1] - in[N - 2*i + 1] * B[2*i + 0];
    }
#endif

    p->plans[0]->transform(p->plans[0], buf, output);
}

FFTS_API ffts_plan_t*
ffts_init_1d_real(size_t N, int sign)
{
    ffts_plan_t *p;

    p = (ffts_plan_t*) calloc(1, sizeof(*p) + sizeof(*p->plans));
    if (!p) {
        return NULL;
    }

    if (sign < 0) {
        p->transform = &ffts_execute_1d_real;
    } else {
        p->transform = &ffts_execute_1d_real_inv;
    }

    p->destroy = &ffts_free_1d_real;
    p->N       = N;
    p->rank    = 1;
    p->plans   = (ffts_plan_t**) &p[1];

    p->plans[0] = ffts_init_1d(N/2, sign);
    if (!p->plans[0]) {
        goto cleanup;
    }

    p->buf = ffts_aligned_malloc(2 * ((N/2) + 1) * sizeof(float));
    if (!p->buf) {
        goto cleanup;
    }

    p->A = (float*) ffts_aligned_malloc(N * sizeof(float));
    if (!p->A) {
        goto cleanup;
    }

    p->B = (float*) ffts_aligned_malloc(N * sizeof(float));
    if (!p->B) {
        goto cleanup;
    }

#ifdef HAVE_SSE3
    ffts_generate_table_1d_real_32f(p, sign, 1);
#else
    ffts_generate_table_1d_real_32f(p, sign, 0);
#endif

    return p;

cleanup:
    ffts_free_1d_real(p);
    return NULL;
}