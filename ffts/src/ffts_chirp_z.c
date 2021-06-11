/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2016, Jukka Ojanen <jukka.ojanen@kolumbus.fi>

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

#include "ffts_chirp_z.h"

#include "ffts_internal.h"
#include "ffts_trig.h"

/*
*  For more information on algorithms:
*
*  L. I. Bluestein, A linear filtering approach to the computation of
*  the discrete Fourier transform, 1968 NEREM Rec., pp. 218-219
*
*  Lawrence R. Rabiner, Ronald W. Schafer, Charles M. Rader,
*  The Chirp z-Transform Algorithm and Its Application
*  Bell Sys. Tech. J., vol. 48, pp. 1249-1292, May 1969.
*
*  Rick Lyons, Four Ways to Compute an Inverse FFT Using the Forward FFT Algorithm
*  https://www.dsprelated.com/showarticle/800.php, July 7, 2015
*/

/* forward declarations */
static void
ffts_chirp_z_transform_f_32f(struct _ffts_plan_t *p, const void *in, void *out);

static void
ffts_chirp_z_transform_i_32f(struct _ffts_plan_t *p, const void *in, void *out);

static void
ffts_chirp_z_free(ffts_plan_t *p)
{
    if (p->B)
        ffts_aligned_free(p->B);

    if (p->A)
        ffts_aligned_free(p->A);

    if (p->buf)
        ffts_aligned_free(p->buf);

    if (p->plans[0])
        ffts_free(p->plans[0]);

    free(p);
}

ffts_plan_t*
ffts_chirp_z_init(size_t N, int sign)
{
    float *A, *B, reciprocal_M, *tmp;
    ffts_plan_t *p;
    size_t i, M;

    FFTS_ASSUME(N > 2);

    p = (ffts_plan_t*) calloc(1, sizeof(*p) + sizeof(*p->plans));
    if (!p)
        return NULL;

    p->destroy = ffts_chirp_z_free;
    p->N = N;
    p->rank = 1;
    p->plans = (ffts_plan_t**) &p[1];

    if (sign < 0)
        p->transform = ffts_chirp_z_transform_f_32f;
    else
        p->transform = ffts_chirp_z_transform_i_32f;

    /* determinate next power of two such that M >= 2*N-1 */
    M = ffts_next_power_of_2(2*N-1);
    p->plans[0] = ffts_init_1d(M, FFTS_FORWARD);
    if (!p->plans[0])
        goto cleanup;

    p->A = A = (float*) ffts_aligned_malloc(2 * N * sizeof(float));
    if (!p->A)
        goto cleanup;

    p->B = B = (float*) ffts_aligned_malloc(2 * M * sizeof(float));
    if (!p->B)
        goto cleanup;

    p->buf = tmp = (float*) ffts_aligned_malloc(2 * 2 * M * sizeof(float));

    ffts_generate_chirp_32f((ffts_cpx_32f*) A, N);

    /* scale with reciprocal of length */
    reciprocal_M = 1.0f / M;
    tmp[0] = A[0] * reciprocal_M;
    tmp[1] = A[1] * reciprocal_M;
    for (i = 1; i < N; ++i) {
        tmp[2 * i + 0] = tmp[2 * (M - i) + 0] = A[2 * i + 0] * reciprocal_M;
        tmp[2 * i + 1] = tmp[2 * (M - i) + 1] = A[2 * i + 1] * reciprocal_M;
    }

    /* zero pad */
    for (; i <= M - N; ++i)
        tmp[2 * i] = tmp[2 * i + 1] = 0.0f;

    /* FFT */
    p->plans[0]->transform(p->plans[0], tmp, B);
    return p;

cleanup:
    ffts_chirp_z_free(p);
    return NULL;
}

static void
ffts_chirp_z_transform_f_32f(struct _ffts_plan_t *p, const void *in, void *out)
{
    const float *A = FFTS_ASSUME_ALIGNED_32(p->A);
    const float *B = FFTS_ASSUME_ALIGNED_32(p->B);
    size_t i, M = p->plans[0]->N, N = p->N;
    float *t1 = (float*) FFTS_ASSUME_ALIGNED_32(p->buf);
    float *t2 = FFTS_ASSUME_ALIGNED_32(&t1[2 * M]);
    const float *din = (const float*) in;
    float *dout = (float*) out;

    /* we know this */
    FFTS_ASSUME(M >= 8);

    /* multiply input with conjugated sequence */
    for (i = 0; i < N; ++i) {
        t1[2 * i + 0] = din[2 * i + 0] * A[2 * i + 0] + din[2 * i + 1] * A[2 * i + 1];
        t1[2 * i + 1] = din[2 * i + 1] * A[2 * i + 0] - din[2 * i + 0] * A[2 * i + 1];
    }

    /* zero pad */
    for (; i < M; ++i)
        t1[2 * i] = t1[2 * i + 1] = 0.0f;

    /* convolution using FFT */
    p->plans[0]->transform(p->plans[0], t1, t2);

    /* complex multiply */
    for (i = 0; i < M; ++i) {
        t1[2 * i + 0] = t2[2 * i + 1] * B[2 * i + 0] + t2[2 * i + 0] * B[2 * i + 1];
        t1[2 * i + 1] = t2[2 * i + 0] * B[2 * i + 0] - t2[2 * i + 1] * B[2 * i + 1];
    }

    /* IFFT using FFT with real and imaginary parts swapped */
    p->plans[0]->transform(p->plans[0], t1, t2);

    /* multiply output with conjugated sequence */
    for (i = 0; i < N; ++i) {
        dout[2 * i + 0] = t2[2 * i + 1] * A[2 * i + 0] + t2[2 * i + 0] * A[2 * i + 1];
        dout[2 * i + 1] = t2[2 * i + 0] * A[2 * i + 0] - t2[2 * i + 1] * A[2 * i + 1];
    }
}

/* IFFT using FFT with real and imaginary parts swapped */
static void
ffts_chirp_z_transform_i_32f(struct _ffts_plan_t *p, const void *in, void *out)
{
    const float *A = FFTS_ASSUME_ALIGNED_32(p->A);
    const float *B = FFTS_ASSUME_ALIGNED_32(p->B);
    size_t i, M = p->plans[0]->N, N = p->N;
    float *t1 = (float*) FFTS_ASSUME_ALIGNED_32(p->buf);
    float *t2 = FFTS_ASSUME_ALIGNED_32(&t1[2 * M]);
    const float *din = (const float*) in;
    float *dout = (float*) out;

    /* we know this */
    FFTS_ASSUME(M >= 8);

    /* multiply input with conjugated sequence */
    for (i = 0; i < N; ++i) {
        t1[2 * i + 0] = din[2 * i + 1] * A[2 * i + 0] + din[2 * i + 0] * A[2 * i + 1];
        t1[2 * i + 1] = din[2 * i + 0] * A[2 * i + 0] - din[2 * i + 1] * A[2 * i + 1];
    }

    /* zero pad */
    for (; i < M; ++i)
        t1[2 * i] = t1[2 * i + 1] = 0.0f;

    /* convolution using FFT */
    p->plans[0]->transform(p->plans[0], t1, t2);

    /* complex multiply */
    for (i = 0; i < M; ++i) {
        t1[2 * i + 0] = t2[2 * i + 1] * B[2 * i + 0] + t2[2 * i + 0] * B[2 * i + 1];
        t1[2 * i + 1] = t2[2 * i + 0] * B[2 * i + 0] - t2[2 * i + 1] * B[2 * i + 1];
    }

    /* IFFT using FFT with real and imaginary parts swapped */
    p->plans[0]->transform(p->plans[0], t1, t2);

    /* multiply output with conjugated sequence */
    for (i = 0; i < N; ++i) {
        dout[2 * i + 0] = t2[2 * i + 0] * A[2 * i + 0] - t2[2 * i + 1] * A[2 * i + 1];
        dout[2 * i + 1] = t2[2 * i + 1] * A[2 * i + 0] + t2[2 * i + 0] * A[2 * i + 1];
    }
}
