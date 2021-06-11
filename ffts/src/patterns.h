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

#ifndef FFTS_PATTERNS_H
#define FFTS_PATTERNS_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#include <stddef.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifndef LEAF_N
#define LEAF_N 8
#endif

#if LEAF_N == 8
static void
ffts_elaborate_offsets_even8(ptrdiff_t *const offsets,
                             int log_N);

static void
ffts_elaborate_offsets_odd8(ptrdiff_t *const offsets,
                            int log_N,
                            int input_offset,
                            int output_offset,
                            int stride);

static void
ffts_hardcodedleaf_is_rec_even4(ptrdiff_t **is,
                                int big_N,
                                int offset,
                                int stride,
                                int VL);

static void
ffts_hardcodedleaf_is_rec_even8(ptrdiff_t **is,
                                int big_N,
                                int offset,
                                int stride,
                                int VL);
#else
static void
ffts_elaborate_offsets_even(ptrdiff_t *const offsets,
                            int leaf_N,
                            int N,
                            int input_offset,
                            int output_offset,
                            int stride);

static void
ffts_elaborate_offsets_odd(ptrdiff_t *const offsets,
                           int leaf_N,
                           int N,
                           int input_offset,
                           int output_offset,
                           int stride);

static void
ffts_hardcodedleaf_is_rec_even(ptrdiff_t **is,
                               int big_N,
                               int N,
                               int offset,
                               int stride,
                               int VL);

static void
ffts_hardcodedleaf_is_rec_odd(ptrdiff_t **is,
                              int big_N,
                              int N,
                              int offset,
                              int stride,
                              int VL);
#endif

static int
ffts_compare_offsets(const void *pa, const void *pb)
{
    const ptrdiff_t a = *(const ptrdiff_t*) pa;
    const ptrdiff_t b = *(const ptrdiff_t*) pb;
    return (a > b) - (a < b);
}

static void
ffts_permute_addr(int N, int offset, int stride, int *const d)
{
    int a[4] = {0,2,1,3};
    int i;

    for (i = 0; i < 4; i++) {
        d[i] = offset + (a[i] << stride);
        if (d[i] < 0) {
            d[i] += N;
        }
    }
}

#if LEAF_N == 8
static void
ffts_elaborate_offsets_even8(ptrdiff_t *const offsets, int log_N)
{
    int offset = 1 << (log_N - 4);
    int stride = 1;

    offsets[0] = 0;
    offsets[1] = 0;
    offsets[2] = offset * 2;
    offsets[3] = 8;
    offsets[4] = offset;
    offsets[5] = 16;
    offsets[6] = -offset;
    offsets[7] = 24;

    for(; log_N > 5; --log_N, stride *= 2) {
        ffts_elaborate_offsets_odd8(offsets, log_N - 2,
            stride, 1 << (log_N - 1), stride * 4);

        ffts_elaborate_offsets_odd8(offsets, log_N - 2,
            -stride, 3 * (1 << (log_N - 2)), stride * 4);
    }
}

static void
ffts_elaborate_offsets_odd8(ptrdiff_t *const offsets,
                            int log_N,
                            int input_offset,
                            int output_offset,
                            int stride)
{
    if (log_N <= 4) {
        offsets[(output_offset / 4) + 0] = input_offset * 2;
        offsets[(output_offset / 4) + 1] = output_offset;

        if (log_N == 4) {
            offsets[(output_offset / 4) + 2] = (input_offset + stride) * 2;
            offsets[(output_offset / 4) + 3] = output_offset + 8;
        }
    } else {
        ffts_elaborate_offsets_odd8(offsets, log_N - 1, input_offset,
            output_offset, stride * 2);

        ffts_elaborate_offsets_odd8(offsets, log_N - 2, input_offset + stride,
            output_offset + (1 << (log_N - 1)), stride * 4);

        ffts_elaborate_offsets_odd8(offsets, log_N - 2, input_offset - stride,
            output_offset + 3 * (1 << (log_N - 2)), stride * 4);
    }
}

static void
ffts_hardcodedleaf_is_rec_even4(ptrdiff_t **is,
                                int big_N,
                                int offset,
                                int stride,
                                int VL)
{
    int i, perm[4];

    ffts_permute_addr(big_N, offset, stride, perm);

    if (!((2 * perm[0]) % (2 * VL))) {
        for (i = 0; i < 4; i++) {
            (*is)[i] = 2 * perm[i];
        }

        *is += 4;
    }
}

static void
ffts_hardcodedleaf_is_rec_even8(ptrdiff_t **is,
                                int big_N,
                                int offset,
                                int stride,
                                int VL)
{
    int temp;

    ffts_hardcodedleaf_is_rec_even4(is, big_N, offset, stride + 1, VL);

    temp = offset + (1 << stride);
    if (temp < 0) {
        temp += big_N;
    }

    temp *= 2;

    if (!(temp % (2 * VL))) {
        int i;

        (*is)[0] = offset + (1 << stride);
        (*is)[1] = offset + (1 << stride) + (1 << (stride + 2));
        (*is)[2] = offset - (1 << stride);
        (*is)[3] = offset - (1 << stride) + (1 << (stride + 2));

        for (i = 0; i < 4; i++) {
            if ((*is)[i] < 0) {
                (*is)[i] += big_N;
            }
        }

        for (i = 0; i < 4; i++) {
            (*is)[i] *= 2;
        }

        *is += 4;
    }
}
#else
static void
ffts_elaborate_offsets_even(ptrdiff_t *const offsets,
                            int leaf_N,
                            int N,
                            int input_offset,
                            int output_offset,
                            int stride)
{
    if (N == leaf_N) {
        offsets[2 * (output_offset / leaf_N) + 0] = input_offset * 2;
        offsets[2 * (output_offset / leaf_N) + 1] = output_offset;
    } else if (N > 4) {
        ffts_elaborate_offsets_even(offsets, leaf_N,
            N/2, input_offset, output_offset, stride * 2);

        ffts_elaborate_offsets_odd(offsets, leaf_N,
            N/4, input_offset + stride, output_offset + N/2, stride * 4);

        if (N/4 >= leaf_N) {
            ffts_elaborate_offsets_odd(offsets, leaf_N,
                N/4, input_offset - stride, output_offset + 3*N/4, stride * 4);
        }
    }
}

static void
ffts_elaborate_offsets_odd(ptrdiff_t *const offsets,
                           int leaf_N,
                           int N,
                           int input_offset,
                           int output_offset,
                           int stride)
{
    if (N <= leaf_N) {
        offsets[2 * (output_offset / leaf_N) + 0] = input_offset * 2;
        offsets[2 * (output_offset / leaf_N) + 1] = output_offset;
    } else if (N > 4) {
        ffts_elaborate_offsets_odd(offsets, leaf_N, N/2,
            input_offset, output_offset, stride * 2);

        ffts_elaborate_offsets_odd(offsets, leaf_N, N/4,
            input_offset + stride, output_offset + N/2, stride * 4);

        if (N/4 >= leaf_N) {
            ffts_elaborate_offsets_odd(offsets, leaf_N, N/4,
                input_offset - stride, output_offset + 3*N/4, stride * 4);
        }
    }
}

static void
ffts_hardcodedleaf_is_rec_even(ptrdiff_t **is,
                               int big_N,
                               int N,
                               int offset,
                               int stride,
                               int VL)
{
    if (N > 4) {
        ffts_hardcodedleaf_is_rec_even(is, big_N, N/2, offset, stride + 1, VL);

        if (N/4 >= 4) {
            ffts_hardcodedleaf_is_rec_odd(
                is, big_N, N/4, offset + (1 << stride), stride + 2, VL);
            ffts_hardcodedleaf_is_rec_odd(
                is, big_N, N/4, offset - (1 << stride), stride + 2, VL);
        } else {
            int temp = offset + (1 << stride);

            if (temp < 0) {
                temp += big_N;
            }

            temp *= 2;

            if (!(temp % (2 * VL))) {
                int i;

                (*is)[0] = offset + (1 << stride);
                (*is)[1] = offset + (1 << stride) + (1 << (stride + 2));
                (*is)[2] = offset - (1 << stride);
                (*is)[3] = offset - (1 << stride) + (1 << (stride + 2));

                for (i = 0; i < 4; i++) {
                    if ((*is)[i] < 0) {
                        (*is)[i] += big_N;
                    }
                }

                for (i = 0; i < 4; i++) {
                    (*is)[i] *= 2;
                }

                *is += 4;
            }
        }
    } else if (N == 4) {
        int perm[4];

        ffts_permute_addr(big_N, offset, stride, perm);

        if (!((2 * perm[0]) % (2 * VL))) {
            int i;

            for (i = 0; i < 4; i++) {
                (*is)[i] = 2 * perm[i];
            }

            *is += 4;
        }
    }
}

static void
ffts_hardcodedleaf_is_rec_odd(ptrdiff_t **is,
                              int big_N,
                              int N,
                              int offset,
                              int stride,
                              int VL)
{
    if (N > 4) {
        ffts_hardcodedleaf_is_rec_odd(is, big_N, N/2, offset, stride + 1, VL);

        if (N/4 >= 4) {
            ffts_hardcodedleaf_is_rec_odd(
                is, big_N, N/4, offset + (1 << stride), stride + 2, VL);
            ffts_hardcodedleaf_is_rec_odd(
                is, big_N, N/4, offset - (1 << stride), stride + 2, VL);
        } else {
            int temp = offset + (1 << stride);

            if (temp < 0) {
                temp += big_N;
            }

            temp *= 2;

            if (!(temp % (2 * VL))) {
                int i;

                (*is)[0] = offset + (1 << stride);
                (*is)[1] = offset + (1 << stride) + (1 << (stride + 2));
                (*is)[2] = offset - (1 << stride);
                (*is)[3] = offset - (1 << stride) + (1 << (stride + 2));

                for (i = 0; i < 4; i++) {
                    if ((*is)[i] < 0) {
                        (*is)[i] += big_N;
                    }
                }

                for (i = 0; i < 4; i++) {
                    (*is)[i] *= 2;
                }

                *is += 4;
            }
        }
    } else if (N == 4) {
        int perm[4];

        ffts_permute_addr(big_N, offset, stride, perm);

        if (!((2 * perm[0]) % (2 * VL))) {
            int i;

            for (i = 0; i < 4; i++) {
                (*is)[i] = 2 * perm[i];
            }

            *is += 4;
        }
    }
}
#endif

static ptrdiff_t*
ffts_init_is(size_t N, size_t leaf_N, int VL)
{
    int i, i0, i1, i2;
    int stride = ffts_ctzl(N/leaf_N);
    ptrdiff_t *is, *pis;

    is = malloc(N / VL * sizeof(*is));
    if (!is) {
        return NULL;
    }

    i0 = N/leaf_N/3 + 1;
    i1 = i2 = N/leaf_N/3;
    if ((N/leaf_N) % 3 > 1) {
        i1++;
    }

    pis = is;

#if LEAF_N == 8
    for (i = 0; i < i0; i++) {
        ffts_hardcodedleaf_is_rec_even8(
            &pis, N, i, stride, VL);
    }

    for (i = i0; i < i0 + i1; i++) {
        ffts_hardcodedleaf_is_rec_even4(
            &pis, N, i, stride + 1, VL);
        ffts_hardcodedleaf_is_rec_even4(
            &pis, N, i - (1 << stride), stride + 1, VL);
    }

    for (i = 0 - i2; i < 0; i++) {
        ffts_hardcodedleaf_is_rec_even8(
            &pis, N, i, stride, VL);
    }
#else
    for (i = 0; i < i0; i++) {
        ffts_hardcodedleaf_is_rec_even(
            &pis, N, leaf_N, i, stride, VL);
    }

    for (i = i0; i < i0 + i1; i++) {
        ffts_hardcodedleaf_is_rec_even(
            &pis, N, leaf_N / 2, i, stride + 1, VL);
        ffts_hardcodedleaf_is_rec_even(
            &pis, N, leaf_N / 2, i - (1 << stride), stride + 1, VL);
    }

    for (i = 0 - i2; i < 0; i++) {
        ffts_hardcodedleaf_is_rec_even(
            &pis, N, leaf_N, i, stride, VL);
    }
#endif

    return is;
}

static ptrdiff_t*
ffts_init_offsets(size_t N, size_t leaf_N)
{
    ptrdiff_t *offsets, *tmp;
    size_t i;

    offsets = malloc(N/leaf_N * sizeof(*offsets));
    if (!offsets) {
        return NULL;
    }

    tmp = malloc(2 * N/leaf_N * sizeof(*tmp));
    if (!tmp) {
        free(offsets);
        return NULL;
    }

#if LEAF_N == 8
    ffts_elaborate_offsets_even8(tmp, ffts_ctzl(N));
#else
    ffts_elaborate_offsets_even(tmp, leaf_N, N, 0, 0, 1);
#endif

    for (i = 0; i < 2*N/leaf_N; i += 2) {
        if (tmp[i] < 0) {
            tmp[i] += N;
        }
    }

    qsort(tmp, N/leaf_N, 2 * sizeof(*tmp), ffts_compare_offsets);

    for (i = 0; i < N/leaf_N; i++) {
        offsets[i] = 2 * tmp[2*i + 1];
    }

    free(tmp);
    return offsets;
}

#endif /* FFTS_PATTERNS_H */
