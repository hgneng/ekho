/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

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

#ifndef FFTS_DD_H
#define FFTS_DD_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#include "ffts_attributes.h"

#if HAVE_SSE2
#include <emmintrin.h>
#endif

/* double-double number */
struct ffts_dd_t
{
    double hi;
    double lo;
};

#if HAVE_SSE2
/* double-double vector */
struct ffts_dd2_t {
    __m128d hi;
    __m128d lo;
};
#endif

static FFTS_INLINE struct ffts_dd_t
ffts_dd_add_dd_unnormalized(const struct ffts_dd_t a,
                            const struct ffts_dd_t b);

static FFTS_INLINE struct ffts_dd_t
ffts_dd_mul_dd_unnormalized(const struct ffts_dd_t a,
                            const struct ffts_dd_t b);

static FFTS_INLINE struct ffts_dd_t
ffts_dd_split(double a);

/* aka quick-two-sum */
static FFTS_INLINE struct ffts_dd_t
ffts_dd_add(double a, double b)
{
    struct ffts_dd_t dd;
    dd.hi = a + b;
    dd.lo = b - (dd.hi - a);
    return dd;
}

static FFTS_INLINE struct ffts_dd_t
ffts_dd_add_dd(const struct ffts_dd_t a,
               const struct ffts_dd_t b)
{
    struct ffts_dd_t t1 = ffts_dd_add_dd_unnormalized(a, b);
    return ffts_dd_add(t1.hi, t1.lo);
}

static FFTS_INLINE struct ffts_dd_t
ffts_dd_add_dd_unnormalized(const struct ffts_dd_t a,
                            const struct ffts_dd_t b)
{
    struct ffts_dd_t dd;
    double e1;
    dd.hi = a.hi + b.hi;
    e1 = dd.hi - a.hi;
    dd.lo = ((a.hi - (dd.hi - e1)) + (b.hi - e1)) + (a.lo + b.lo);
    return dd;
}

static FFTS_INLINE struct ffts_dd_t
ffts_dd_mul(const double a, const double b)
{
    struct ffts_dd_t dd;
    struct ffts_dd_t t1 = ffts_dd_split(a);
    struct ffts_dd_t t2 = ffts_dd_split(b);
    dd.hi = a * b;
    dd.lo = (t1.hi * t2.hi - dd.hi);
    dd.lo += (t1.hi * t2.lo + t1.lo * t2.hi);
    dd.lo += t1.lo * t2.lo;
    return dd;
}

static FFTS_INLINE struct ffts_dd_t
ffts_dd_mul_dd(const struct ffts_dd_t a,
               const struct ffts_dd_t b)
{
    struct ffts_dd_t dd = ffts_dd_mul_dd_unnormalized(a, b);
    return ffts_dd_add(dd.hi, dd.lo);
}

static FFTS_INLINE struct ffts_dd_t
ffts_dd_mul_dd_unnormalized(const struct ffts_dd_t a,
                            const struct ffts_dd_t b)
{
    struct ffts_dd_t dd = ffts_dd_mul(a.hi, b.hi);
    dd.lo += (a.hi * b.lo + a.lo * b.hi);
    return dd;
}

static FFTS_INLINE struct ffts_dd_t
ffts_dd_split(double a)
{
    /* 2^27+1 = 134217729 */
    struct ffts_dd_t dd;
    double t = 134217729.0 * a;
    dd.hi = t - (t - a);
    dd.lo = a - dd.hi;
    return dd;
}

#if HAVE_SSE2
static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_add_dd2_unnormalized(const struct ffts_dd2_t *const FFTS_RESTRICT a,
                              const struct ffts_dd2_t *const FFTS_RESTRICT b);

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_mul_dd2_unnormalized(const struct ffts_dd2_t *const FFTS_RESTRICT a,
                              const struct ffts_dd2_t *const FFTS_RESTRICT b);

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_split(__m128d a);

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_add(__m128d a, __m128d b)
{
    struct ffts_dd2_t dd2;
    dd2.hi = _mm_add_pd(a, b);
    dd2.lo = _mm_sub_pd(b, _mm_sub_pd(dd2.hi, a));
    return dd2;
}

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_add_dd2(const struct ffts_dd2_t *const FFTS_RESTRICT a,
                 const struct ffts_dd2_t *const FFTS_RESTRICT b)
{
    struct ffts_dd2_t t1 = ffts_dd2_add_dd2_unnormalized(a, b);
    return ffts_dd2_add(t1.hi, t1.lo);
}

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_add_dd2_unnormalized(const struct ffts_dd2_t *const FFTS_RESTRICT a,
                              const struct ffts_dd2_t *const FFTS_RESTRICT b)
{
    struct ffts_dd2_t dd2;
    __m128d e1;
    dd2.hi = _mm_add_pd(a->hi, b->hi);
    e1 = _mm_sub_pd(dd2.hi, a->hi);
    dd2.lo = _mm_add_pd(_mm_add_pd(_mm_sub_pd(a->hi, _mm_sub_pd(dd2.hi, e1)),
        _mm_sub_pd(b->hi, e1)), _mm_add_pd(a->lo, b->lo));
    return dd2;
}

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_mul(const __m128d a, const __m128d b)
{
    struct ffts_dd2_t dd2;
    struct ffts_dd2_t t1 = ffts_dd2_split(a);
    struct ffts_dd2_t t2 = ffts_dd2_split(b);
    dd2.hi = _mm_mul_pd(a, b);
    dd2.lo = _mm_add_pd(_mm_add_pd(_mm_sub_pd(
        _mm_mul_pd(t1.hi, t2.hi), dd2.hi),
        _mm_add_pd(_mm_mul_pd(t1.hi, t2.lo),
        _mm_mul_pd(t1.lo, t2.hi))),
        _mm_mul_pd(t1.lo, t2.lo));
    return dd2;
}

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_mul_dd2(const struct ffts_dd2_t *const FFTS_RESTRICT a,
                 const struct ffts_dd2_t *const FFTS_RESTRICT b)
{
    struct ffts_dd2_t dd2 = ffts_dd2_mul_dd2_unnormalized(a, b);
    return ffts_dd2_add(dd2.hi, dd2.lo);
}

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_mul_dd2_unnormalized(const struct ffts_dd2_t *const FFTS_RESTRICT a,
                              const struct ffts_dd2_t *const FFTS_RESTRICT b)
{
    struct ffts_dd2_t dd2 = ffts_dd2_mul(a->hi, b->hi);
    dd2.lo = _mm_add_pd(dd2.lo, _mm_add_pd(
        _mm_mul_pd(a->hi, b->lo), _mm_mul_pd(a->lo, b->hi)));
    return dd2;
}

static FFTS_INLINE struct ffts_dd2_t
ffts_dd2_split(__m128d a)
{
    /* 2^27+1 = 134217729 */
    struct ffts_dd2_t dd2;
    __m128d t = _mm_mul_pd(a, _mm_set1_pd(134217729.0));
    dd2.hi = _mm_sub_pd(t, _mm_sub_pd(t, a));
    dd2.lo = _mm_sub_pd(a, dd2.hi);
    return dd2;
}
#endif /* HAVE_SSE2 */

#endif /* FFTS_DD_H */
