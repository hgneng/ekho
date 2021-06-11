/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2015-2016, Jukka Ojanen <jukka.ojanen@kolumbus.fi>
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

#ifndef FFTS_INTERNAL_H
#define FFTS_INTERNAL_H

#ifdef AUTOTOOLS_BUILD
#include "config.h"
#endif

#include "ffts_attributes.h"
#include "types.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_MM_ALLOC_H
#include <mm_malloc.h>
#ifndef HAVE__MM_MALLOC
#define HAVE__MM_MALLOC
#endif
#endif

#include <stddef.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#if defined(HAVE_DECL_ALIGNED_ALLOC) && !HAVE_DECL_ALIGNED_ALLOC
extern void *aligned_alloc(size_t, size_t);
#endif

#if defined(HAVE_DECL_MEMALIGN) && !HAVE_DECL_MEMALIGN
extern void *memalign(size_t, size_t);
#endif

#if defined(HAVE_DECL_POSIX_MEMALIGN) && !HAVE_DECL_POSIX_MEMALIGN
extern int posix_memalign(void **, size_t, size_t);
#endif

#if defined(HAVE_DECL_VALLOC) && !HAVE_DECL_VALLOC
extern void *valloc(size_t);
#endif

#ifdef _mm_malloc
#ifndef HAVE__MM_MALLOC
#define HAVE__MM_MALLOC
#endif
#endif

#ifdef ENABLE_LOG
#ifdef __ANDROID__
#include <android/log.h>
#define LOG(s) __android_log_print(ANDROID_LOG_ERROR, "FFTS", s)
#else
#define LOG(s) fprintf(stderr, s)
#endif
#else
#define LOG(s)
#endif

struct _ffts_plan_t;
typedef void (*transform_func_t)(struct _ffts_plan_t *p, const void *in, void *out);

/**
 * Contains all the Information need to perform FFT
 *
 *
 * DO NOT CHANGE THE ORDER OF MEMBERS
 * ASSEMBLY CODE USES HARD CODED OFFSETS TO REFERENCE
 * SOME OF THESE VARIABES!!
 */
struct _ffts_plan_t {

    /**
     *
     */
    ptrdiff_t *offsets;
#ifdef DYNAMIC_DISABLED
    /**
     * Twiddle factors
     */
    void *ws;

    /**
     * ee - 2 size x  size8
     * oo - 2 x size4 in parallel
     * oe -
     */
    void  *oe_ws, *eo_ws, *ee_ws;
#else
    void FFTS_ALIGN(32) *ws;
    void FFTS_ALIGN(32) *oe_ws, *eo_ws, *ee_ws;
#endif

    /**
     * Pointer into an array of precomputed indexes for the input data array
     */
    ptrdiff_t *is;

    /**
     * Twiddle Factor Indexes
     */
    size_t *ws_is;

    /**
     * Size of the loops for the base cases
     */
    size_t i0, i1, n_luts;

    /**
     * Size fo the Transform
     */
    size_t N;
    void *lastlut;

#ifdef __arm__
    size_t *temporary_fix_as_dynamic_code_assumes_fixed_offset;
#endif

    /**
     * Pointer to the dynamically generated function
     * that will execute the FFT
     */
    transform_func_t transform;

    /**
     * Pointer to the base memory address of
     * of the transform function
     */
    void *transform_base;

    /**
     * Size of the memory block contain the
     * generated code
     */
    size_t transform_size;

    /* pointer to the constant variable used by SSE for sign change */
    /* TODO: #ifdef HAVE_SSE */
    const void *constants;

    // multi-dimensional stuff:
    struct _ffts_plan_t **plans;
    int rank;
    size_t *Ns, *Ms;
    void *buf;

    void *transpose_buf;

    /**
     * Pointer to the destroy function
     * to clean up the plan after use
     * (differs for real and multi dimension transforms
     */
    void (*destroy)(struct _ffts_plan_t *);

    /**
     * Coefficiants for the real valued transforms
     */
    float *A, *B;

    size_t i2;
};

static FFTS_INLINE void*
ffts_aligned_malloc(size_t size)
{
    void *p;

    /* various ways to allocate aligned memory in order of preferance */
#if defined(HAVE_ALIGNED_ALLOC)
    p = aligned_alloc(32, size);
#elif defined(__ICC) || defined(__INTEL_COMPILER) || defined(HAVE__MM_MALLOC)
    p = (void*) _mm_malloc(size, 32);
#elif defined(HAVE_POSIX_MEMALIGN)
    if (posix_memalign(&p, 32, size))
        p = NULL;
#elif defined(HAVE_MEMALIGN)
    p = memalign(32, size);
#elif defined(__ALTIVEC__)
    p = vec_malloc(size);
#elif defined(_MSC_VER) || defined(WIN32)
    p = _aligned_malloc(size, 32);
#elif defined(HAVE_VALLOC)
    p = valloc(size);
#else
    p = malloc(size);
#endif

    return p;
}

static FFTS_INLINE
void ffts_aligned_free(void *p)
{
    /* order must match with ffts_aligned_malloc */
#if defined(HAVE_ALIGNED_ALLOC)
    free(p);
#elif defined(__ICC) || defined(__INTEL_COMPILER) || defined(HAVE__MM_MALLOC)
    _mm_free(p);
#elif defined(HAVE_POSIX_MEMALIGN) || defined(HAVE_MEMALIGN)
    free(p);
#elif defined(__ALTIVEC__)
    vec_free(p);
#elif defined(_MSC_VER) || defined(WIN32)
    _aligned_free(p);
#else
    /* valloc or malloc */
    free(p);
#endif
}

#if GCC_VERSION_AT_LEAST(3,3)
#define ffts_ctzl __builtin_ctzl

static FFTS_INLINE size_t
ffts_next_power_of_2(size_t N)
{
    return 1 << (32 - __builtin_clzl(N));
}
#elif defined(_MSC_VER)
#include <intrin.h>
#ifdef _M_X64
#pragma intrinsic(_BitScanForward64)
static FFTS_INLINE unsigned long
ffts_ctzl(size_t N)
{
    unsigned long count;
    _BitScanForward64((unsigned long*) &count, N);
    return count;
}

#pragma intrinsic(_BitScanReverse64)
static FFTS_INLINE size_t
ffts_next_power_of_2(size_t N)
{
    unsigned long log_2;
    _BitScanReverse64((unsigned long*)&log_2, N);
    return 1ULL << (log_2 + 1);
}
#else
#pragma intrinsic(_BitScanForward)
static FFTS_INLINE unsigned long
ffts_ctzl(size_t N)
{
    unsigned long count;
    _BitScanForward((unsigned long*) &count, N);
    return count;
}

#pragma intrinsic(_BitScanReverse)
static FFTS_INLINE size_t
ffts_next_power_of_2(size_t N)
{
    unsigned long log_2;
    _BitScanReverse((unsigned long*)&log_2, N);
    return 1 << (log_2 + 1);
}
#endif /* _WIN64 */
#endif /* _MSC_VER */

#endif /* FFTS_INTERNAL_H */
