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

#ifndef FFTS_ATTRIBUTES_H
#define FFTS_ATTRIBUTES_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Macro definitions for various function/variable attributes */
#ifdef __GNUC__
#define GCC_VERSION_AT_LEAST(x,y) \
	(__GNUC__ > x || __GNUC__ == x && __GNUC_MINOR__ >= y)
#else
#define GCC_VERSION_AT_LEAST(x,y) 0
#endif

#ifdef __GNUC__
#define FFTS_ALIGN(x) __attribute__((aligned(x)))
#elif defined(_MSC_VER)
#define FFTS_ALIGN(x) __declspec(align(x))
#else
#define FFTS_ALIGN(x)
#endif

#if GCC_VERSION_AT_LEAST(3,1)
#define FFTS_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define FFTS_ALWAYS_INLINE __forceinline
#else
#define FFTS_ALWAYS_INLINE inline
#endif

#if defined(_MSC_VER)
#define FFTS_INLINE __inline
#else
#define FFTS_INLINE inline
#endif

#if defined(__GNUC__)
#define FFTS_RESTRICT __restrict
#elif defined(_MSC_VER)
#define FFTS_RESTRICT __restrict
#else
#define FFTS_RESTRICT
#endif

#if GCC_VERSION_AT_LEAST(4,5)
#define FFTS_ASSUME(cond) do { if (!(cond)) __builtin_unreachable(); } while (0)
#elif defined(_MSC_VER)
#define FFTS_ASSUME(cond) __assume(cond)
#else
#define FFTS_ASSUME(cond)
#endif

#if GCC_VERSION_AT_LEAST(4,7)
#define FFTS_ASSUME_ALIGNED_16(x) __builtin_assume_aligned(x, 16)
#else
#define FFTS_ASSUME_ALIGNED_16(x) x
#endif

#if GCC_VERSION_AT_LEAST(4,7)
#define FFTS_ASSUME_ALIGNED_32(x) __builtin_assume_aligned(x, 32)
#else
#define FFTS_ASSUME_ALIGNED_32(x) x
#endif

#if defined(__GNUC__)
#define FFTS_LIKELY(cond) __builtin_expect(!!(cond), 1)
#else
#define FFTS_LIKELY(cond) cond
#endif

#if defined(__GNUC__)
#define FFTS_UNLIKELY(cond) __builtin_expect(!!(cond), 0)
#else
#define FFTS_UNLIKELY(cond) cond
#endif

#endif /* FFTS_ATTRIBUTES_H */
