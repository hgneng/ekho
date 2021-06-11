/*

 This file is part of FFTS.

 Copyright (c) 2012, Anthony M. Blake
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

#ifndef FFTS_H
#define FFTS_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#if (defined(_WIN32) || defined(WIN32)) && defined(FFTS_SHARED)
#  ifdef FFTS_BUILD
#    define FFTS_API __declspec(dllexport)
#  else
#    define FFTS_API __declspec(dllimport)
#  endif
#else
#  if (__GNUC__ >= 4) || defined(HAVE_GCC_VISIBILITY)
#    define FFTS_API __attribute__ ((visibility("default")))
#  else
#    define FFTS_API
#  endif
#endif

/* The direction of the transform
   (i.e, the sign of the exponent in the transform.)
*/
#define FFTS_FORWARD (-1)
#define FFTS_BACKWARD (+1)

struct _ffts_plan_t;
typedef struct _ffts_plan_t ffts_plan_t;

/* Complex data is stored in the interleaved format
   (i.e, the real and imaginary parts composing each
   element of complex data are stored adjacently in memory)

   The multi-dimensional arrays passed are expected to be
   stored as a single contiguous block in row-major order
*/
FFTS_API ffts_plan_t*
ffts_init_1d(size_t N, int sign);

FFTS_API ffts_plan_t*
ffts_init_2d(size_t N1, size_t N2, int sign);

FFTS_API ffts_plan_t*
ffts_init_nd(int rank, size_t *Ns, int sign);

/* For real transforms, sign == FFTS_FORWARD implies a real-to-complex
   forwards tranform, and sign == FFTS_BACKWARD implies a complex-to-real
   backwards transform.

   The output of a real-to-complex transform is N/2+1 complex numbers,
   where the redundant outputs have been omitted.
*/
FFTS_API ffts_plan_t*
ffts_init_1d_real(size_t N, int sign);

FFTS_API ffts_plan_t*
ffts_init_2d_real(size_t N1, size_t N2, int sign);

FFTS_API ffts_plan_t*
ffts_init_nd_real(int rank, size_t *Ns, int sign);

FFTS_API void
ffts_execute(ffts_plan_t *p, const void *input, void *output);

FFTS_API void
ffts_free(ffts_plan_t *p);

#ifdef __cplusplus
}
#endif

#endif /* FFTS_H */
