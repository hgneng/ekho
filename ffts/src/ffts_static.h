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

#ifndef FFTS_STATIC_H
#define FFTS_STATIC_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#include "ffts.h"

void
ffts_small_2_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_2_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_forward4_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_forward4_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_backward4_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_backward4_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_forward8_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_forward8_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_backward8_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_backward8_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_forward16_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_forward16_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_backward16_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_small_backward16_64f(ffts_plan_t *p, const void *in, void *out);

void
ffts_static_transform_f_32f(ffts_plan_t *p, const void *in, void *out);

void
ffts_static_transform_i_32f(ffts_plan_t *p, const void *in, void *out);

#endif /* FFTS_STATIC_H */
