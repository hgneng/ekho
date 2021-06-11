/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2015-2016, Jukka Ojanen <jukka.ojanen@kolumbus.fi>

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

#ifndef FFTS_TRIG_H
#define FFTS_TRIG_H

#if defined (_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#include "ffts_internal.h"

/* calculate cos(pi * n / d) and sin(pi * n / d) with maximum error less than 1 ULP, average ~0.5 ULP */
int
ffts_cexp_32f(size_t n, size_t d, float *output);

int
ffts_generate_chirp_32f(ffts_cpx_32f *const table, size_t table_size);

/* generate cosine and sine tables with maximum error less than 1 ULP, average ~0.5 ULP */
int
ffts_generate_cosine_sine_32f(ffts_cpx_32f *const table, size_t table_size);

int
ffts_generate_cosine_sine_pow2_32f(ffts_cpx_32f *const table, int table_size);

int
ffts_generate_cosine_sine_pow2_64f(ffts_cpx_64f *const table, int table_size);

int
ffts_generate_table_1d_real_32f(struct _ffts_plan_t *const p,
                                int sign,
                                int invert);

#endif /* FFTS_TRIG_H */
