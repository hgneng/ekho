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

#ifndef FFTS_NEON_H
#define FFTS_NEON_H

#include "ffts.h"

void neon_x4(float *, size_t, float *);
void neon_x8(float *, size_t, float *);
void neon_x8_t(float *, size_t, float *);
void neon_ee();
void neon_oo();
void neon_eo();
void neon_oe();
void neon_end();

void neon_transpose4(uint64_t *in, uint64_t *out, int w, int h); 
void neon_transpose8(uint64_t *in, uint64_t *out, int w, int h); 

void neon_static_e_f(ffts_plan_t*, const void*, void*);
void neon_static_o_f(ffts_plan_t*, const void*, void*);
void neon_static_x4_f(float*, const float*);
void neon_static_x8_f(float*, size_t, const float*);
void neon_static_x8_t_f(float*, size_t, const float*);

void neon_static_e_i(ffts_plan_t*, const void*, void*);
void neon_static_o_i(ffts_plan_t*, const void*, void*);
void neon_static_x4_i(float*, const float*);
void neon_static_x8_i(float*, size_t, const float*);
void neon_static_x8_t_i(float*, size_t, const float*);

#endif /* FFTS_NEON_H */
