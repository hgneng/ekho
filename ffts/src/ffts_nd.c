/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2016, Jukka Ojanen <jukka.ojanen@kolumbus.fi>
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

#include "ffts_nd.h"
#include "ffts_internal.h"
#include "ffts_transpose.h"

static void
ffts_free_nd(ffts_plan_t *p)
{
    if (p->plans) {
        int i, j;

        for (i = 0; i < p->rank; i++) {
            ffts_plan_t *plan = p->plans[i];

            if (plan) {
                for (j = 0; j < i; j++) {
                    if (p->Ns[i] == p->Ns[j]) {
                        plan = NULL;
                        break;
                    }
                }

                if (plan) {
                    ffts_free(plan);
                }
            }
        }

        free(p->plans);
    }

    if (p->Ns) {
        free(p->Ns);
    }

    if (p->Ms) {
        free(p->Ms);
    }

    if (p->buf) {
        ffts_aligned_free(p->buf);
    }

    free(p);
}

static void
ffts_execute_nd(ffts_plan_t *p, const void *in, void *out)
{
    uint64_t *din = (uint64_t*) in;
    uint64_t *buf = p->buf;
    uint64_t *dout = (uint64_t*) out;

    ffts_plan_t *plan;
    int i;
    size_t j;

    plan = p->plans[0];
    for (j = 0; j < p->Ms[0]; j++) {
        plan->transform(plan, din + (j * p->Ns[0]), buf + (j * p->Ns[0]));
    }

    ffts_transpose(buf, dout, p->Ns[0], p->Ms[0]);

    for (i = 1; i < p->rank; i++) {
        plan = p->plans[i];

        for (j = 0; j < p->Ms[i]; j++) {
            plan->transform(plan, dout + (j * p->Ns[i]), buf + (j * p->Ns[i]));
        }

        ffts_transpose(buf, dout, p->Ns[i], p->Ms[i]);
    }
}

FFTS_API ffts_plan_t*
ffts_init_nd(int rank, size_t *Ns, int sign)
{
    ffts_plan_t *p;
    size_t vol = 1;
    int i, j;

    if (!Ns) {
        return NULL;
    }

    if (rank == 1) {
         return ffts_init_1d(Ns[0], sign);
    }

    p = calloc(1, sizeof(*p));
    if (!p) {
        return NULL;
    }

    p->transform = &ffts_execute_nd;
    p->destroy   = &ffts_free_nd;
    p->rank      = rank;

    p->Ms = malloc(rank * sizeof(*p->Ms));
    if (!p->Ms) {
        goto cleanup;
    }

    p->Ns = malloc(rank * sizeof(*p->Ns));
    if (!p->Ns) {
        goto cleanup;
    }

    /* reverse the order */
    for (i = 0; i < rank; i++) {
        size_t N = Ns[rank - i - 1];
        p->Ns[i] = N;
        vol *= N;
    }

    p->buf = ffts_aligned_malloc(2 * vol * sizeof(float));
    if (!p->buf) {
        goto cleanup;
    }

    p->plans = calloc(rank, sizeof(*p->plans));
    if (!p->plans) {
        goto cleanup;
    }

    for (i = 0; i < rank; i++) {
        p->Ms[i] = vol / p->Ns[i];

        for (j = 0; j < i; j++) {
            if (p->Ns[i] == p->Ns[j]) {
                p->plans[i] = p->plans[j];
                break;
            }
        }

        if (!p->plans[i]) {
            p->plans[i] = ffts_init_1d(p->Ns[i], sign);
            if (!p->plans) {
                goto cleanup;
            }
        }
    }

    return p;

cleanup:
    ffts_free_nd(p);
    return NULL;
}

FFTS_API ffts_plan_t*
ffts_init_2d(size_t N1, size_t N2, int sign)
{
    size_t Ns[2];

    Ns[0] = N1; /* x */
    Ns[1] = N2; /* y */
    return ffts_init_nd(2, Ns, sign);
}
