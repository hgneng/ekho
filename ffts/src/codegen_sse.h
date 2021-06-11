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

#ifndef FFTS_CODEGEN_SSE_H
#define FFTS_CODEGEN_SSE_H

#define TARGET_AMD64
#include "arch/x64/x64-codegen.h"

#include <assert.h>

static const FFTS_ALIGN(16) unsigned int sse_constants[20] = {
    /* 0.0, -0.0, 0.0, -0.0 */
    0x00000000, 0x80000000, 0x00000000, 0x80000000,
    /* 0.707, 0.707, 0.707, 0.707 */
    0x3f3504f3, 0x3f3504f3, 0x3f3504f3, 0x3f3504f3,
    /* -0.707, 0.707, -0.707, 0.707 */
    0xbf3504f3, 0x3f3504f3, 0xbf3504f3, 0x3f3504f3,
    /* 1.0, 1.0, 0.707, 0.707 */
    0x3f800000, 0x3f800000, 0x3f3504f3, 0x3f3504f3,
    /* 0.0, 0.0, -.707, 0.707 */
    0x00000000, 0x00000000, 0xbf3504f3, 0x3f3504f3
};

static const FFTS_ALIGN(16) unsigned int sse_constants_inv[20] = {
    /* -0.0, 0.0, -0.0, 0.0 */
    0x80000000, 0x00000000, 0x80000000, 0x00000000,
    /* 0.707, 0.707, 0.707, 0.707 */
    0x3f3504f3, 0x3f3504f3, 0x3f3504f3, 0x3f3504f3,
    /* 0.707, -0.707, 0.707, -0.707 */
    0x3f3504f3, 0xbf3504f3, 0x3f3504f3, 0xbf3504f3,
    /* 1.0, 1.0, 0.707, 0.707 */
    0x3f800000, 0x3f800000, 0x3f3504f3, 0x3f3504f3,
    /* 0.0, 0.0, 0.707, -0.707 */
    0x00000000, 0x00000000, 0x3f3504f3, 0xbf3504f3
};

#define P(x) (*(*p)++ = x)

static FFTS_INLINE void
ffts_insert_nops(uint8_t **p, uint32_t count)
{
    if (count >= 9) {
        P(0x66);
        P(0x0F);
        P(0x1F);
        P(0x84);
        P(0x00);
        P(0x00);
        P(0x00);
        P(0x00);
        P(0x00);

        if (count > 9) {
            ffts_insert_nops(p, count - 9);
        }
    } else {
        switch(count) {
        case 0:
            break;
        case 2:
            P(0x66);
        /* fall through */
        case 1:
            P(0x90);
            break;
        case 3:
            P(0x0F);
            P(0x1F);
            P(0x00);
            break;
        case 4:
            P(0x0F);
            P(0x1F);
            P(0x40);
            P(0x00);
            break;
        case 5:
            P(0x0F);
            P(0x1F);
            P(0x44);
            P(0x00);
            P(0x00);
            break;
        case 6:
            P(0x66);
            P(0x0F);
            P(0x1F);
            P(0x44);
            P(0x00);
            P(0x00);
            break;
        case 7:
            P(0x0F);
            P(0x1F);
            P(0x80);
            P(0x00);
            P(0x00);
            P(0x00);
            P(0x00);
            break;
        case 8:
        default:
            P(0x0F);
            P(0x1F);
            P(0x84);
            P(0x00);
            P(0x00);
            P(0x00);
            P(0x00);
            P(0x00);
            break;
        }
    }
}

static FFTS_INLINE void
ffts_align_mem16(uint8_t **p, uint32_t offset)
{
    int r = (16 - (offset & 0xf)) - (int) ((uintptr_t)(*p) & 0xf);
    r = (16 + r) & 0xf;
    ffts_insert_nops(p, r);
}

static FFTS_INLINE void
generate_epilogue(insns_t **fp)
{
#ifdef _M_X64
    /* restore nonvolatile registers */
    x64_mov_reg_membase(*fp, X64_RBX, X64_RSP, -64, 8);
    x64_mov_reg_membase(*fp, X64_RSI, X64_RSP, -56, 8);

    x64_sse_movaps_reg_membase(*fp, X64_XMM6,  X64_RSP, -48);
    x64_sse_movaps_reg_membase(*fp, X64_XMM7,  X64_RSP, -32);
    x64_sse_movaps_reg_membase(*fp, X64_XMM8,  X64_RSP, -16);
    x64_sse_movaps_reg_membase(*fp, X64_XMM9,  X64_RSP,   0);
    x64_sse_movaps_reg_membase(*fp, X64_XMM10, X64_RSP,  16);
    x64_sse_movaps_reg_membase(*fp, X64_XMM11, X64_RSP,  32);
    x64_sse_movaps_reg_membase(*fp, X64_XMM12, X64_RSP,  48);
    x64_sse_movaps_reg_membase(*fp, X64_XMM13, X64_RSP,  64);

    /* restore the last 2 registers from the shadow space */
    x64_sse_movaps_reg_membase(*fp, X64_XMM14, X64_RSP,  96);
    x64_sse_movaps_reg_membase(*fp, X64_XMM15, X64_RSP, 112);

    /* restore stack */
    x64_alu_reg_imm_size(*fp, X86_ADD, X64_RSP, 88, 8);
#else
    x64_pop_reg(*fp, X64_R15);
    x64_pop_reg(*fp, X64_R14);
    x64_pop_reg(*fp, X64_R13);
    x64_pop_reg(*fp, X64_R12);
    x64_pop_reg(*fp, X64_R11);
    x64_pop_reg(*fp, X64_R10);
    x64_pop_reg(*fp, X64_RBX);
    x64_pop_reg(*fp, X64_RBP);
#endif

    x64_ret(*fp);
}

static FFTS_INLINE insns_t*
generate_prologue(insns_t **fp, ffts_plan_t *p)
{
    insns_t *start;

    /* unreferenced parameter */
    (void) p;

    /* align call destination */
    ffts_align_mem16(fp, 0);
    start = *fp;

    /* save nonvolatile registers */
#ifdef _M_X64
    /* reserve space to save XMM6-XMM15 registers */
    x64_alu_reg_imm_size(*fp, X86_SUB, X64_RSP, 88, 8);

    x64_mov_membase_reg(*fp, X64_RSP, -64, X64_RBX, 8);
    x64_mov_membase_reg(*fp, X64_RSP, -56, X64_RSI, 8);

    x64_sse_movaps_membase_reg(*fp, X64_RSP, -48, X64_XMM6);
    x64_sse_movaps_membase_reg(*fp, X64_RSP, -32, X64_XMM7);
    x64_sse_movaps_membase_reg(*fp, X64_RSP, -16, X64_XMM8);
    x64_sse_movaps_membase_reg(*fp, X64_RSP,   0, X64_XMM9);
    x64_sse_movaps_membase_reg(*fp, X64_RSP,  16, X64_XMM10);
    x64_sse_movaps_membase_reg(*fp, X64_RSP,  32, X64_XMM11);
    x64_sse_movaps_membase_reg(*fp, X64_RSP,  48, X64_XMM12);
    x64_sse_movaps_membase_reg(*fp, X64_RSP,  64, X64_XMM13);

    /* use the shadow space to save last 2 registers */
    x64_sse_movaps_membase_reg(*fp, X64_RSP,  96, X64_XMM14);
    x64_sse_movaps_membase_reg(*fp, X64_RSP, 112, X64_XMM15);
#else
    x64_push_reg(*fp, X64_RBP);
    x64_push_reg(*fp, X64_RBX);
    x64_push_reg(*fp, X64_R10);
    x64_push_reg(*fp, X64_R11);
    x64_push_reg(*fp, X64_R12);
    x64_push_reg(*fp, X64_R13);
    x64_push_reg(*fp, X64_R14);
    x64_push_reg(*fp, X64_R15);
#endif

    return start;
}

static FFTS_INLINE void
generate_transform_init(insns_t **fp)
{
#ifdef _M_X64
    /* generate function */
    x64_sse_movaps_reg_membase(*fp, X64_XMM3, X64_RSI, 0);

    /* set "pointer" to twiddle factors */
    x64_mov_reg_membase(*fp, X64_R9, X64_RCX, 0x20, 8);
#else
    /* generate function */
    x64_sse_movaps_reg_membase(*fp, X64_XMM3, X64_R9, 0);

    /* set "pointer" to twiddle factors */
    x64_mov_reg_membase(*fp, X64_R8, X64_RDI, 0x20, 8);
#endif
}

static FFTS_INLINE insns_t*
generate_size4_base_case(insns_t **fp, int sign)
{
    insns_t *ins;
    insns_t *x4_addr;

    /* unreferenced parameter */
    (void) sign;

    /* to avoid deferring */
    ins = *fp;

    /* align call destination */
    ffts_align_mem16(&ins, 0);
    x4_addr = ins;

#ifdef _M_X64
    /* generate function */
    x64_sse_movaps_reg_membase(ins, X64_XMM0, X64_R8, 64);
    x64_sse_movaps_reg_membase(ins, X64_XMM1, X64_R8, 96);
    x64_sse_movaps_reg_membase(ins, X64_XMM7, X64_R8,  0);
    x64_sse_movaps_reg_membase(ins, X64_XMM4, X64_R9,  0);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM4);
    x64_sse_movaps_reg_membase(ins, X64_XMM2, X64_R9, 16);
    x64_sse_mulps_reg_reg(ins, X64_XMM6, X64_XMM0);
    x64_sse_mulps_reg_reg(ins, X64_XMM4, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM0, X64_XMM0, 0xB1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM1, X64_XMM1, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM0, X64_XMM2);
    x64_sse_mulps_reg_reg(ins, X64_XMM2, X64_XMM1);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM0);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM2);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM4);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM4);
    x64_sse_movaps_reg_membase(ins, X64_XMM8, X64_R8, 32);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM6, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM6, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_movaps_reg_membase(ins, X64_XMM12, X64_R8, 112);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM5);
    x64_sse_addps_reg_reg(ins, X64_XMM7, X64_XMM5);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM6);

    x64_sse_movaps_membase_reg(ins, X64_R8,  0, X64_XMM7);
    x64_sse_movaps_membase_reg(ins, X64_R8, 32, X64_XMM8);
    x64_sse_movaps_membase_reg(ins, X64_R8, 64, X64_XMM9);
    x64_sse_movaps_membase_reg(ins, X64_R8, 96, X64_XMM10);

    x64_sse_movaps_reg_membase(ins, X64_XMM14, X64_R9, 32);
    x64_sse_movaps_reg_membase(ins, X64_XMM11, X64_R8, 80);
    x64_sse_movaps_reg_reg(ins, X64_XMM0, X64_XMM14);
    x64_sse_movaps_reg_membase(ins, X64_XMM13, X64_R9, 48);
    x64_sse_mulps_reg_reg(ins, X64_XMM0, X64_XMM11);
    x64_sse_mulps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM11, 0xB1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM13);
    x64_sse_mulps_reg_reg(ins, X64_XMM13, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM13);
    x64_sse_movaps_reg_reg(ins, X64_XMM15, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM15, X64_XMM14);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM0, X64_XMM3);

    x64_sse_movaps_reg_membase(ins, X64_XMM1, X64_R8, 16);
    x64_sse_movaps_reg_membase(ins, X64_XMM2, X64_R8, 48);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM0, X64_XMM0, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM2);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM15);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM0);

    x64_sse_movaps_membase_reg(ins, X64_R8,  16, X64_XMM1);
    x64_sse_movaps_membase_reg(ins, X64_R8,  48, X64_XMM2);
    x64_sse_movaps_membase_reg(ins, X64_R8,  80, X64_XMM4);
    x64_sse_movaps_membase_reg(ins, X64_R8, 112, X64_XMM5);

    x64_ret(ins);
#else
    /* generate function */
    x64_sse_movaps_reg_membase(ins, X64_XMM0, X64_RDX, 64);
    x64_sse_movaps_reg_membase(ins, X64_XMM1, X64_RDX, 96);
    x64_sse_movaps_reg_membase(ins, X64_XMM7, X64_RDX,  0);
    x64_sse_movaps_reg_membase(ins, X64_XMM4, X64_R8,   0);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM4);
    x64_sse_movaps_reg_membase(ins, X64_XMM2, X64_R8, 16);
    x64_sse_mulps_reg_reg(ins, X64_XMM6, X64_XMM0);
    x64_sse_mulps_reg_reg(ins, X64_XMM4, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM0, X64_XMM0, 0xB1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM1, X64_XMM1, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM0, X64_XMM2);
    x64_sse_mulps_reg_reg(ins, X64_XMM2, X64_XMM1);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM0);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM2);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM4);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM4);
    x64_sse_movaps_reg_membase(ins, X64_XMM8, X64_RDX, 32);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM6, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM6, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_movaps_reg_membase(ins, X64_XMM12, X64_RDX, 112);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM5);
    x64_sse_addps_reg_reg(ins, X64_XMM7, X64_XMM5);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM6);

    x64_sse_movaps_membase_reg(ins, X64_RDX,  0, X64_XMM7);
    x64_sse_movaps_membase_reg(ins, X64_RDX, 32, X64_XMM8);
    x64_sse_movaps_membase_reg(ins, X64_RDX, 64, X64_XMM9);
    x64_sse_movaps_membase_reg(ins, X64_RDX, 96, X64_XMM10);

    x64_sse_movaps_reg_membase(ins, X64_XMM14, X64_R8, 32);
    x64_sse_movaps_reg_membase(ins, X64_XMM11, X64_RDX, 80);
    x64_sse_movaps_reg_reg(ins, X64_XMM0, X64_XMM14);
    x64_sse_movaps_reg_membase(ins, X64_XMM13, X64_R8, 48);
    x64_sse_mulps_reg_reg(ins, X64_XMM0, X64_XMM11);
    x64_sse_mulps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM11, 0xB1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM13);
    x64_sse_mulps_reg_reg(ins, X64_XMM13, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM13);
    x64_sse_movaps_reg_reg(ins, X64_XMM15, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM15, X64_XMM14);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM0, X64_XMM3);

    x64_sse_movaps_reg_membase(ins, X64_XMM1, X64_RDX, 16);
    x64_sse_movaps_reg_membase(ins, X64_XMM2, X64_RDX, 48);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM0, X64_XMM0, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM2);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM15);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM0);

    x64_sse_movaps_membase_reg(ins, X64_RDX,  16, X64_XMM1);
    x64_sse_movaps_membase_reg(ins, X64_RDX,  48, X64_XMM2);
    x64_sse_movaps_membase_reg(ins, X64_RDX,  80, X64_XMM4);
    x64_sse_movaps_membase_reg(ins, X64_RDX, 112, X64_XMM5);

    x64_ret(ins);
#endif

    *fp = ins;
    return x4_addr;
}

static FFTS_INLINE void
generate_leaf_init(insns_t **fp, uint32_t loop_count)
{
    /* to avoid deferring */
    insns_t *ins = *fp;

#ifdef _M_X64
    /* set loop counter */
    x86_mov_reg_imm(ins, X86_EBX, loop_count);

    /* generate function */

    /* clear */
    x86_clear_reg(ins, X86_EAX);

    /* set "pointer" to offsets */
    x64_mov_reg_membase(ins, X64_R9, X64_RCX,
        offsetof(struct _ffts_plan_t, offsets), 8);

    /* set "pointer" to constants */
    x64_mov_reg_membase(ins, X64_RSI, X64_RCX,
        offsetof(struct _ffts_plan_t, constants), 8);
    
    /* use XMM3 for sign change */
    x64_sse_movaps_reg_membase(ins, X64_XMM3, X64_RSI, 0);
#else
    /* set loop counter */
    x86_mov_reg_imm(ins, X86_ECX, loop_count);

    /* generate function */

    /* clear */
    x86_clear_reg(ins, X86_EAX);

    /* set "pointer" to offsets */
    x64_mov_reg_membase(ins, X64_R8, X64_RDI,
        offsetof(struct _ffts_plan_t, offsets), 8);

    /* set "pointer" to constants */
    x64_mov_reg_membase(ins, X64_R9, X64_RDI,
        offsetof(struct _ffts_plan_t, constants), 8);

    /* align loop/jump destination */
    ffts_align_mem16(&ins, 9);
#endif

    *fp = ins;
}

static FFTS_INLINE void
generate_leaf_ee(insns_t **fp, uint32_t *offsets, int extend)
{
    insns_t *leaf_ee_loop;

    /* to avoid deferring */
    insns_t *ins = *fp;

#ifdef _M_X64
    x64_sse_movaps_reg_membase_size(ins, X64_XMM0, X64_RSI, 32, 1);

    /* beginning of the loop (make sure it's 16 byte aligned) */
    leaf_ee_loop = ins;
    assert(!(((uintptr_t) leaf_ee_loop) & 0xF));

    x64_sse_movaps_reg_memindex(ins, X64_XMM7,  X64_RDX, offsets[0], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RDX, offsets[2], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM6, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM10, X64_RDX, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM10);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM12, X64_XMM3);

    x64_sse_movaps_reg_memindex(ins, X64_XMM9,  X64_RDX, offsets[1], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM10, X64_RDX, offsets[4], X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM13, X64_RDX, offsets[5], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM10);
    x64_sse_movaps_reg_memindex(ins, X64_XMM8,  X64_RDX, offsets[6], X64_RAX, 2);

    x64_sse_movaps_reg_reg_size(ins, X64_XMM5, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM14, X64_RDX, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM15, X64_XMM8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);

    x64_sse_movaps_reg_reg_size(ins, X64_XMM4, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

    x64_movsxd_reg_memindex(ins, X64_R10, X64_R9, 0, X64_RAX, 2);
    x64_sse_subps_reg_reg(ins, X64_XMM10, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM7, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM13);
    x64_sse_addps_reg_reg(ins, X64_XMM15, X64_XMM14);
    x64_sse_movaps_reg_membase(ins, X64_XMM12, X64_RSI, 16);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM9);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM12);

    x64_sse_movaps_reg_reg_size(ins, X64_XMM2, X64_XMM5, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_mulps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM15);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM8);

    x64_sse_addps_reg_reg_size(ins, X64_XMM2, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_subps_reg_reg_size(ins, X64_XMM5, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM10, 0xB1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM9, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM8, 0xB1);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM1, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_mulps_reg_reg(ins, X64_XMM10, X64_XMM0);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM4);
    x64_sse_mulps_reg_reg(ins, X64_XMM8, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM8);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM12);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM7);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM9, X64_XMM9, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM8, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM9);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM9);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM8);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM12, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM6);
    x64_movsxd_reg_memindex(ins, X64_R11, X64_R9, 8, X64_RAX, 2);
    x64_sse_movlhps_reg_reg(ins, X64_XMM8, X64_XMM4);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM2, X64_XMM4, 0xEE);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM4, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_movlhps_reg_reg(ins, X64_XMM4, X64_XMM7);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM1, X64_XMM7, 0xEE);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM7, X64_XMM5, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movlhps_reg_reg(ins, X64_XMM7, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM9, X64_XMM14);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM13, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM14, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R10, 2, X64_XMM8);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R10, 2, X64_XMM4);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R10, 2, X64_XMM7);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R10, 2, X64_XMM9);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R11, 2, X64_XMM2);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R11, 2, X64_XMM1);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R11, 2, X64_XMM5);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R11, 2, X64_XMM6);

    /* loop condition */
    x64_alu_reg_reg_size(ins, X86_CMP, X64_RBX, X64_RAX, 8);
    x64_branch_size(ins, X86_CC_NE, leaf_ee_loop, 0, 4);
#else
    x64_sse_movaps_reg_membase_size(ins, X64_XMM0, X64_R9, 32, 1);

    /* use XMM8 for sign change */
    x64_sse_movaps_reg_membase(ins, X64_XMM8, X64_R9, 0);

    /* beginning of the loop (make sure it's 16 byte aligned) */
    leaf_ee_loop = ins;
    assert(!(((uintptr_t) leaf_ee_loop) & 0xF));

    x64_sse_movaps_reg_memindex(ins, X64_XMM7,  X64_RSI, offsets[0], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RSI, offsets[2], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM6, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM10, X64_RSI, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM10);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM12, X64_XMM8);

    x64_sse_movaps_reg_memindex(ins, X64_XMM9,  X64_RSI, offsets[1], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM10, X64_RSI, offsets[4], X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM9);

    x64_sse_movaps_reg_memindex(ins, X64_XMM13, X64_RSI, offsets[5], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM10);
    x64_sse_movaps_reg_memindex(ins, X64_XMM3,  X64_RSI, offsets[6], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM5, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM14, X64_RSI, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM15, X64_XMM3);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM4, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

    x64_movsxd_reg_memindex(ins, X64_R11, X64_R8, 0, X64_RAX, 2);
    x64_sse_subps_reg_reg(ins, X64_XMM10, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM3, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM7, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM13);
    x64_sse_addps_reg_reg(ins, X64_XMM15, X64_XMM14);
    x64_sse_movaps_reg_membase(ins, X64_XMM12, X64_R9, 16);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM9);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM12);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM2, X64_XMM5, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_mulps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM15);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM3);

	x64_sse_addps_reg_reg_size(ins, X64_XMM2, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

	x64_sse_subps_reg_reg_size(ins, X64_XMM5, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM10, 0xB1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM9, X64_XMM8);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM3, X64_XMM3, 0xB1);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM1, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_mulps_reg_reg(ins, X64_XMM10, X64_XMM0);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM4);
    x64_sse_mulps_reg_reg(ins, X64_XMM3, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM3);
    x64_sse_movaps_reg_reg(ins, X64_XMM3, X64_XMM12);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM7);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM9, X64_XMM9, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM3, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM9);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM9);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM3);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM3);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM12, X64_XMM8);

    x64_sse_movaps_reg_reg(ins, X64_XMM3, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM6);
    x64_movsxd_reg_memindex(ins, X64_R12, X64_R8, 8, X64_RAX, 2);
    x64_sse_movlhps_reg_reg(ins, X64_XMM3, X64_XMM4);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM2, X64_XMM4, 0xEE);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM4, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_movlhps_reg_reg(ins, X64_XMM4, X64_XMM7);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM1, X64_XMM7, 0xEE);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM7, X64_XMM5, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movlhps_reg_reg(ins, X64_XMM7, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM9, X64_XMM14);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM13, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM14, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R11, 2, X64_XMM3);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R11, 2, X64_XMM4);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R11, 2, X64_XMM7);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R11, 2, X64_XMM9);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R12, 2, X64_XMM2);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R12, 2, X64_XMM1);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R12, 2, X64_XMM5);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R12, 2, X64_XMM6);

    /* loop condition */
    x64_alu_reg_reg_size(ins, X86_CMP, X64_RCX, X64_RAX, 8);
    x64_branch_size(ins, X86_CC_NE, leaf_ee_loop, 0, 4);
#endif

    *fp = ins;
}

static FFTS_INLINE void
generate_leaf_eo(insns_t **fp, uint32_t *offsets)
{
    /* to avoid deferring */
    insns_t *ins = *fp;

#ifdef _M_X64
    x64_sse_movaps_reg_memindex(ins, X64_XMM9, X64_RDX, offsets[0], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RDX, offsets[2], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM5, X64_RDX, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM7);
    x64_sse_movaps_reg_memindex(ins, X64_XMM4, X64_RDX, offsets[1], X64_RAX, 2);
    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM5);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM4);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM4);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM5);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM11);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM7, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM9);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM7);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_movsxd_reg_memindex(ins, X64_R11, X64_R9, 8, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM2, X64_XMM10);
    x64_movsxd_reg_memindex(ins, X64_R10, X64_R9, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM8, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM9, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R11, 2, X64_XMM10);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R11, 2, X64_XMM11);
    x64_sse_movaps_reg_memindex(ins, X64_XMM15,  X64_RDX, offsets[4], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12,  X64_RDX, offsets[5], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM15);
    x64_sse_movaps_reg_memindex(ins, X64_XMM4,  X64_RDX, offsets[6], X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM15, X64_XMM12);
    x64_sse_movaps_reg_memindex(ins, X64_XMM13,  X64_RDX, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM7, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM2, X64_XMM8);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM5);
    x64_sse_movlhps_reg_reg(ins, X64_XMM7, X64_XMM15);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM15, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM5);
    x64_sse_subps_reg_reg(ins, X64_XMM5, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM14);
    x64_sse_movlhps_reg_reg(ins, X64_XMM1, X64_XMM9);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM4);
    x64_sse_movlhps_reg_reg(ins, X64_XMM8, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM12, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM15, X64_XMM15, 0xB1);
    x64_sse_movaps_reg_membase(ins, X64_XMM11, X64_RSI, 48);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM15);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM11);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM7);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM15);
    x64_sse_movaps_reg_membase(ins, X64_XMM15, X64_RSI, 64);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_mulps_reg_reg(ins, X64_XMM7, X64_XMM15);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM8, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM7);
    x64_sse_mulps_reg_reg(ins, X64_XMM8, X64_XMM15);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM14, 0xEE);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM9);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM11, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM0, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM11, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM4, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R11, 2, X64_XMM5);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R11, 2, X64_XMM6);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R10, 2, X64_XMM2);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R10, 2, X64_XMM1);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R10, 2, X64_XMM0);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R10, 2, X64_XMM12);
#else
    x64_sse_movaps_reg_memindex(ins, X64_XMM9,  X64_RSI, offsets[0], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7,  X64_RSI, offsets[2], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM5,  X64_RSI, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM7);
    x64_sse_movaps_reg_memindex(ins, X64_XMM4,  X64_RSI, offsets[1], X64_RAX, 2);
    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM5);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM4);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM4);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM5);
    x64_sse_movaps_reg_membase(ins, X64_XMM3, X64_R9, 0);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM11);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM7, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM9);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM7);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_movsxd_reg_memindex(ins, X64_R12, X64_R8, 8, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM2, X64_XMM10);
    x64_movsxd_reg_memindex(ins, X64_R11, X64_R8, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM8, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM9, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R12, 2, X64_XMM10);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R12, 2, X64_XMM11);
    x64_sse_movaps_reg_memindex(ins, X64_XMM15,  X64_RSI, offsets[4], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12,  X64_RSI, offsets[5], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM15);
    x64_sse_movaps_reg_memindex(ins, X64_XMM4,  X64_RSI, offsets[6], X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM15, X64_XMM12);
    x64_sse_movaps_reg_memindex(ins, X64_XMM13,  X64_RSI, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM7, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM2, X64_XMM8);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM5);
    x64_sse_movlhps_reg_reg(ins, X64_XMM7, X64_XMM15);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM15, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM5);
    x64_sse_subps_reg_reg(ins, X64_XMM5, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM14);
    x64_sse_movlhps_reg_reg(ins, X64_XMM1, X64_XMM9);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM4);
    x64_sse_movlhps_reg_reg(ins, X64_XMM8, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM12, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM15, X64_XMM15, 0xB1);
    x64_sse_movaps_reg_membase(ins, X64_XMM11, X64_R9, 48);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM15);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM11);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM7);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM15);
    x64_sse_movaps_reg_membase(ins, X64_XMM15, X64_R9, 64);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_mulps_reg_reg(ins, X64_XMM7, X64_XMM15);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM8, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM7);
    x64_sse_mulps_reg_reg(ins, X64_XMM8, X64_XMM15);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM14, 0xEE);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM9);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM11, X64_XMM3);

    x64_sse_movaps_reg_reg(ins, X64_XMM3, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM11, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM3, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM4, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R12, 2, X64_XMM5);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R12, 2, X64_XMM6);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R11, 2, X64_XMM2);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R11, 2, X64_XMM1);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R11, 2, X64_XMM3);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R11, 2, X64_XMM12);
#endif

    *fp = ins;
}

static FFTS_INLINE void
generate_leaf_oe(insns_t **fp, uint32_t *offsets)
{
    /* to avoid deferring */
    insns_t *ins = *fp;

#ifdef _M_X64
    x64_sse_movaps_reg_memindex(ins, X64_XMM6, X64_RDX, offsets[2], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM8, X64_RDX, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM6);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM8, 0xE4);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM10);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM6, 0xE4);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RDX, offsets[0], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RDX, offsets[1], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_movsxd_reg_memindex(ins, X64_R10, X64_R9, 0, X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM7);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM9);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM14);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM4, X64_XMM10, 0xEE);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM10, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM10, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM12);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM13, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM10);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM9);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM12, 0xEE);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_movsxd_reg_memindex(ins, X64_R11, X64_R9, 8, X64_RAX, 2);
    x64_sse_movlhps_reg_reg(ins, X64_XMM13, X64_XMM11);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 0, X64_R10, 2, X64_XMM13);
    x64_sse_movaps_reg_membase(ins, X64_XMM13, X64_RSI, 48);
    x64_sse_movlhps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM13);
    x64_sse_movaps_reg_membase(ins, X64_XMM12, X64_RSI, 64);
    x64_sse_mulps_reg_reg(ins, X64_XMM13, X64_XMM5);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM5, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM5, X64_XMM12);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R10, 2, X64_XMM14);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM5);
    x64_sse_mulps_reg_reg(ins, X64_XMM1, X64_XMM4);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM4, X64_XMM4, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM4, X64_XMM12);
    x64_sse_movaps_reg_memindex(ins, X64_XMM9, X64_RDX, offsets[4], X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM4);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RDX, offsets[6], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM0, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM2, X64_RDX, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM7);
    x64_sse_movaps_reg_memindex(ins, X64_XMM15, X64_RDX, offsets[5], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM2);
    x64_sse_addps_reg_reg(ins, X64_XMM0, X64_XMM15);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM2);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM1);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM7, X64_XMM3);

    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_movaps_reg_reg(ins, X64_XMM2, X64_XMM0);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM9);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM13, X64_XMM3);

    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM7);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM6);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM0);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM2, X64_XMM8, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM0, X64_XMM9, 0xEE);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM13, X64_XMM13, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM4);
    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM0);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM13);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_movlhps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R10, 2, X64_XMM10);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R10, 2, X64_XMM11);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R11, 2, X64_XMM2);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R11, 2, X64_XMM0);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R11, 2, X64_XMM14);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R11, 2, X64_XMM4);
#else
    x64_sse_movaps_reg_membase(ins, X64_XMM0, X64_R9, 0);
    x64_sse_movaps_reg_memindex(ins, X64_XMM6, X64_RSI, offsets[2], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM8, X64_RSI, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM6);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM8, 0xE4);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM10);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM6, 0xE4);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RSI, offsets[0], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RSI, offsets[1], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_movsxd_reg_memindex(ins, X64_R11, X64_R8, 0, X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_addps_reg_reg(ins, X64_XMM14, X64_XMM7);
    x64_sse_subps_reg_reg(ins, X64_XMM12, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM9);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM14);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM4, X64_XMM10, 0xEE);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM10, X64_XMM0);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM10, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM12);
    x64_sse_movaps_reg_reg(ins, X64_XMM5, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM13, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM10);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM9);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM12, 0xEE);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM10);
    x64_movsxd_reg_memindex(ins, X64_R12, X64_R8, 8, X64_RAX, 2);
    x64_sse_movlhps_reg_reg(ins, X64_XMM13, X64_XMM11);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 0, X64_R11, 2, X64_XMM13);
    x64_sse_movaps_reg_membase(ins, X64_XMM13, X64_R9, 48);
    x64_sse_movlhps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM13);
    x64_sse_movaps_reg_membase(ins, X64_XMM12, X64_R9, 64);
    x64_sse_mulps_reg_reg(ins, X64_XMM13, X64_XMM5);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM5, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM5, X64_XMM12);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R11, 2, X64_XMM14);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM5);
    x64_sse_mulps_reg_reg(ins, X64_XMM1, X64_XMM4);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM4, X64_XMM4, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM4, X64_XMM12);
    x64_sse_movaps_reg_memindex(ins, X64_XMM9, X64_RSI, offsets[4], X64_RAX, 2);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM4);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RSI, offsets[6], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM3, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM2, X64_RSI, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM7);
    x64_sse_movaps_reg_memindex(ins, X64_XMM15, X64_RSI, offsets[5], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM7, X64_XMM2);
    x64_sse_addps_reg_reg(ins, X64_XMM3, X64_XMM15);
    x64_sse_subps_reg_reg(ins, X64_XMM9, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM2);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM1);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM7, X64_XMM0);

    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_movaps_reg_reg(ins, X64_XMM2, X64_XMM3);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM9);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM13, X64_XMM0);

    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM8, X64_XMM7);
    x64_sse_subps_reg_reg(ins, X64_XMM3, X64_XMM6);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM3);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM2, X64_XMM8, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM3, X64_XMM9, 0xEE);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM13, X64_XMM13, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM4);
    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM3);
    x64_sse_subps_reg_reg(ins, X64_XMM3, X64_XMM13);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_movlhps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R11, 2, X64_XMM10);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R11, 2, X64_XMM11);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R12, 2, X64_XMM2);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R12, 2, X64_XMM3);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R12, 2, X64_XMM14);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R12, 2, X64_XMM4);
#endif

    *fp = ins;
}

static FFTS_INLINE void
generate_leaf_oo(insns_t **fp, uint32_t loop_count, uint32_t *offsets, int extend)
{
    insns_t *leaf_oo_loop;

    /* to avoid deferring */	
    insns_t *ins = *fp;

#ifdef _M_X64
    /* align loop/jump destination */
    x86_mov_reg_imm(ins, X86_EBX, loop_count);

    /* beginning of the loop (make sure it's 16 byte aligned) */
    leaf_oo_loop = ins;
    assert(!(((uintptr_t) leaf_oo_loop) & 0xF));

    x64_sse_movaps_reg_memindex(ins, X64_XMM4, X64_RDX, offsets[0], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM6, X64_XMM4, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RDX, offsets[1], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM10, X64_RDX, offsets[2], X64_RAX, 2);

	x64_sse_addps_reg_reg_size(ins, X64_XMM6, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

	x64_sse_subps_reg_reg_size(ins, X64_XMM4, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM8, X64_RDX, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM10);
    x64_sse_movaps_reg_memindex(ins, X64_XMM1, X64_RDX, offsets[4], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM5, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM11, X64_RDX, offsets[5], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM2, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM14, X64_RDX, offsets[6], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM15, X64_XMM4);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RDX, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM14);
    x64_movsxd_reg_memindex(ins, X64_R10, X64_R9, 0, X64_RAX, 2);
    x64_sse_subps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM13, X64_XMM12);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM10, X64_XMM3);

    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM9);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM14, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM10, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM14, X64_XMM14, 0xB1);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM7, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_movsxd_reg_memindex(ins, X64_R11, X64_R9, 8, X64_RAX, 2);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM15, X64_XMM10);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM1);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM2);

	x64_sse_movlhps_reg_reg_size(ins, X64_XMM7, X64_XMM4, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM14);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM4, 0xEE);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM5);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM9);
    x64_sse_movlhps_reg_reg(ins, X64_XMM14, X64_XMM15);
    x64_sse_movlhps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM8, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM5, X64_XMM15, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM9, X64_XMM13, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM2, X64_XMM1, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R10, 2, X64_XMM14);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R10, 2, X64_XMM7);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R10, 2, X64_XMM4);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R10, 2, X64_XMM8);
    x64_sse_movaps_memindex_reg(ins, X64_R8,  0, X64_R11, 2, X64_XMM5);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 16, X64_R11, 2, X64_XMM6);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 32, X64_R11, 2, X64_XMM9);
    x64_sse_movaps_memindex_reg(ins, X64_R8, 48, X64_R11, 2, X64_XMM2);

    /* loop condition */
    x64_alu_reg_reg_size(ins, X86_CMP, X64_RBX, X64_RAX, 8);
    x64_branch_size(ins, X86_CC_NE, leaf_oo_loop, 0, 4);
#else
    /* align loop/jump destination */
    x86_mov_reg_imm(ins, X86_ECX, loop_count);
    ffts_align_mem16(&ins, 4);

    x64_sse_movaps_reg_membase(ins, X64_XMM5, X64_R9, 0);

    /* beginning of the loop (make sure it's 16 byte aligned) */
    leaf_oo_loop = ins;
    assert(!(((uintptr_t) leaf_oo_loop) & 0xF));

    x64_sse_movaps_reg_memindex(ins, X64_XMM4, X64_RSI, offsets[0], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM6, X64_XMM4, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RSI, offsets[1], X64_RAX, 2);
    x64_sse_movaps_reg_memindex(ins, X64_XMM10, X64_RSI, offsets[2], X64_RAX, 2);

	x64_sse_addps_reg_reg_size(ins, X64_XMM6, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

	x64_sse_subps_reg_reg_size(ins, X64_XMM4, X64_XMM7, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM8, X64_RSI, offsets[3], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM10);
    x64_sse_movaps_reg_memindex(ins, X64_XMM1, X64_RSI, offsets[4], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM3, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM11, X64_RSI, offsets[5], X64_RAX, 2);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM2, X64_XMM1, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_movaps_reg_memindex(ins, X64_XMM14, X64_RSI, offsets[6], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM15, X64_XMM4);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RSI, offsets[7], X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM14);
    x64_movsxd_reg_memindex(ins, X64_R11, X64_R8, 0, X64_RAX, 2);
    x64_sse_subps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_addps_reg_reg(ins, X64_XMM2, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM12);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM13, X64_XMM12);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM10, X64_XMM5);

    x64_sse_addps_reg_reg(ins, X64_XMM3, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM9);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM14, X64_XMM5);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM10, X64_XMM10, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM9, X64_XMM2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM14, X64_XMM14, 0xB1);

	x64_sse_movaps_reg_reg_size(ins, X64_XMM7, X64_XMM6, extend > 0 ? 8 : 0);
    extend--;

    x64_movsxd_reg_memindex(ins, X64_R12, X64_R8, 8, X64_RAX, 2);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);
    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM15, X64_XMM10);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM1);
    x64_sse_movaps_reg_reg(ins, X64_XMM8, X64_XMM2);

	x64_sse_movlhps_reg_reg_size(ins, X64_XMM7, X64_XMM4, extend > 0 ? 8 : 0);
    extend--;

    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM14);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM14);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM4, 0xEE);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM3);
    x64_sse_movaps_reg_reg(ins, X64_XMM4, X64_XMM9);
    x64_sse_movlhps_reg_reg(ins, X64_XMM14, X64_XMM15);
    x64_sse_movlhps_reg_reg(ins, X64_XMM4, X64_XMM13);
    x64_sse_movlhps_reg_reg(ins, X64_XMM8, X64_XMM1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM3, X64_XMM15, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM9, X64_XMM13, 0xEE);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM2, X64_XMM1, 0xEE);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R11, 2, X64_XMM14);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R11, 2, X64_XMM7);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R11, 2, X64_XMM4);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R11, 2, X64_XMM8);
    x64_sse_movaps_memindex_reg(ins, X64_RDX,  0, X64_R12, 2, X64_XMM3);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 16, X64_R12, 2, X64_XMM6);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 32, X64_R12, 2, X64_XMM9);
    x64_sse_movaps_memindex_reg(ins, X64_RDX, 48, X64_R12, 2, X64_XMM2);

    /* loop condition */
    x64_alu_reg_reg_size(ins, X86_CMP, X64_RCX, X64_RAX, 8);
    x64_branch_size(ins, X86_CC_NE, leaf_oo_loop, 0, 4);
#endif

    *fp = ins;
}

static FFTS_INLINE insns_t*
generate_size8_base_case(insns_t **fp, int sign)
{
    insns_t *ins;
    insns_t *x8_addr;
    insns_t *x8_soft_loop;

    /* unreferenced parameter */
    (void) sign;

    /* to avoid deferring */
    ins = *fp;

    /* align call destination */
    ffts_align_mem16(&ins, 0);
    x8_addr = ins;

    /* align loop/jump destination */
#ifdef _M_X64
    ffts_align_mem16(&ins, 6);
#else
    ffts_align_mem16(&ins, 5);
#endif

#ifdef _M_X64
    /* generate function */

    /* input */
    x64_mov_reg_reg(ins, X64_RAX, X64_R9, 8);

    /* output */
    x64_mov_reg_reg(ins, X64_RCX, X64_R8, 8);

    /* loop stop (RDX = output + output_stride) */
    x64_lea_memindex(ins, X64_RDX, X64_R8, 0, X64_RBX, 0);

    /* RSI = 3 * output_stride */
    x64_lea_memindex(ins, X64_RSI, X64_RBX, 0, X64_RBX, 1);

    /* R10 = 5 * output_stride */
    x64_lea_memindex(ins, X64_R10, X64_RBX, 0, X64_RBX, 2);

    /* R11 = 7 * output_stride */
    x64_lea_memindex(ins, X64_R11, X64_RSI, 0, X64_RBX, 2);

    /* beginning of the loop (make sure it's 16 byte aligned) */
    x8_soft_loop = ins;
    assert(!(((uintptr_t) x8_soft_loop) & 0xF));

    /* load [input + 0 * input_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM9, X64_RAX, 0);

    /* load [output + 2 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM6, X64_RCX, 0, X64_RBX, 1);

    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM9);

    /* load [output + 3 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RCX, 0, X64_RSI, 0);

    /* load [input + 1 * input_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM8, X64_RAX, 16);

    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM6);
    x64_sse_mulps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM6, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM6, X64_XMM8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM6);
    x64_sse_mulps_reg_reg(ins, X64_XMM8, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);

    /* load [input + 2 * input_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM15, X64_RAX, 32);

    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM9);

    /* load [output + 0 * output_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM5, X64_RCX, 0);

    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM15);

    /* load [output + 4 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_RCX, 0, X64_RBX, 2);

    x64_sse_movaps_reg_reg(ins, X64_XMM2, X64_XMM5);

    /* load [output + 6 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM13, X64_RCX, 0, X64_RSI, 1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM11, X64_XMM3);

    /* load [input + 3 * input_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM14, X64_RAX, 48);

    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM10);
    x64_sse_mulps_reg_reg(ins, X64_XMM6, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM10);
    x64_sse_mulps_reg_reg(ins, X64_XMM15, X64_XMM13);

    /* load [input + 4 * input_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM10, X64_RAX, 64);

    x64_sse_movaps_reg_reg(ins, X64_XMM0, X64_XMM5);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM13, X64_XMM13, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM12, X64_XMM14);
    x64_sse_mulps_reg_reg(ins, X64_XMM14, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM15, X64_XMM14);

    /* load [output + 5 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_RCX, 0, X64_R10, 0);

    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM10);

    /* load [output + 7 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM8, X64_RCX, 0, X64_R11, 0);

    x64_sse_movaps_reg_reg(ins, X64_XMM12, X64_XMM6);

    /* load [input + 5 * input_stride] */
    x64_sse_movaps_reg_membase(ins, X64_XMM9, X64_RAX, 80);

    /* move input by 6 * input_stride */
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 0x60, 8);

    x64_sse_mulps_reg_reg(ins, X64_XMM13, X64_XMM7);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM15);
    x64_sse_mulps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM12);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM6, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM8, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM12, X64_XMM2);
    x64_sse_mulps_reg_reg(ins, X64_XMM7, X64_XMM9);
    x64_sse_mulps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM7);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM9);

    /* load [output + 1 * output_stride] */
    x64_sse_movaps_reg_memindex(ins, X64_XMM4, X64_RCX, 0, X64_RBX, 0);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM11, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM4);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM6, 0xB1);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM6);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM13);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM1);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM10);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM13, X64_XMM3);

    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM11);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM13, X64_XMM13, 0xB1);

    /* store [output + 0 * output_stride] */
    x64_sse_movaps_membase_reg(ins, X64_RCX, 0, X64_XMM5);

    /* store [output + 1 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_RBX, 0, X64_XMM4);

    /* store [output + 2 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_RBX, 1, X64_XMM2);

    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM13);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM13);

    /* store [output + 3 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_RSI, 0, X64_XMM1);

    /* store [output + 4 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_RBX, 2, X64_XMM0);

    /* store [output + 5 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_R10, 0, X64_XMM14);

    /* store [output + 6 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_RSI, 1, X64_XMM12);

    /* store [output + 7 * output_stride] */
    x64_sse_movaps_memindex_reg(ins, X64_RCX, 0, X64_R11, 0, X64_XMM6);

    /* move output by 16 */
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RCX, 16, 8);

    /* loop condition */
    x64_alu_reg_reg_size(ins, X86_CMP, X64_RCX, X64_RDX, 8);
    x64_branch_size(ins, X86_CC_NE, x8_soft_loop, 0, 4);

    x64_ret(ins);
#else
    /* generate function */
    x86_clear_reg(ins, X86_EAX);
    x64_mov_reg_reg(ins, X64_RBX, X64_RDX, 8);
    x64_mov_reg_reg(ins, X64_RSI, X64_R8, 8);

    x64_lea_memindex(ins, X64_R9,  X64_RDX, 0, X64_RCX, 2);
    x64_lea_memindex(ins, X64_R10, X64_R9,  0, X64_RCX, 2);
    x64_lea_memindex(ins, X64_R11, X64_R10, 0, X64_RCX, 2);
    x64_lea_memindex(ins, X64_R12, X64_R11, 0, X64_RCX, 2);
    x64_lea_memindex(ins, X64_R13, X64_R12, 0, X64_RCX, 2);
    x64_lea_memindex(ins, X64_R14, X64_R13, 0, X64_RCX, 2);
    x64_lea_memindex(ins, X64_R15, X64_R14, 0, X64_RCX, 2);

    /* beginning of the loop (make sure it's 16 byte aligned) */
    x8_soft_loop = ins;
    assert(!(((uintptr_t) x8_soft_loop) & 0xF));

    x64_sse_movaps_reg_membase(ins, X64_XMM9, X64_RSI, 0);
    x64_sse_movaps_reg_memindex(ins, X64_XMM6, X64_R10, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_R11, 0, X64_RAX, 2);
    x64_sse_movaps_reg_membase(ins, X64_XMM8, X64_RSI, 16);
    x64_sse_mulps_reg_reg(ins, X64_XMM11, X64_XMM6);
    x64_sse_mulps_reg_reg(ins, X64_XMM9, X64_XMM7);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM6, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM6, X64_XMM8);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM6);
    x64_sse_mulps_reg_reg(ins, X64_XMM8, X64_XMM7);
    x64_sse_movaps_reg_reg(ins, X64_XMM10, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_movaps_reg_membase(ins, X64_XMM15, X64_RSI, 32);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM9);
    x64_sse_subps_reg_reg(ins, X64_XMM11, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM5, X64_RBX, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM15);
    x64_sse_movaps_reg_memindex(ins, X64_XMM12, X64_R12, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM2, X64_XMM5);
    x64_sse_movaps_reg_memindex(ins, X64_XMM13, X64_R14, 0, X64_RAX, 2);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM11, X64_XMM3);

    x64_sse_movaps_reg_membase(ins, X64_XMM14, X64_RSI, 48);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM10);
    x64_sse_mulps_reg_reg(ins, X64_XMM6, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM10);
    x64_sse_mulps_reg_reg(ins, X64_XMM15, X64_XMM13);
    x64_sse_movaps_reg_membase(ins, X64_XMM10, X64_RSI, 64);
    x64_sse_movaps_reg_reg(ins, X64_XMM0, X64_XMM5);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM12, X64_XMM12, 0xB1);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM13, X64_XMM13, 0xB1);
    x64_sse_mulps_reg_reg(ins, X64_XMM12, X64_XMM14);
    x64_sse_mulps_reg_reg(ins, X64_XMM14, X64_XMM13);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM15, X64_XMM14);
    x64_sse_movaps_reg_memindex(ins, X64_XMM7, X64_R13, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM13, X64_XMM10);
    x64_sse_movaps_reg_memindex(ins, X64_XMM8, X64_R15, 0, X64_RAX, 2);
    x64_sse_movaps_reg_reg(ins, X64_XMM12, X64_XMM6);
    x64_sse_movaps_reg_membase(ins, X64_XMM9, X64_RSI, 80);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RSI, 0x60, 8);
    x64_sse_mulps_reg_reg(ins, X64_XMM13, X64_XMM7);
    x64_sse_subps_reg_reg(ins, X64_XMM6, X64_XMM15);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM15);
    x64_sse_mulps_reg_reg(ins, X64_XMM10, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM0, X64_XMM12);
    x64_sse_addps_reg_reg(ins, X64_XMM5, X64_XMM12);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM7, X64_XMM7, 0xB1);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM6, X64_XMM3);

    x64_sse_shufps_reg_reg_imm(ins, X64_XMM8, X64_XMM8, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM12, X64_XMM2);
    x64_sse_mulps_reg_reg(ins, X64_XMM7, X64_XMM9);
    x64_sse_mulps_reg_reg(ins, X64_XMM9, X64_XMM8);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM7);
    x64_sse_addps_reg_reg(ins, X64_XMM10, X64_XMM9);
    x64_sse_movaps_reg_memindex(ins, X64_XMM4, X64_R9, 0, X64_RAX, 2);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM11, X64_XMM11, 0xB1);
    x64_sse_movaps_reg_reg(ins, X64_XMM1, X64_XMM4);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM6, X64_XMM6, 0xB1);
    x64_sse_addps_reg_reg(ins, X64_XMM1, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM4, X64_XMM11);
    x64_sse_addps_reg_reg(ins, X64_XMM12, X64_XMM6);
    x64_sse_subps_reg_reg(ins, X64_XMM2, X64_XMM6);
    x64_sse_movaps_reg_reg(ins, X64_XMM11, X64_XMM13);
    x64_sse_movaps_reg_reg(ins, X64_XMM14, X64_XMM4);
    x64_sse_movaps_reg_reg(ins, X64_XMM6, X64_XMM1);
    x64_sse_subps_reg_reg(ins, X64_XMM13, X64_XMM10);
    x64_sse_addps_reg_reg(ins, X64_XMM11, X64_XMM10);

    /* change sign */
    x64_sse_xorps_reg_reg(ins, X64_XMM13, X64_XMM3);

    x64_sse_addps_reg_reg(ins, X64_XMM4, X64_XMM11);
    x64_sse_subps_reg_reg(ins, X64_XMM14, X64_XMM11);
    x64_sse_shufps_reg_reg_imm(ins, X64_XMM13, X64_XMM13, 0xB1);
    x64_sse_movaps_memindex_reg(ins, X64_RBX, 0, X64_RAX, 2, X64_XMM5);
    x64_sse_movaps_memindex_reg(ins, X64_R9, 0, X64_RAX, 2, X64_XMM4);
    x64_sse_movaps_memindex_reg(ins, X64_R10, 0, X64_RAX, 2, X64_XMM2);
    x64_sse_subps_reg_reg(ins, X64_XMM1, X64_XMM13);
    x64_sse_addps_reg_reg(ins, X64_XMM6, X64_XMM13);
    x64_sse_movaps_memindex_reg(ins, X64_R11, 0, X64_RAX, 2, X64_XMM1);
    x64_sse_movaps_memindex_reg(ins, X64_R12, 0, X64_RAX, 2, X64_XMM0);
    x64_sse_movaps_memindex_reg(ins, X64_R13, 0, X64_RAX, 2, X64_XMM14);
    x64_sse_movaps_memindex_reg(ins, X64_R14, 0, X64_RAX, 2, X64_XMM12);
    x64_sse_movaps_memindex_reg(ins, X64_R15, 0, X64_RAX, 2, X64_XMM6);
    x64_alu_reg_imm_size(ins, X86_ADD, X64_RAX, 4, 8);

    /* loop condition */
    x64_alu_reg_reg_size(ins, X86_CMP, X64_RCX, X64_RAX, 8);
    x64_branch_size(ins, X86_CC_NE, x8_soft_loop, 0, 4);
    x64_ret(ins);
#endif

    *fp = ins;
    return x8_addr;
}

#endif /* FFTS_CODEGEN_SSE_H */