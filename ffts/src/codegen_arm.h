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

#ifndef FFTS_CODEGEN_ARM_H
#define FFTS_CODEGEN_ARM_H

#include "neon.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

uint32_t BL(void *pos, void *target) {
	return 0xeb000000 | (((target - pos) / 4) & 0xffffff);
}

uint32_t B(uint8_t r) {
	return 0xe12fff10 | r;
}

uint32_t MOV(uint8_t dst, uint8_t src) {
		return 0xe1a00000 | (src & 0xf) | ((dst & 0xf) << 12);
}

void ADDI(uint32_t **p, uint8_t dst, uint8_t src, int32_t imm) {
	int32_t oimm = imm;
	if(imm < 0) {
		imm = -imm;
		uint32_t shamt = (__builtin_ctzl(imm)>23)?23:__builtin_ctzl(imm);
		if(shamt & 1) shamt -= 1;
		imm >>= shamt;
		shamt = (32 - shamt)/2;
		
	//	if(imm > 255) fprintf(stderr, "imm>255: %d\n", oimm);
		*(*p)++ = 0xe2400000 | ((src & 0xf) << 16) | ((dst & 0xf) << 12) | ((shamt & 0xf) << 8) | (imm & 0xff);
	
		if(imm > 255) ADDI(p, dst, src, (oimm + ((imm & 0xff) << (32-shamt*2))));

	}else{
		uint32_t shamt = (__builtin_ctzl(imm)>23)?23:__builtin_ctzl(imm);
		if(shamt & 1) shamt -= 1;
		imm >>= shamt;
		shamt = (32 - shamt)/2;

//		if(imm > 255) fprintf(stderr, "imm>255: %d\n", oimm);

		*(*p)++ = 0xe2800000 | ((src & 0xf) << 16) | ((dst & 0xf) << 12) | ((shamt & 0xf) << 8) | (imm & 0xff);
		
		if(imm > 255) ADDI(p, dst, src, (oimm - ((imm & 0xff) << (32-shamt*2))));
	}
}

uint32_t LDRI(uint8_t dst, uint8_t base, uint32_t offset) {
	return 0xe5900000 | ((dst & 0xf) << 12)
	                  | ((base & 0xf) << 16) | (offset & 0xfff) ;
}

void MOVI(uint32_t **p, uint8_t dst, uint32_t imm) {
	uint32_t oimm = imm;
	
		uint32_t shamt = (__builtin_ctzl(imm)>23)?23:__builtin_ctzl(imm);
		if(shamt & 1) shamt -= 1;
		imm >>= shamt;
		shamt = (32 - shamt)/2;
	*(*p)++ = 0xe3a00000 | ((dst & 0xf) << 12) | ((shamt & 0xf) << 8) | (imm & 0xff) ;
		if(imm > 255) ADDI(p, dst, dst, (oimm - ((imm & 0xff) << (32-shamt*2))));
}

uint32_t PUSH_LR() { return 0xe92d4ff0; } //0xe92d4000; }
uint32_t POP_LR() { return 0xe8bd8ff0; } //0xe8bd8000; }

static FFTS_INLINE insns_t* generate_size4_base_case(insns_t **fp, int sign)
{
	insns_t *x_4_addr;
	size_t len;

	x_4_addr = *fp;

#ifdef HAVE_NEON
	len = (char*) neon_x8 - (char*) neon_x4;
	memcpy(x_4_addr, neon_x4, len);

	if (sign < 0) {
		x_4_addr[26] ^= 0x00200000;
		x_4_addr[28] ^= 0x00200000;
		x_4_addr[31] ^= 0x00200000;
		x_4_addr[32] ^= 0x00200000;
	}
#else
	len = (char*) vfp_x8 - (char*) vfp_x4;
	memcpy(x_4_addr, vfp_x4, len);

	if (sign > 0) {
		x_4_addr[36] ^= 0x00000040;
		x_4_addr[38] ^= 0x00000040;
		x_4_addr[43] ^= 0x00000040;
		x_4_addr[44] ^= 0x00000040;
	}
#endif

	*fp += len / 4;
	return x_4_addr;
}

static FFTS_INLINE insns_t* generate_size8_base_case(insns_t **fp, int sign)
{
	insns_t *x_8_addr;
	ptrdiff_t len;

	x_8_addr = *fp;

#ifdef HAVE_NEON
	len = (char*) neon_x8_t - (char*) neon_x8;
	memcpy(x_8_addr, neon_x8, len);

	/*
	* Changes adds to subtracts and vice versa to allow the computation
	* of both the IFFT and FFT
	*/
	if (sign < 0) {
		x_8_addr[31] ^= 0x00200000;
		x_8_addr[32] ^= 0x00200000;
		x_8_addr[33] ^= 0x00200000;
		x_8_addr[34] ^= 0x00200000;
		x_8_addr[65] ^= 0x00200000;
		x_8_addr[66] ^= 0x00200000;
		x_8_addr[70] ^= 0x00200000;
		x_8_addr[74] ^= 0x00200000;
		x_8_addr[97] ^= 0x00200000;
		x_8_addr[98] ^= 0x00200000;
		x_8_addr[102] ^= 0x00200000;
		x_8_addr[104] ^= 0x00200000;
	}

	*fp += len / 4;
	
	//uint32_t *x_8_t_addr = fp;
    //memcpy(fp, neon_x8_t, neon_end - neon_x8_t);
    //fp += (neon_end - neon_x8_t) / 4;
#else
	len = (char*) vfp_end - (char*) vfp_x8;
	memcpy(x_8_addr, vfp_x8, len);

	if (sign > 0) {
		x_8_addr[65] ^= 0x00000040;
		x_8_addr[66] ^= 0x00000040;
		x_8_addr[68] ^= 0x00000040;
		x_8_addr[70] ^= 0x00000040;
		x_8_addr[103] ^= 0x00000040;
		x_8_addr[104] ^= 0x00000040;
		x_8_addr[105] ^= 0x00000040;
		x_8_addr[108] ^= 0x00000040;
		x_8_addr[113] ^= 0x00000040;
		x_8_addr[114] ^= 0x00000040;
		x_8_addr[117] ^= 0x00000040;
		x_8_addr[118] ^= 0x00000040;
	}

	*fp += len / 4;
#endif

	return x_8_addr;
}

static FFTS_INLINE insns_t* generate_prologue(insns_t **fp, ffts_plan_t *p)
{
	insns_t	*start = *fp;

	*(*fp)++ = PUSH_LR();
	*(*fp)++ = 0xed2d8b10;

	ADDI(fp,  3,  1,        0);
	ADDI(fp,  7,  1,     p->N);
	ADDI(fp,  5,  1, 2 * p->N);
	ADDI(fp, 10,  7, 2 * p->N);
	ADDI(fp,  4,  5, 2 * p->N);
	ADDI(fp,  8, 10, 2 * p->N);
	ADDI(fp,  6,  4, 2 * p->N);
	ADDI(fp,  9,  8, 2 * p->N);

	// load offsets into r12
	*(*fp)++ = LDRI(12, 0, ((uint32_t) &p->offsets) - ((uint32_t) p));
	//  *(*fp)++ = LDRI(1, 0, 4); // load ws into r1
	ADDI(fp, 1, 0, 0);

	ADDI(fp, 0, 2, 0), // mov out into r0
	*(*fp)++ = LDRI(2, 1, ((uint32_t) &p->ee_ws) - ((uint32_t) p));

#ifdef HAVE_NEON
	MOVI(fp, 11, p->i0);
#else
	MOVI(fp, 11, p->i0);
#endif

	return start;
}

#endif /* FFTS_CODEGEN_ARM_H */