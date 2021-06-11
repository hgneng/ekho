/*
 * x64-codegen.h: Macros for generating x86-64 code
 *
 * Authors:
 *   Paolo Molaro (lupus@ximian.com)
 *   Intel Corporation (ORP Project)
 *   Sergey Chaban (serge@wildwestsoftware.com)
 *   Dietmar Maurer (dietmar@ximian.com)
 *   Patrik Torstensson
 *   Zalman Stern
 * 
 * Copyright (C)  2000 Intel Corporation.  All rights reserved.
 * Copyright (C)  2001, 2002 Ximian, Inc.
 */

#ifndef X64_H
#define X64_H

#include "../x86/x86-codegen.h"

#include <stdint.h>

/* x86-64 general purpose registers */
typedef enum {
	X64_RAX = 0,
	X64_RCX = 1,
	X64_RDX = 2,
	X64_RBX = 3,
	X64_RSP = 4,
	X64_RBP = 5,
	X64_RSI = 6,
	X64_RDI = 7,
	X64_R8 = 8,
	X64_R9 = 9,
	X64_R10 = 10,
	X64_R11 = 11,
	X64_R12 = 12,
	X64_R13 = 13,
	X64_R14 = 14,
	X64_R15 = 15,
	X64_RIP = 16,
	X64_NREG
} X64_Reg_No;

/* x86-64 XMM registers */
typedef enum {
	X64_XMM0 = 0,
	X64_XMM1 = 1,
	X64_XMM2 = 2,
	X64_XMM3 = 3,
	X64_XMM4 = 4,
	X64_XMM5 = 5,
	X64_XMM6 = 6,
	X64_XMM7 = 7,
	X64_XMM8 = 8,
	X64_XMM9 = 9,
	X64_XMM10 = 10,
	X64_XMM11 = 11,
	X64_XMM12 = 12,
	X64_XMM13 = 13,
	X64_XMM14 = 14,
	X64_XMM15 = 15,
	X64_XMM_NREG = 16,
} X64_XMM_Reg_No;

typedef enum
{
  X64_REX_B = 1, /* The register in r/m field, base register in SIB byte, or reg in opcode is 8-15 rather than 0-7 */
  X64_REX_X = 2, /* The index register in SIB byte is 8-15 rather than 0-7 */
  X64_REX_R = 4, /* The reg field of ModRM byte is 8-15 rather than 0-7 */
  X64_REX_W = 8  /* Opeartion is 64-bits instead of 32 (default) or 16 (with 0x66 prefix) */
} X64_REX_Bits;

#if defined(__native_client_codegen__)

#define x64_codegen_pre(inst) uint8_t* _codegen_start = (inst); x64_nacl_instruction_pre();
#define x64_codegen_post(inst) (x64_nacl_instruction_post(&_codegen_start, &(inst)), _codegen_start);

/* Because of rex prefixes, etc, call sequences are not constant size.  */
/* These pre- and post-sequence hooks remedy this by aligning the call  */
/* sequence after we emit it, since we will know the exact size then.   */
#define x64_call_sequence_pre(inst) uint8_t* _code_start = (inst);
#define x64_call_sequence_post(inst) \
  (mono_nacl_align_call(&_code_start, &(inst)), _code_start);

/* Native client can load/store using one of the following registers     */
/* as a base: rip, r15, rbp, rsp.  Any other base register needs to have */
/* its upper 32 bits cleared and reference memory using r15 as the base. */
#define x64_is_valid_nacl_base(reg) \
  ((reg) == X64_RIP || (reg) == X64_R15 || \
   (reg) == X64_RBP || (reg) == X64_RSP)
#else

#define x64_codegen_pre(inst)
#define x64_codegen_post(inst)

#endif /* __native_client_codegen__ */

#ifdef TARGET_WIN32
#define X64_ARG_REG1 X64_RCX
#define X64_ARG_REG2 X64_RDX
#define X64_ARG_REG3 X64_R8
#define X64_ARG_REG4 X64_R9
#else
#define X64_ARG_REG1 X64_RDI
#define X64_ARG_REG2 X64_RSI
#define X64_ARG_REG3 X64_RDX
#define X64_ARG_REG4 X64_RCX
#endif

#ifdef TARGET_WIN32
#define X64_CALLEE_REGS ((1 << X64_RAX) | (1 << X64_RCX) | (1 << X64_RDX) | (1 << X64_R8) | (1 << X64_R9) | (1 << X64_R10))
#define X64_IS_CALLEE_REG(reg)  (X64_CALLEE_REGS & (1 << (reg)))

#define X64_ARGUMENT_REGS ((1 << X64_RDX) | (1 << X64_RCX) | (1 << X64_R8) | (1 << X64_R9))
#define X64_IS_ARGUMENT_REG(reg) (X64_ARGUMENT_REGS & (1 << (reg)))

#define X64_CALLEE_SAVED_REGS ((1 << X64_RDI) | (1 << X64_RSI) | (1 << X64_RBX) | (1 << X64_R12) | (1 << X64_R13) | (1 << X64_R14) | (1 << X64_R15) | (1 << X64_RBP))
#define X64_IS_CALLEE_SAVED_REG(reg) (X64_CALLEE_SAVED_REGS & (1 << (reg)))
#elif defined(__native_client_codegen__)
/* x64 Native Client code may not write R15 */
#define X64_CALLEE_REGS ((1 << X64_RAX) | (1 << X64_RCX) | (1 << X64_RDX) | (1 << X64_RSI) | (1 << X64_RDI) | (1 << X64_R8) | (1 << X64_R9) | (1 << X64_R10))
#define X64_IS_CALLEE_REG(reg)  (X64_CALLEE_REGS & (1 << (reg)))

#define X64_ARGUMENT_REGS ((1 << X64_RDI) | (1 << X64_RSI) | (1 << X64_RDX) | (1 << X64_RCX) | (1 << X64_R8) | (1 << X64_R9))
#define X64_IS_ARGUMENT_REG(reg) (X64_ARGUMENT_REGS & (1 << (reg)))

#define X64_CALLEE_SAVED_REGS ((1 << X64_RBX) | (1 << X64_R12) | (1 << X64_R13) | (1 << X64_R14) | (1 << X64_RBP))
#define X64_IS_CALLEE_SAVED_REG(reg) (X64_CALLEE_SAVED_REGS & (1 << (reg)))
#else
#define X64_CALLEE_REGS ((1 << X64_RAX) | (1 << X64_RCX) | (1 << X64_RDX) | (1 << X64_RSI) | (1 << X64_RDI) | (1 << X64_R8) | (1 << X64_R9) | (1 << X64_R10))
#define X64_IS_CALLEE_REG(reg)  (X64_CALLEE_REGS & (1 << (reg)))

#define X64_ARGUMENT_REGS ((1 << X64_RDI) | (1 << X64_RSI) | (1 << X64_RDX) | (1 << X64_RCX) | (1 << X64_R8) | (1 << X64_R9))
#define X64_IS_ARGUMENT_REG(reg) (X64_ARGUMENT_REGS & (1 << (reg)))

#define X64_CALLEE_SAVED_REGS ((1 << X64_RBX) | (1 << X64_R12) | (1 << X64_R13) | (1 << X64_R14) | (1 << X64_R15) | (1 << X64_RBP))
#define X64_IS_CALLEE_SAVED_REG(reg) (X64_CALLEE_SAVED_REGS & (1 << (reg)))
#endif

#define X64_REX(bits) ((unsigned char)(0x40 | (bits)))

#if defined(__native_client_codegen__)
#define x64_emit_rex(inst, width, reg_modrm, reg_index, reg_rm_base_opcode) do \
	{ \
		unsigned char _x64_rex_bits = \
			(((width) > 4) ? X64_REX_W : 0) | \
			(((reg_modrm) > 7) ? X64_REX_R : 0) | \
			(((reg_index) > 7) ? X64_REX_X : 0) | \
			(((reg_rm_base_opcode) > 7) ? X64_REX_B : 0); \
		x64_nacl_tag_rex((inst)); \
		if ((_x64_rex_bits != 0) || (((width) == 1))) *(inst)++ = X64_REX(_x64_rex_bits); \
	} while (0)
#else
#define x64_emit_rex(inst, width, reg_modrm, reg_index, reg_rm_base_opcode) do \
	{ \
		unsigned char _x64_rex_bits = \
			(((width) > 4) ? X64_REX_W : 0) | \
			(((reg_modrm) > 7) ? X64_REX_R : 0) | \
			(((reg_index) > 7) ? X64_REX_X : 0) | \
			(((reg_rm_base_opcode) > 7) ? X64_REX_B : 0); \
		if ((_x64_rex_bits != 0) || (((width) == 1))) *(inst)++ = X64_REX(_x64_rex_bits); \
	} while (0)
#endif /* __native_client_codegen__ */

typedef union {
	uint64_t val;
	unsigned char b[8];
} x64_imm_buf;

/* In 64 bit mode, all registers have a low byte subregister */
#undef X86_IS_BYTE_REG
#define X86_IS_BYTE_REG(reg) 1

#define x64_modrm_mod(modrm) ((modrm) >> 6)
#define x64_modrm_reg(modrm) (((modrm) >> 3) & 0x7)
#define x64_modrm_rm(modrm) ((modrm) & 0x7)

#define x64_rex_r(rex) ((((rex) >> 2) & 0x1) << 3)
#define x64_rex_x(rex) ((((rex) >> 1) & 0x1) << 3)
#define x64_rex_b(rex) ((((rex) >> 0) & 0x1) << 3)

#define x64_sib_scale(sib) ((sib) >> 6)
#define x64_sib_index(sib) (((sib) >> 3) & 0x7)
#define x64_sib_base(sib) ((sib) & 0x7)

#define x64_is_imm32(val) ((int64_t)val >= -((int64_t)1<<31) && (int64_t)val <= (((int64_t)1<<31)-1))

#define x86_imm_emit64(inst,imm)     \
	do {	\
			x64_imm_buf imb; 	\
			imb.val = (uint64_t) (imm);	\
			*(inst)++ = imb.b [0];	\
			*(inst)++ = imb.b [1];	\
			*(inst)++ = imb.b [2];	\
			*(inst)++ = imb.b [3];	\
			*(inst)++ = imb.b [4];	\
			*(inst)++ = imb.b [5];	\
			*(inst)++ = imb.b [6];	\
			*(inst)++ = imb.b [7];	\
	} while (0)

#define x64_membase_emit(inst,reg,basereg,disp) do { \
	if ((basereg) == X64_RIP) { \
        x86_address_byte ((inst), 0, (reg)&0x7, 5); \
        x86_imm_emit32 ((inst), (disp)); \
    } \
	else \
		x86_membase_emit ((inst),(reg)&0x7, (basereg)&0x7, (disp)); \
} while (0)

#define x64_memindex_emit(inst, reg, basereg, disp, indexreg, shift) \
	x86_memindex_emit((inst), ((reg) & 0x7), ((basereg) & 0x7), (disp), ((indexreg) & 0x7), (shift))

#define x64_alu_reg_imm_size_body(inst,opc,reg,imm,size) \
	do {	\
		if (x86_is_imm8((imm))) {	\
			x64_emit_rex(inst, size, 0, 0, (reg)); \
			*(inst)++ = (unsigned char)0x83;	\
			x86_reg_emit ((inst), (opc), (reg));	\
			x86_imm_emit8 ((inst), (imm));	\
		} else if ((reg) == X64_RAX) {	\
			x64_emit_rex(inst, size, 0, 0, 0); \
			*(inst)++ = (((unsigned char)(opc)) << 3) + 5;	\
			x86_imm_emit32 ((inst), (imm));	\
		} else {	\
			x64_emit_rex(inst, size, 0, 0, (reg)); \
			*(inst)++ = (unsigned char)0x81;	\
			x86_reg_emit ((inst), (opc), (reg));	\
			x86_imm_emit32 ((inst), (imm));	\
		}	\
	} while (0)

#define x64_alu_reg_reg_size_body(inst,opc,dreg,reg,size)	\
	do {	\
		x64_emit_rex(inst, size, (dreg), 0, (reg)); \
		*(inst)++ = (((unsigned char)(opc)) << 3) + 3;	\
		x86_reg_emit ((inst), (dreg), (reg));	\
	} while (0)

#if defined(__native_client_codegen__)
/* NaCl modules may not directly update RSP or RBP other than direct copies */
/* between them. Instead the lower 4 bytes are updated and then added to R15 */
#define x64_is_nacl_stack_reg(reg) (((reg) == X64_RSP) || ((reg) == X64_RBP))

#define x64_alu_reg_imm_size(inst,opc,reg,imm,size) 	\
	do{ \
		x64_codegen_pre(inst);		\
		if (x64_is_nacl_stack_reg(reg)) { \
			if (((opc) != X86_ADD) && ((opc) != X86_SUB)) \
				g_assert_not_reached(); \
			x64_alu_reg_imm_size_body((inst), (opc), (reg), (imm), 4); \
			/* Use LEA instead of ADD to preserve flags */ \
			x64_lea_memindex_size((inst), (reg), (reg), 0, X64_R15, 0, 8); \
		} else { \
			x64_alu_reg_imm_size_body((inst), (opc), (reg), (imm), (size)); \
		} \
		x64_codegen_post(inst);		\
	} while(0)

#define x64_alu_reg_reg_size(inst,opc,dreg,reg,size) \
	do { \
		x64_codegen_pre(inst);		\
		if (x64_is_nacl_stack_reg((dreg)) && ((reg) != X64_R15)) { \
			if (((opc) != X86_ADD && (opc) != X86_SUB)) \
				g_assert_not_reached(); \
			x64_alu_reg_reg_size_body((inst), (opc), (dreg), (reg), 4); \
			/* Use LEA instead of ADD to preserve flags */ \
			x64_lea_memindex_size((inst), (dreg), (dreg), 0, X64_R15, 0, 8); \
		} else { \
			x64_alu_reg_reg_size_body((inst), (opc), (dreg), (reg), (size)); \
		} \
		x64_codegen_post(inst);		\
	} while (0)

#else

#define x64_alu_reg_imm_size(inst,opc,reg,imm,size) \
	x64_alu_reg_imm_size_body((inst), (opc), (reg), (imm), (size))

#define x64_alu_reg_reg_size(inst,opc,dreg,reg,size) \
		x64_alu_reg_reg_size_body((inst), (opc), (dreg), (reg), (size))

#endif /*__native_client_codegen__*/

#define x64_alu_reg_imm(inst,opc,reg,imm) x64_alu_reg_imm_size((inst),(opc),(reg),(imm),8)

#define x64_alu_reg_reg(inst,opc,dreg,reg) x64_alu_reg_reg_size ((inst),(opc),(dreg),(reg),8)

#define x64_alu_reg_membase_size(inst,opc,reg,basereg,disp,size) \
	do { \
		x64_codegen_pre(inst);						  \
		x64_emit_rex ((inst),(size),(reg),0,(basereg)); \
		*(inst)++ = (((unsigned char)(opc)) << 3) + 3;	\
		x64_membase_emit (inst, reg, basereg, disp); \
		x64_codegen_post(inst);					   \
} while (0)

#define x64_mov_regp_reg(inst,regp,reg,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size), (reg), 0, (regp)); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x88; break;	\
		case 2: case 4: case 8: *(inst)++ = (unsigned char)0x89; break;	\
		default: assert (0);	\
		}	\
		x86_regp_emit ((inst), (reg), (regp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_mov_membase_reg(inst,basereg,disp,reg,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size), (reg), 0, (basereg)); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x88; break;	\
		case 2: case 4: case 8: *(inst)++ = (unsigned char)0x89; break;	\
		default: assert (0);	\
		}	\
		x86_membase_emit ((inst), ((reg)&0x7), ((basereg)&0x7), (disp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_mov_mem_reg(inst,mem,reg,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size), (reg), 0, 0); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x88; break;	\
		case 2: case 4: case 8: *(inst)++ = (unsigned char)0x89; break;	\
		default: assert (0);	\
		}	\
		x86_address_byte ((inst), 0, (reg), 4); \
		x86_address_byte ((inst), 0, 4, 5); \
		x86_imm_emit32 ((inst), (mem)); \
		x64_codegen_post(inst); \
	} while (0)

#define x64_mov_reg_reg(inst,dreg,reg,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size), (dreg), 0, (reg)); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x8a; break;	\
		case 2: case 4: case 8: *(inst)++ = (unsigned char)0x8b; break;	\
		default: assert (0);	\
		}	\
		x86_reg_emit ((inst), (dreg), (reg));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_mov_reg_mem_body(inst,reg,mem,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size), (reg), 0, 0); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x8a; break;	\
		case 2: case 4: case 8: *(inst)++ = (unsigned char)0x8b; break;	\
		default: assert (0);	\
		}	\
		x86_address_byte ((inst), 0, (reg), 4); \
		x86_address_byte ((inst), 0, 4, 5); \
		x86_imm_emit32 ((inst), (mem)); \
		x64_codegen_post(inst); \
	} while (0)

#if defined(__native_client_codegen__)
/* We have to re-base memory reads because memory isn't zero based. */
#define x64_mov_reg_mem(inst,reg,mem,size)	\
	do {    \
		x64_mov_reg_membase((inst),(reg),X64_R15,(mem),(size)); \
	} while (0)
#else
#define x64_mov_reg_mem(inst,reg,mem,size)	\
	do {    \
		x64_mov_reg_mem_body((inst),(reg),(mem),(size)); \
	} while (0)
#endif /* __native_client_codegen__ */

#define x64_mov_reg_membase_body(inst,reg,basereg,disp,size)	\
	do {	\
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size), (reg), 0, (basereg)); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x8a; break;	\
		case 2: case 4: case 8: *(inst)++ = (unsigned char)0x8b; break;	\
		default: assert (0);	\
		}	\
		x64_membase_emit ((inst), (reg), (basereg), (disp));	\
	} while (0)

#define x64_mov_reg_memindex_size_body(inst,reg,basereg,disp,indexreg,shift,size) \
	do { \
		x64_emit_rex ((inst),(size),(reg),(indexreg),(basereg)); \
		x86_mov_reg_memindex((inst),((reg)&0x7),((basereg)&0x7),(disp),((indexreg)&0x7),(shift),(size) == 8 ? 4 : (size)); \
	} while (0)

#if defined(__native_client_codegen__)

#define x64_mov_reg_memindex_size(inst,reg,basereg,disp,indexreg,shift,size) \
	do { \
		x64_codegen_pre(inst); \
		if (x64_is_nacl_stack_reg((reg))) { \
			/* Clear upper 32 bits with mov of size 4 */ \
			x64_mov_reg_memindex_size_body((inst), (reg), (basereg), (disp), (indexreg), (shift), 4); \
			/* Add %r15 using LEA to preserve flags */ \
			x64_lea_memindex_size((inst), (reg), (reg), 0, X64_R15, 0, 8); \
		} else { \
			x64_mov_reg_memindex_size_body((inst), (reg), (basereg), (disp), (indexreg), (shift), (size)); \
		} \
		x64_codegen_post(inst); \
	} while(0)

#define x64_mov_reg_membase(inst,reg,basereg,disp,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if (x64_is_nacl_stack_reg((reg))) { \
			/* Clear upper 32 bits with mov of size 4 */ \
			x64_mov_reg_membase_body((inst), (reg), (basereg), (disp), 4); \
			/* Add %r15 */ \
			x64_lea_memindex_size((inst), (reg), (reg), 0, X64_R15, 0, 8); \
		} else { \
			x64_mov_reg_membase_body((inst), (reg), (basereg), (disp), (size)); \
		} \
		x64_codegen_post(inst); \
	} while (0)

#else

#define x64_mov_reg_memindex_size(inst,reg,basereg,disp,indexreg,shift,size) \
	x64_mov_reg_memindex_size_body((inst),(reg),(basereg),(disp),(indexreg),(shift),(size))
#define x64_mov_reg_membase(inst,reg,basereg,disp,size)	\
	do {	\
		x64_mov_reg_membase_body((inst), (reg), (basereg), (disp), (size)); \
	} while (0)

#endif /*__native_client_codegen__*/

#define x64_movzx_reg_membase(inst,reg,basereg,disp,size)	\
	do {	\
		x64_codegen_pre(inst); \
		x64_emit_rex(inst, (size), (reg), 0, (basereg)); \
		switch ((size)) {	\
		case 1: *(inst)++ = (unsigned char)0x0f; *(inst)++ = (unsigned char)0xb6; break;	\
		case 2: *(inst)++ = (unsigned char)0x0f; *(inst)++ = (unsigned char)0xb7; break;	\
		case 4: case 8: *(inst)++ = (unsigned char)0x8b; break;	\
		default: assert (0);	\
		}	\
		x86_membase_emit ((inst), ((reg)&0x7), ((basereg)&0x7), (disp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movsxd_reg_mem(inst,reg,mem) \
    do {     \
	x64_codegen_pre(inst); \
	x64_emit_rex(inst,8,(reg),0,0); \
	*(inst)++ = (unsigned char)0x63; \
	x86_mem_emit ((inst), ((reg)&0x7), (mem)); \
	x64_codegen_post(inst); \
    } while (0)

#define x64_movsxd_reg_membase(inst,reg,basereg,disp) \
    do {     \
	x64_codegen_pre(inst); \
	x64_emit_rex(inst,8,(reg),0,(basereg)); \
	*(inst)++ = (unsigned char)0x63; \
	x86_membase_emit ((inst), ((reg)&0x7), ((basereg)&0x7), (disp)); \
	x64_codegen_post(inst); \
    } while (0)

#define x64_movsxd_reg_memindex(inst, reg, basereg, disp, indexreg, shift) \
    do {     \
	x64_codegen_pre(inst); \
	x64_emit_rex(inst,8,(reg),0,(basereg)); \
	*(inst)++ = (unsigned char)0x63; \
	x64_memindex_emit((inst), (reg), (basereg), (disp), (indexreg), (shift)); \
	x64_codegen_post(inst); \
    } while (0)

#define x64_movsxd_reg_reg(inst,dreg,reg) \
    do {     \
	x64_codegen_pre(inst); \
	x64_emit_rex(inst,8,(dreg),0,(reg)); \
	*(inst)++ = (unsigned char)0x63; \
	x86_reg_emit ((inst), (dreg), (reg));	\
	x64_codegen_post(inst); \
    } while (0)

/* Pretty much the only instruction that supports a 64-bit immediate. Optimize for common case of
 * 32-bit immediate. Pepper with casts to avoid warnings.
 */
#define x64_mov_reg_imm_size(inst,reg,imm,size)	\
	do {	\
		x64_codegen_pre(inst); \
		x64_emit_rex(inst, (size), 0, 0, (reg)); \
		*(inst)++ = (unsigned char)0xb8 + ((reg) & 0x7);	\
		if ((size) == 8) \
			x86_imm_emit64 ((inst), (uint64_t)(imm));	\
		else \
			x86_imm_emit32 ((inst), (int)(uint64_t)(imm));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_mov_reg_imm(inst,reg,imm)	\
	do {	\
		int _x64_width_temp = ((uint64_t)(imm) == (uint64_t)(int)(uint64_t)(imm)); \
		x64_codegen_pre(inst); \
		x64_mov_reg_imm_size ((inst), (reg), (imm), (_x64_width_temp ? 4 : 8)); \
		x64_codegen_post(inst); \
	} while (0)

#define x64_set_reg_template(inst,reg) x64_mov_reg_imm_size ((inst),(reg), 0, 8)

#define x64_set_template(inst,reg) x64_set_reg_template((inst),(reg))

#define x64_mov_membase_imm(inst,basereg,disp,imm,size)	\
	do {	\
		x64_codegen_pre(inst); \
		if ((size) == 2) \
			x86_prefix((inst), X86_OPERAND_PREFIX); \
		x64_emit_rex(inst, (size) == 1 ? 0 : (size), 0, 0, (basereg)); \
		if ((size) == 1) {	\
			*(inst)++ = (unsigned char)0xc6;	\
			x86_membase_emit ((inst), 0, (basereg) & 0x7, (disp));	\
			x86_imm_emit8 ((inst), (imm));	\
		} else if ((size) == 2) {	\
			*(inst)++ = (unsigned char)0xc7;	\
			x86_membase_emit ((inst), 0, (basereg) & 0x7, (disp));	\
			x86_imm_emit16 ((inst), (imm));	\
		} else {	\
			*(inst)++ = (unsigned char)0xc7;	\
			x86_membase_emit ((inst), 0, (basereg) & 0x7, (disp));	\
			x86_imm_emit32 ((inst), (imm));	\
		}	\
		x64_codegen_post(inst); \
	} while (0)


#define x64_lea_membase_body(inst,reg,basereg,disp)	\
	do {	\
		x64_emit_rex(inst, 8, (reg), 0, (basereg)); \
		*(inst)++ = (unsigned char)0x8d;	\
		x64_membase_emit ((inst), (reg), (basereg), (disp));	\
	} while (0)

#if defined(__native_client_codegen__)
/* NaCl modules may not write directly into RSP/RBP. Instead, use a */
/*  32-bit LEA and add R15 to the effective address */
#define x64_lea_membase(inst,reg,basereg,disp) \
	do { \
		x64_codegen_pre(inst); \
		if (x64_is_nacl_stack_reg(reg)) { \
			/* 32-bit LEA */ \
			x64_emit_rex((inst), 4, (reg), 0, (basereg)); \
			*(inst)++ = (unsigned char)0x8d; \
			x64_membase_emit((inst), (reg), (basereg), (disp)); \
			/* Use a 64-bit LEA instead of an ADD to preserve flags */ \
			x64_lea_memindex_size((inst), (reg), (reg), 0, X64_R15, 0, 8); \
		} else { \
			x64_lea_membase_body((inst), (reg), (basereg), (disp)); \
		} \
		x64_codegen_post(inst); \
	} while (0)
#else
#define x64_lea_membase(inst,reg,basereg,disp) \
	x64_lea_membase_body((inst), (reg), (basereg), (disp))
#endif /*__native_client_codegen__*/

/* Instruction are implicitly 64-bits so don't generate REX for just the size. */
#define x64_push_reg(inst,reg)	\
	do {	\
		x64_codegen_pre(inst); \
		x64_emit_rex(inst, 0, 0, 0, (reg)); \
		*(inst)++ = (unsigned char)0x50 + ((reg) & 0x7);	\
		x64_codegen_post(inst); \
	} while (0)

/* Instruction is implicitly 64-bits so don't generate REX for just the size. */
#define x64_push_membase(inst,basereg,disp)	\
	do {	\
		x64_codegen_pre(inst); \
		x64_emit_rex(inst, 0, 0, 0, (basereg)); \
		*(inst)++ = (unsigned char)0xff;	\
		x86_membase_emit ((inst), 6, (basereg) & 0x7, (disp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_pop_reg_body(inst,reg)	\
	do {	\
		x64_codegen_pre(inst);  \
		x64_emit_rex(inst, 0, 0, 0, (reg)); \
		*(inst)++ = (unsigned char)0x58 + ((reg) & 0x7);	\
		x64_codegen_post(inst);  \
	} while (0)

#if defined(__native_client_codegen__)

/* Size is ignored for Native Client jumps, we restrict jumping to 32-bits */
#define x64_jump_reg_size(inst,reg,size)                                \
  do {                                                                    \
    x64_codegen_pre((inst));                                            \
    x64_alu_reg_imm_size((inst), X86_AND, (reg), (nacl_align_byte), 4); \
    x64_alu_reg_reg_size((inst), X86_ADD, (reg), X64_R15, 8);         \
    x64_emit_rex ((inst),0,0,0,(reg));                                  \
    x86_jump_reg((inst),((reg)&0x7));                                     \
    x64_codegen_post((inst));                                           \
  } while (0)

/* Size is ignored for Native Client jumps, we restrict jumping to 32-bits */
#define x64_jump_mem_size(inst,mem,size)                                \
  do {                                                                    \
    x64_codegen_pre((inst));                                            \
    x64_mov_reg_mem((inst), (mem), X64_R11, 4);                       \
    x64_jump_reg_size((inst), X64_R11, 4);                            \
    x64_codegen_post((inst));                                           \
  } while (0)

#define x64_call_reg_internal(inst,reg)                                 \
  do {                                                                    \
    x64_codegen_pre((inst));                                            \
    x64_alu_reg_imm_size((inst), X86_AND, (reg), (nacl_align_byte), 4); \
    x64_alu_reg_reg_size((inst), X86_ADD, (reg), X64_R15, 8);         \
    x64_emit_rex((inst), 0, 0, 0, (reg));                               \
    x86_call_reg((inst), ((reg) & 0x7));                                  \
    x64_codegen_post((inst));                                           \
  } while (0)

#define x64_call_reg(inst,reg)                                          \
  do {                                                                    \
    x64_codegen_pre((inst));                                            \
    x64_call_sequence_pre(inst);                                        \
    x64_call_reg_internal((inst), (reg));                               \
    x64_call_sequence_post(inst);                                       \
    x64_codegen_post((inst));                                           \
  } while (0)


#define x64_ret(inst)                                                   \
  do {                                                                    \
    x64_codegen_pre(inst);						  \
    x64_pop_reg_body((inst), X64_R11);                                \
    x64_jump_reg_size((inst), X64_R11, 8);                            \
    x64_codegen_post(inst);						  \
  } while (0)

#define x64_leave(inst)                                                 \
  do {                                                                    \
    x64_codegen_pre(inst);						  \
    x64_mov_reg_reg((inst), X64_RSP, X64_RBP, 8);                   \
    x64_pop_reg_body((inst), X64_R11);                                \
    x64_mov_reg_reg_size((inst), X64_RBP, X64_R11, 4);              \
    x64_alu_reg_reg_size((inst), X86_ADD, X64_RBP, X64_R15, 8);     \
    x64_codegen_post(inst);						  \
  } while (0)

#define x64_pop_reg(inst,reg) \
	do { \
		x64_codegen_pre(inst); \
		if (x64_is_nacl_stack_reg((reg))) { \
			x64_pop_reg_body((inst), X64_R11); \
			x64_mov_reg_reg_size((inst), (reg), X64_R11, 4); \
			x64_alu_reg_reg_size((inst), X86_ADD, (reg), X64_R15, 8); \
		} else { \
			x64_pop_reg_body((inst), (reg)); \
		} \
		x64_codegen_post(inst); \
	} while (0)

#else

#define x64_call_reg(inst,reg)	\
	do {	\
		x64_emit_rex(inst, 0, 0, 0, (reg)); \
		*(inst)++ = (unsigned char)0xff;	\
		x86_reg_emit ((inst), 2, ((reg) & 0x7));	\
	} while (0)


#define x64_ret(inst) do { *(inst)++ = (unsigned char)0xc3; } while (0)
#define x64_leave(inst) do { *(inst)++ = (unsigned char)0xc9; } while (0)

#define x64_pop_reg(inst,reg) x64_pop_reg_body((inst), (reg))

#endif /*__native_client_codegen__*/

#define x64_movsd_reg_regp(inst,reg,regp)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf2); \
		x64_emit_rex(inst, 0, (reg), 0, (regp)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x10;	\
		x86_regp_emit ((inst), (reg) & 0x7, (regp) & 0x7);	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movsd_regp_reg(inst,regp,reg)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf2); \
		x64_emit_rex(inst, 0, (reg), 0, (regp)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x11;	\
		x86_regp_emit ((inst), (reg) & 0x7, (regp) & 0x7);	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movss_reg_regp(inst,reg,regp)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf3); \
		x64_emit_rex(inst, 0, (reg), 0, (regp)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x10;	\
		x86_regp_emit ((inst), (reg) & 0x7, (regp) & 0x7);	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movss_regp_reg(inst,regp,reg)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf3); \
		x64_emit_rex(inst, 0, (reg), 0, (regp)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x11;	\
		x86_regp_emit ((inst), (reg) & 0x7, (regp) & 0x7);	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movsd_reg_membase(inst,reg,basereg,disp)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf2); \
		x64_emit_rex(inst, 0, (reg), 0, (basereg)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x10;	\
		x86_membase_emit ((inst), (reg) & 0x7, (basereg) & 0x7, (disp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movss_reg_membase(inst,reg,basereg,disp)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf3); \
		x64_emit_rex(inst, 0, (reg), 0, (basereg)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x10;	\
		x86_membase_emit ((inst), (reg) & 0x7, (basereg) & 0x7, (disp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movsd_membase_reg(inst,basereg,disp,reg)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf2); \
		x64_emit_rex(inst, 0, (reg), 0, (basereg)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x11;	\
		x86_membase_emit ((inst), (reg) & 0x7, (basereg) & 0x7, (disp));	\
		x64_codegen_post(inst); \
	} while (0)

#define x64_movss_membase_reg(inst,basereg,disp,reg)	\
	do {	\
		x64_codegen_pre(inst); \
		x86_prefix((inst), 0xf3); \
		x64_emit_rex(inst, 0, (reg), 0, (basereg)); \
		*(inst)++ = (unsigned char)0x0f;	\
		*(inst)++ = (unsigned char)0x11;	\
		x86_membase_emit ((inst), (reg) & 0x7, (basereg) & 0x7, (disp));	\
		x64_codegen_post(inst); \
	} while (0)

/* The original inc_reg opcode is used as the REX prefix */
#define x64_inc_reg_size(inst,reg,size) \
	do { \
		x64_codegen_pre(inst); \
		x64_emit_rex ((inst),(size),0,0,(reg)); \
		*(inst)++ = (unsigned char)0xff; \
		x86_reg_emit ((inst),0,(reg) & 0x7); \
		x64_codegen_post(inst); \
	} while (0)

#define x64_dec_reg_size(inst,reg,size) \
	do { \
		x64_codegen_pre(inst); \
		x64_emit_rex ((inst),(size),0,0,(reg)); \
		*(inst)++ = (unsigned char)0xff; \
		x86_reg_emit ((inst),1,(reg) & 0x7); \
		x64_codegen_post(inst); \
	} while (0)

#define x64_fld_membase_size(inst,basereg,disp,is_double,size) do { \
	x64_codegen_pre(inst); \
	x64_emit_rex ((inst),0,0,0,(basereg)); \
	*(inst)++ = (is_double) ? (unsigned char)0xdd : (unsigned char)0xd9;	\
	x64_membase_emit ((inst), 0, (basereg), (disp));	\
	x64_codegen_post(inst); \
} while (0)

#if defined(__native_client_codegen__)

/* The 3-7 byte NOP sequences in x64_padding_size below are all illegal in */
/* 64-bit Native Client because they load into rSP/rBP or use duplicate */
/* prefixes. Instead we use the NOPs recommended in Section 3.5.1.8 of the */
/* Intel64 and IA-32 Architectures Optimization Reference Manual and */
/* Section 4.13 of AMD Software Optimization Guide for Family 10h Processors. */

#define x64_padding_size(inst,size) \
	do { \
		unsigned char *code_start = (inst); \
		switch ((size)) { \
			/* xchg %eax,%eax, recognized by hardware as a NOP */ \
			case 1: *(inst)++ = 0x90; break; \
			/* xchg %ax,%ax */ \
			case 2: *(inst)++ = 0x66; *(inst)++ = 0x90; \
				break; \
			/* nop (%rax) */ \
			case 3: *(inst)++ = 0x0f; *(inst)++ = 0x1f; \
				*(inst)++ = 0x00; \
				break; \
			/* nop 0x0(%rax) */ \
			case 4: *(inst)++ = 0x0f; *(inst)++ = 0x1f; \
				x86_address_byte ((inst), 1, 0, X64_RAX);	\
				x86_imm_emit8 ((inst), 0);	\
				break; \
			/* nop 0x0(%rax,%rax) */ \
			case 5: *(inst)++ = 0x0f; *(inst)++ = 0x1f; \
				x86_address_byte ((inst), 1, 0, 4);	\
				x86_address_byte ((inst), 0, X64_RAX, X64_RAX);	\
				x86_imm_emit8 ((inst), 0);	\
				break; \
			/* nopw 0x0(%rax,%rax) */ \
			case 6: *(inst)++ = 0x66; *(inst)++ = 0x0f; \
				*(inst)++ = 0x1f; \
				x86_address_byte ((inst), 1, 0, 4);	\
				x86_address_byte ((inst), 0, X64_RAX, X64_RAX);	\
				x86_imm_emit8 ((inst), 0);	\
				break; \
			/* nop 0x0(%rax) (32-bit displacement) */ \
			case 7: *(inst)++ = 0x0f; *(inst)++ = 0x1f; \
				x86_address_byte ((inst), 2, 0, X64_RAX);	\
				x86_imm_emit32((inst), 0); \
				break; \
			/* nop 0x0(%rax,%rax) (32-bit displacement) */ \
			case 8: *(inst)++ = 0x0f; *(inst)++ = 0x1f; \
				x86_address_byte ((inst), 2, 0, 4);	\
				x86_address_byte ((inst), 0, X64_RAX, X64_RAX);	\
				x86_imm_emit32 ((inst), 0);	\
				break; \
			default: \
				g_assert_not_reached(); \
		} \
		g_assert(code_start + (size) == (unsigned char *)(inst)); \
	} while (0)

/* Size is ignored for Native Client calls, we restrict jumping to 32-bits */
#define x64_call_membase_size(inst,basereg,disp,size)                   \
  do {                                                                    \
    x64_codegen_pre((inst));                                            \
    x64_call_sequence_pre(inst);                                        \
    x64_mov_reg_membase((inst), X64_R11, (basereg), (disp), 4);       \
    x64_call_reg_internal((inst), X64_R11);                           \
    x64_call_sequence_post(inst);                                       \
    x64_codegen_post((inst));                                           \
  } while (0)

/* Size is ignored for Native Client jumps, we restrict jumping to 32-bits */
#define x64_jump_membase_size(inst,basereg,disp,size)                   \
  do {                                                                    \
    x64_mov_reg_membase((inst), X64_R11, (basereg), (disp), 4);       \
    x64_jump_reg_size((inst), X64_R11, 4);                            \
  } while (0)
    
/* On Native Client we can't jump more than INT_MAX in either direction */
#define x64_jump_code_size(inst,target,size)                            \
  do {                                                                    \
    /* x86_jump_code used twice in case of */                             \
    /* relocation by x64_codegen_post    */                             \
    uint8_t* jump_start;                                                   \
    x64_codegen_pre(inst);                                              \
    assert(x64_is_imm32 ((int64_t)(target) - (int64_t)(inst)));           \
    x86_jump_code((inst),(target));                                       \
    inst = x64_codegen_post(inst);                                      \
    jump_start = (inst);                                                  \
    x86_jump_code((inst),(target));                                       \
    mono_x64_patch(jump_start, (target));                               \
} while (0)

#else

/* From the AMD64 Software Optimization Manual */
#define x64_padding_size(inst,size) \
    do { \
	    switch ((size)) {								  \
        case 1: *(inst)++ = 0x90; break;						  \
        case 2: *(inst)++ = 0x66; *(inst)++ = 0x90; break;			  \
        case 3: *(inst)++ = 0x66; *(inst)++ = 0x66; *(inst)++ = 0x90; break; \
		default: x64_emit_rex ((inst),8,0,0,0); x86_padding ((inst), (size) - 1); \
		}; \
		} while (0)

#define x64_call_membase_size(inst,basereg,disp,size) do { x64_emit_rex ((inst),0,0,0,(basereg)); *(inst)++ = (unsigned char)0xff; x64_membase_emit ((inst),2, (basereg),(disp)); } while (0)
#define x64_jump_membase_size(inst,basereg,disp,size) do { x64_emit_rex ((inst),0,0,0,(basereg)); *(inst)++ = (unsigned char)0xff; x64_membase_emit ((inst), 4, (basereg), (disp)); } while (0)
    
#define x64_jump_code_size(inst,target,size) do { \
	if (x64_is_imm32 ((int64_t)(target) - (int64_t)(inst))) {		\
		x86_jump_code((inst),(target));									\
	} else {															\
	    x64_jump_membase ((inst), X64_RIP, 0);							\
		*(uint64_t*)(inst) = (uint64_t)(target);							\
		(inst) += 8; \
	} \
} while (0)

#endif /*__native_client_codegen__*/

/*
 * SSE
 */

//TODO Reorganize SSE opcode defines.

/* Two opcode SSE defines */
#define emit_sse_reg_reg_op2(inst, dreg, reg, op1, op2) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (reg), (op1), (op2), 0)

#define emit_sse_reg_reg_op2_size(inst, dreg, reg, op1, op2, size) \
	do { \
		x64_codegen_pre(inst); \
		x64_emit_rex ((inst), size, (dreg), 0, (reg)); \
		*(inst)++ = (unsigned char)(op1); \
		*(inst)++ = (unsigned char)(op2); \
		x86_reg_emit ((inst), (dreg), (reg)); \
		x64_codegen_post(inst); \
	} while (0)

#define emit_sse_reg_reg_op2_imm(inst, dreg, reg, op1, op2, imm) \
	do { \
		x64_codegen_pre(inst); \
		emit_sse_reg_reg_op2 ((inst), (dreg), (reg), (op1), (op2)); \
		x86_imm_emit8 ((inst), (imm)); \
		x64_codegen_post(inst); \
	} while (0)

#define emit_sse_membase_reg_op2(inst, basereg, disp, reg, op1, op2) \
	emit_sse_membase_reg_op2_size((inst), (basereg), (disp), (reg), (op1), (op2), 0)

#define emit_sse_membase_reg_op2_size(inst, basereg, disp, reg, op1, op2, size) \
	do { \
		x64_codegen_pre(inst); \
		x64_emit_rex ((inst), (size), (reg), 0, (basereg)); \
		*(inst)++ = (unsigned char)(op1); \
		*(inst)++ = (unsigned char)(op2); \
		x64_membase_emit ((inst), (reg), (basereg), (disp)); \
		x64_codegen_post(inst); \
	} while (0)

#define emit_sse_memindex_reg_op2(inst, basereg, disp, indexreg, shift, reg, op1, op2) \
	do { \
	    x64_codegen_pre(inst); \
		x64_emit_rex (inst, 0, (reg), (indexreg), (basereg)); \
		*(inst)++ = (unsigned char)(op1); \
		*(inst)++ = (unsigned char)(op2); \
		x64_memindex_emit((inst), (reg), (basereg), (disp), (indexreg), (shift)); \
		x64_codegen_post(inst); \
	} while(0)

#define emit_sse_reg_membase_op2(inst, dreg, basereg, disp, op1, op2) \
	emit_sse_reg_membase_op2_size((inst), (dreg), (basereg), (disp), (op1), (op2), 0)

#define emit_sse_reg_membase_op2_size(inst, dreg, basereg, disp, op1, op2, size) \
	do { \
		x64_codegen_pre(inst); \
		x64_emit_rex ((inst), (size), (dreg), 0, (basereg) == X64_RIP ? 0 : (basereg)); \
		*(inst)++ = (unsigned char)(op1); \
		*(inst)++ = (unsigned char)(op2); \
		x64_membase_emit ((inst), (dreg), (basereg), (disp)); \
		x64_codegen_post(inst); \
	} while (0)

#define emit_sse_reg_memindex_op2(inst, dreg, basereg, disp, indexreg, shift, op1, op2) \
	do { \
	    x64_codegen_pre(inst); \
		x64_emit_rex (inst, 0, (dreg), (indexreg), (basereg) == X64_RIP ? 0 : (basereg)); \
		*(inst)++ = (unsigned char)(op1); \
		*(inst)++ = (unsigned char)(op2); \
		x64_memindex_emit((inst), (dreg), (basereg), (disp), (indexreg), (shift)); \
		x64_codegen_post(inst); \
	} while(0)

/* Three opcode SSE defines */
#define emit_opcode3(inst,op1,op2,op3) do { \
   *(inst)++ = (unsigned char)(op1); \
   *(inst)++ = (unsigned char)(op2); \
   *(inst)++ = (unsigned char)(op3); \
} while (0)

#define emit_sse_reg_reg_size(inst,dreg,reg,op1,op2,op3,size) do { \
    x64_codegen_pre(inst); \
    *(inst)++ = (unsigned char)(op1); \
	x64_emit_rex ((inst), size, (dreg), 0, (reg)); \
    *(inst)++ = (unsigned char)(op2); \
    *(inst)++ = (unsigned char)(op3); \
    x86_reg_emit ((inst), (dreg), (reg)); \
    x64_codegen_post(inst); \
} while (0)

#define emit_sse_reg_reg(inst,dreg,reg,op1,op2,op3) emit_sse_reg_reg_size ((inst), (dreg), (reg), (op1), (op2), (op3), 0)

#define emit_sse_reg_reg_imm(inst,dreg,reg,op1,op2,op3,imm) do { \
   x64_codegen_pre(inst); \
   emit_sse_reg_reg ((inst), (dreg), (reg), (op1), (op2), (op3)); \
   x86_imm_emit8 ((inst), (imm)); \
   x64_codegen_post(inst); \
} while (0)

#define emit_sse_membase_reg(inst,basereg,disp,reg,op1,op2,op3) do { \
    x64_codegen_pre(inst); \
    x86_prefix((inst), (unsigned char)(op1)); \
    x64_emit_rex ((inst), 0, (reg), 0, (basereg)); \
    *(inst)++ = (unsigned char)(op2); \
    *(inst)++ = (unsigned char)(op3); \
    x64_membase_emit ((inst), (reg), (basereg), (disp)); \
    x64_codegen_post(inst); \
} while (0)

#define emit_sse_reg_membase(inst,dreg,basereg,disp,op1,op2,op3) do { \
    x64_codegen_pre(inst); \
    x86_prefix((inst), (unsigned char)(op1)); \
    x64_emit_rex ((inst), 0, (dreg), 0, (basereg) == X64_RIP ? 0 : (basereg)); \
    *(inst)++ = (unsigned char)(op2); \
    *(inst)++ = (unsigned char)(op3); \
    x64_membase_emit ((inst), (dreg), (basereg), (disp)); \
    x64_codegen_post(inst); \
} while (0)

/* Four opcode SSE defines */

#define emit_sse_reg_reg_op4_size(inst,dreg,reg,op1,op2,op3,op4,size) do { \
    x64_codegen_pre(inst); \
    x86_prefix((inst), (unsigned char)(op1)); \
    x64_emit_rex ((inst), size, (dreg), 0, (reg)); \
    *(inst)++ = (unsigned char)(op2); \
    *(inst)++ = (unsigned char)(op3); \
    *(inst)++ = (unsigned char)(op4); \
    x86_reg_emit ((inst), (dreg), (reg)); \
    x64_codegen_post(inst); \
} while (0)

#define emit_sse_reg_reg_op4(inst,dreg,reg,op1,op2,op3,op4) emit_sse_reg_reg_op4_size ((inst), (dreg), (reg), (op1), (op2), (op3), (op4), 0)

/* specific SSE opcode defines */
 
#define x64_sse_xorpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst),(dreg),(reg), 0x66, 0x0f, 0x57)

#define x64_sse_xorpd_reg_membase(inst,dreg,basereg,disp) emit_sse_reg_membase ((inst),(dreg),(basereg), (disp), 0x66, 0x0f, 0x57)

#define x64_sse_andpd_reg_membase(inst,dreg,basereg,disp) emit_sse_reg_membase ((inst),(dreg),(basereg), (disp), 0x66, 0x0f, 0x54)

#define x64_sse_movsd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf2, 0x0f, 0x10)

#define x64_sse_movsd_reg_membase(inst,dreg,basereg,disp) emit_sse_reg_membase ((inst), (dreg), (basereg), (disp), 0xf2, 0x0f, 0x10)

#define x64_sse_movsd_membase_reg(inst,basereg,disp,reg) emit_sse_membase_reg ((inst), (basereg), (disp), (reg), 0xf2, 0x0f, 0x11)

#define x64_sse_movss_membase_reg(inst,basereg,disp,reg) emit_sse_membase_reg ((inst), (basereg), (disp), (reg), 0xf3, 0x0f, 0x11)

#define x64_sse_movss_reg_membase(inst,dreg,basereg,disp) emit_sse_reg_membase ((inst), (dreg), (basereg), (disp), 0xf3, 0x0f, 0x10)

#define x64_sse_comisd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst),(dreg),(reg),0x66,0x0f,0x2f)

#define x64_sse_comisd_reg_membase(inst,dreg,basereg,disp) emit_sse_reg_membase ((inst), (dreg), (basereg), (disp), 0x66, 0x0f, 0x2f)

#define x64_sse_ucomisd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst),(dreg),(reg),0x66,0x0f,0x2e)

#define x64_sse_cvtsd2si_reg_reg(inst,dreg,reg) emit_sse_reg_reg_size ((inst), (dreg), (reg), 0xf2, 0x0f, 0x2d, 8)

#define x64_sse_cvttsd2si_reg_reg_size(inst,dreg,reg,size) emit_sse_reg_reg_size ((inst), (dreg), (reg), 0xf2, 0x0f, 0x2c, (size))

#define x64_sse_cvttsd2si_reg_reg(inst,dreg,reg) x64_sse_cvttsd2si_reg_reg_size ((inst), (dreg), (reg), 8)

#define x64_sse_cvtsi2sd_reg_reg_size(inst,dreg,reg,size) emit_sse_reg_reg_size ((inst), (dreg), (reg), 0xf2, 0x0f, 0x2a, (size))

#define x64_sse_cvtsi2sd_reg_reg(inst,dreg,reg) x64_sse_cvtsi2sd_reg_reg_size ((inst), (dreg), (reg), 8)

#define x64_sse_cvtsi2ss_reg_reg_size(inst,dreg,reg,size) emit_sse_reg_reg_size ((inst), (dreg), (reg), 0xf3, 0x0f, 0x2a, (size))

#define x64_sse_cvtsi2ss_reg_reg(inst,dreg,reg) x64_sse_cvtsi2ss_reg_reg_size ((inst), (dreg), (reg), 8)

#define x64_sse_cvtsd2ss_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf2, 0x0f, 0x5a)

#define x64_sse_cvtss2sd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf3, 0x0f, 0x5a)

#define x64_sse_addsd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf2, 0x0f, 0x58)

#define x64_sse_subsd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf2, 0x0f, 0x5c)

#define x64_sse_mulsd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf2, 0x0f, 0x59)

#define x64_sse_divsd_reg_reg(inst,dreg,reg) emit_sse_reg_reg ((inst), (dreg), (reg), 0xf2, 0x0f, 0x5e)

#define x64_sse_sqrtsd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf2, 0x0f, 0x51)


#define x64_sse_pinsrw_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm ((inst), (dreg), (reg), 0x66, 0x0f, 0xc4, (imm))

#define x64_sse_pextrw_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm ((inst), (dreg), (reg), 0x66, 0x0f, 0xc5, (imm))


#define x64_sse_cvttsd2si_reg_xreg_size(inst,reg,xreg,size) emit_sse_reg_reg_size ((inst), (reg), (xreg), 0xf2, 0x0f, 0x2c, (size))


#define x64_sse_addps_reg_reg(inst, dreg, reg) \
	emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x58)

#define x64_sse_addps_reg_reg_size(inst, dreg, reg, size) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (reg), 0x0f, 0x58, size)

#define x64_sse_divps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x5e)

#define x64_sse_mulps_reg_reg(inst, dreg, reg) \
	emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x59)

#define x64_sse_mulps_reg_reg_size(inst, dreg, reg, size) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (reg), 0x0f, 0x59, size)

#define x64_sse_subps_reg_reg(inst, dreg, reg) \
	emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x5c)

#define x64_sse_subps_reg_reg_size(inst, dreg, reg, size) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (reg), 0x0f, 0x5c, size)

#define x64_sse_maxps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x5f)

#define x64_sse_minps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x5d)

#define x64_sse_cmpps_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_op2_imm((inst), (dreg), (reg), 0x0f, 0xc2, (imm))

#define x64_sse_andps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x54)

#define x64_sse_andnps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x55)

#define x64_sse_orps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x56)

#define x64_sse_xorps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x57)

#define x64_sse_sqrtps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x51)

#define x64_sse_rsqrtps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x52)

#define x64_sse_rcpps_reg_reg(inst,dreg,reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x53)

#define x64_sse_addsubps_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf2, 0x0f, 0xd0)

#define x64_sse_haddps_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf2, 0x0f, 0x7c)

#define x64_sse_hsubps_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf2, 0x0f, 0x7d)

#define x64_sse_movshdup_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf3, 0x0f, 0x16)

#define x64_sse_movsldup_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf3, 0x0f, 0x12)


#define x64_sse_pshufhw_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm((inst), (dreg), (reg), 0xf3, 0x0f, 0x70, (imm))

#define x64_sse_pshuflw_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm((inst), (dreg), (reg), 0xf2, 0x0f, 0x70, (imm))

#define x64_sse_pshufd_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm((inst), (dreg), (reg), 0x66, 0x0f, 0x70, (imm))

#define x64_sse_shufps_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_op2_imm((inst), (dreg), (reg), 0x0f, 0xC6, (imm))

#define x64_sse_shufpd_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm((inst), (dreg), (reg), 0x66, 0x0f, 0xC6, (imm))


#define x64_sse_addpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x58)

#define x64_sse_divpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x5e)

#define x64_sse_mulpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x59)

#define x64_sse_subpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x5c)

#define x64_sse_maxpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x5f)

#define x64_sse_minpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x5d)

#define x64_sse_cmppd_reg_reg_imm(inst,dreg,reg,imm) emit_sse_reg_reg_imm((inst), (dreg), (reg), 0x66, 0x0f, 0xc2, (imm))

#define x64_sse_andpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x54)

#define x64_sse_andnpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x55)

#define x64_sse_orpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x56)

#define x64_sse_sqrtpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x51)

#define x64_sse_rsqrtpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x52)

#define x64_sse_rcppd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x53)

#define x64_sse_addsubpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd0)

#define x64_sse_haddpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x7c)

#define x64_sse_hsubpd_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x7d)

#define x64_sse_movddup_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xf2, 0x0f, 0x12)


#define x64_sse_pmovmskb_reg_reg(inst,dreg,reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd7)


#define x64_sse_pand_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xdb)

#define x64_sse_por_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xeb)

#define x64_sse_pxor_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xef)


#define x64_sse_paddb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xfc)

#define x64_sse_paddw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xfd)

#define x64_sse_paddd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xfe)

#define x64_sse_paddq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd4)


#define x64_sse_psubb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf8)

#define x64_sse_psubw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf9)

#define x64_sse_psubd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xfa)

#define x64_sse_psubq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xfb)


#define x64_sse_pmaxub_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xde)

#define x64_sse_pmaxuw_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x3e)

#define x64_sse_pmaxud_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x3f)


#define x64_sse_pmaxsb_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x3c)

#define x64_sse_pmaxsw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xee)

#define x64_sse_pmaxsd_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x3d)


#define x64_sse_pavgb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe0)

#define x64_sse_pavgw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe3)


#define x64_sse_pminub_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xda)

#define x64_sse_pminuw_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x3a)

#define x64_sse_pminud_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x3b)


#define x64_sse_pminsb_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x38)

#define x64_sse_pminsw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xea)

#define x64_sse_pminsd_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x39)


#define x64_sse_pcmpeqb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x74)

#define x64_sse_pcmpeqw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x75)

#define x64_sse_pcmpeqd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x76)

#define x64_sse_pcmpeqq_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x29)


#define x64_sse_pcmpgtb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x64)

#define x64_sse_pcmpgtw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x65)

#define x64_sse_pcmpgtd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x66)

#define x64_sse_pcmpgtq_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x37)


#define x64_sse_psadbw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf6)


#define x64_sse_punpcklbw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x60)

#define x64_sse_punpcklwd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x61)

#define x64_sse_punpckldq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x62)

#define x64_sse_punpcklqdq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x6c)

#define x64_sse_unpcklpd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x14)

#define x64_sse_unpcklps_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x14)


#define x64_sse_punpckhbw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x68)

#define x64_sse_punpckhwd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x69)

#define x64_sse_punpckhdq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x6a)

#define x64_sse_punpckhqdq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x6d)

#define x64_sse_unpckhpd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x15)

#define x64_sse_unpckhps_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x15)


#define x64_sse_packsswb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x63)

#define x64_sse_packssdw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x6b)

#define x64_sse_packuswb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x67)

#define x64_sse_packusdw_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x2b)


#define x64_sse_paddusb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xdc)

#define x64_sse_psubusb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd8)

#define x64_sse_paddusw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xdd)

#define x64_sse_psubusw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd8)


#define x64_sse_paddsb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xec)

#define x64_sse_psubsb_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe8)

#define x64_sse_paddsw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xed)

#define x64_sse_psubsw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe9)


#define x64_sse_pmullw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd5)

#define x64_sse_pmulld_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op4((inst), (dreg), (reg), 0x66, 0x0f, 0x38, 0x40)

#define x64_sse_pmuludq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf4)

#define x64_sse_pmulhuw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe4)

#define x64_sse_pmulhw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe5)


#define x64_sse_psrlw_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SHR, (reg), 0x66, 0x0f, 0x71, (imm))

#define x64_sse_psrlw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd1)


#define x64_sse_psraw_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SAR, (reg), 0x66, 0x0f, 0x71, (imm))

#define x64_sse_psraw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe1)


#define x64_sse_psllw_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SHL, (reg), 0x66, 0x0f, 0x71, (imm))

#define x64_sse_psllw_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf1)


#define x64_sse_psrld_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SHR, (reg), 0x66, 0x0f, 0x72, (imm))

#define x64_sse_psrld_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd2)


#define x64_sse_psrad_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SAR, (reg), 0x66, 0x0f, 0x72, (imm))

#define x64_sse_psrad_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe2)


#define x64_sse_pslld_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SHL, (reg), 0x66, 0x0f, 0x72, (imm))

#define x64_sse_pslld_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf2)


#define x64_sse_psrlq_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SHR, (reg), 0x66, 0x0f, 0x73, (imm))

#define x64_sse_psrlq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xd3)


#define x64_sse_psraq_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SAR, (reg), 0x66, 0x0f, 0x73, (imm))

#define x64_sse_psraq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xe3)


#define x64_sse_psllq_reg_imm(inst, reg, imm) emit_sse_reg_reg_imm((inst), X86_SSE_SHL, (reg), 0x66, 0x0f, 0x73, (imm))

#define x64_sse_psllq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0xf3)


#define x64_sse_cvtdq2pd_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xF3, 0x0F, 0xE6)

#define x64_sse_cvtdq2ps_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0F, 0x5B)

#define x64_sse_cvtpd2dq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xF2, 0x0F, 0xE6)

#define x64_sse_cvtpd2ps_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0F, 0x5A)

#define x64_sse_cvtps2dq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0F, 0x5B)

#define x64_sse_cvtps2pd_reg_reg(inst, dreg, reg) emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0F, 0x5A)

#define x64_sse_cvttpd2dq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0F, 0xE6)

#define x64_sse_cvttps2dq_reg_reg(inst, dreg, reg) emit_sse_reg_reg((inst), (dreg), (reg), 0xF3, 0x0F, 0x5B)


#define x64_movd_xreg_reg_size(inst,dreg,sreg,size) emit_sse_reg_reg_size((inst), (dreg), (sreg), 0x66, 0x0f, 0x6e, (size))

#define x64_movd_reg_xreg_size(inst,dreg,sreg,size) emit_sse_reg_reg_size((inst), (sreg), (dreg), 0x66, 0x0f, 0x7e, (size))

#define x64_movd_xreg_membase(inst,dreg,basereg,disp) emit_sse_reg_membase((inst), (dreg), (basereg), (disp), 0x66, 0x0f, 0x6e)

#define x64_sse_movhlps_reg_reg(inst, dreg, sreg) \
	emit_sse_reg_reg_op2((inst), (dreg), (sreg), 0x0f, 0x12)

#define x64_sse_movhlps_reg_reg_size(inst, dreg, sreg, size) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (sreg), 0x0f, 0x12, size)

#define x64_sse_movlhps_reg_reg(inst, dreg, sreg) \
	emit_sse_reg_reg_op2((inst), (dreg), (sreg), 0x0f, 0x16)

#define x64_sse_movlhps_reg_reg_size(inst, dreg, sreg, size) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (sreg), 0x0f, 0x16, size)

#define x64_sse_movups_membase_reg(inst, basereg, disp, reg) \
	emit_sse_membase_reg_op2((inst), (basereg), (disp), (reg), 0x0f, 0x11)

#define x64_sse_movups_membase_reg_size(inst, basereg, disp, reg, size) \
	emit_sse_membase_reg_op2_size((inst), (basereg), (disp), (reg), 0x0f, 0x11, (size))

#define x64_sse_movups_reg_membase(inst, dreg, basereg, disp) \
	emit_sse_reg_membase_op2((inst), (dreg), (basereg), (disp), 0x0f, 0x10)

#define x64_sse_movups_reg_membase_size(inst, dreg, basereg, disp, size) \
	emit_sse_reg_membase_op2_size((inst), (dreg), (basereg), (disp), 0x0f, 0x10, (size))

#define x64_sse_movaps_membase_reg(inst, basereg, disp, reg) \
	emit_sse_membase_reg_op2((inst), (basereg), (disp), (reg), 0x0f, 0x29)

#define x64_sse_movaps_membase_reg_size(inst, basereg, disp, reg, size) \
	emit_sse_membase_reg_op2_size((inst), (basereg), (disp), (reg), 0x0f, 0x29, (size))

#define x64_sse_movaps_memindex_reg(inst, basereg, disp, indexreg, shift, reg) \
	emit_sse_memindex_reg_op2((inst), (basereg), (disp), (indexreg), (shift), (reg), 0x0f, 0x29);

#define x64_sse_movaps_reg_membase(inst, dreg, basereg, disp) \
	emit_sse_reg_membase_op2((inst), (dreg), (basereg), (disp), 0x0f, 0x28)

#define x64_sse_movaps_reg_membase_size(inst, dreg, basereg, disp, size) \
	emit_sse_reg_membase_op2_size((inst), (dreg), (basereg), (disp), 0x0f, 0x28, (size))

#define x64_sse_movaps_reg_memindex(inst, dreg, basereg, disp, indexreg, shift) \
	emit_sse_reg_memindex_op2((inst), (dreg), (basereg), (disp), (indexreg), (shift), 0x0f, 0x28);

#define x64_sse_movaps_reg_reg(inst, dreg, reg) \
	emit_sse_reg_reg_op2((inst), (dreg), (reg), 0x0f, 0x28)

#define x64_sse_movaps_reg_reg_size(inst, dreg, reg, size) \
	emit_sse_reg_reg_op2_size((inst), (dreg), (reg), 0x0f, 0x28, size)

#define x64_sse_movntps_membase_reg(inst, basereg, disp, reg) \
	emit_sse_membase_reg_op2((inst), (basereg), (disp), (reg), 0x0f, 0x2b)

#define x64_sse_movntps_memindex_reg(inst, basereg, disp, indexreg, shift, reg) \
	emit_sse_memindex_reg_op2((inst), (basereg), (disp), (indexreg), (shift), (reg),  0x0f, 0x2b)

#define x64_sse_prefetch_reg_membase(inst, arg, basereg, disp) \
	emit_sse_reg_membase_op2((inst), (arg), (basereg), (disp), 0x0f, 0x18)

#define x64_sse_movdqa_membase_reg(inst, basereg, disp, reg) \
	emit_sse_membase_reg((inst), (basereg), (disp), (reg), 0x66, 0x0f, 0x7f)

#define x64_sse_movdqa_reg_membase(inst, dreg, basereg, disp) \
	emit_sse_reg_membase((inst), (dreg), (basereg), (disp), 0x66, 0x0f, 0x6f)

#define x64_sse_movdqa_reg_reg(inst, dreg, reg) \
	emit_sse_reg_reg((inst), (dreg), (reg), 0x66, 0x0f, 0x6f)

/* Generated from x86-codegen.h */

#define x64_breakpoint_size(inst,size) do { x86_breakpoint(inst); } while (0)
#define x64_cld_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_cld(inst); x64_codegen_post(inst); } while (0)
#define x64_stosb_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_stosb(inst); x64_codegen_post(inst); } while (0)
#define x64_stosl_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_stosl(inst); x64_codegen_post(inst); } while (0)
#define x64_stosd_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_stosd(inst); x64_codegen_post(inst); } while (0)
#define x64_movsb_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_movsb(inst); x64_codegen_post(inst); } while (0)
#define x64_movsl_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_movsl(inst); x64_codegen_post(inst); } while (0)
#define x64_movsd_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_movsd(inst); x64_codegen_post(inst); } while (0)
#define x64_prefix_size(inst,p,size) do { x86_prefix((inst), p); } while (0)
#define x64_rdtsc_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_rdtsc(inst); x64_codegen_post(inst); } while (0)
#define x64_cmpxchg_reg_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_cmpxchg_reg_reg((inst),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_cmpxchg_mem_reg_size(inst,mem,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_cmpxchg_mem_reg((inst),(mem),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_cmpxchg_membase_reg_size(inst,basereg,disp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_cmpxchg_membase_reg((inst),((basereg)&0x7),(disp),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_xchg_reg_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_xchg_reg_reg((inst),((dreg)&0x7),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_xchg_mem_reg_size(inst,mem,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_xchg_mem_reg((inst),(mem),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_xchg_membase_reg_size(inst,basereg,disp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_xchg_membase_reg((inst),((basereg)&0x7),(disp),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_inc_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_inc_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_inc_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_inc_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
//#define x64_inc_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_inc_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_dec_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_dec_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_dec_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_dec_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
//#define x64_dec_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_dec_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_not_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_not_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_not_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_not_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_not_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_not_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_neg_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_neg_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_neg_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_neg_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_neg_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_neg_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_nop_size(inst,size) do { x64_codegen_pre(inst); x86_nop(inst); x64_codegen_post(inst); } while (0)
//#define x64_alu_reg_imm_size(inst,opc,reg,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_alu_reg_imm((inst),(opc),((reg)&0x7),(imm)); x64_codegen_post(inst); } while (0)
#define x64_alu_mem_imm_size(inst,opc,mem,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_alu_mem_imm((inst),(opc),(mem),(imm)); x64_codegen_post(inst); } while (0)
#define x64_alu_membase_imm_size(inst,opc,basereg,disp,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_alu_membase_imm((inst),(opc),((basereg)&0x7),(disp),(imm)); x64_codegen_post(inst); } while (0)
#define x64_alu_membase8_imm_size(inst,opc,basereg,disp,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_alu_membase8_imm((inst),(opc),((basereg)&0x7),(disp),(imm)); x64_codegen_post(inst); } while (0)	
#define x64_alu_mem_reg_size(inst,opc,mem,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_alu_mem_reg((inst),(opc),(mem),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_alu_membase_reg_size(inst,opc,basereg,disp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_alu_membase_reg((inst),(opc),((basereg)&0x7),(disp),((reg)&0x7)); x64_codegen_post(inst); } while (0)
//#define x64_alu_reg_reg_size(inst,opc,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_alu_reg_reg((inst),(opc),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_alu_reg8_reg8_size(inst,opc,dreg,reg,is_dreg_h,is_reg_h,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_alu_reg8_reg8((inst),(opc),((dreg)&0x7),((reg)&0x7),(is_dreg_h),(is_reg_h)); x64_codegen_post(inst); } while (0)
#define x64_alu_reg_mem_size(inst,opc,reg,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_alu_reg_mem((inst),(opc),((reg)&0x7),(mem)); x64_codegen_post(inst); } while (0)
//#define x64_alu_reg_membase_size(inst,opc,reg,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_alu_reg_membase((inst),(opc),((reg)&0x7),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_test_reg_imm_size(inst,reg,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_test_reg_imm((inst),((reg)&0x7),(imm)); x64_codegen_post(inst); } while (0)
#define x64_test_mem_imm_size(inst,mem,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_test_mem_imm((inst),(mem),(imm)); x64_codegen_post(inst); } while (0)
#define x64_test_membase_imm_size(inst,basereg,disp,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_test_membase_imm((inst),((basereg)&0x7),(disp),(imm)); x64_codegen_post(inst); } while (0)
#define x64_test_reg_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_test_reg_reg((inst),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_test_mem_reg_size(inst,mem,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_test_mem_reg((inst),(mem),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_test_membase_reg_size(inst,basereg,disp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_test_membase_reg((inst),((basereg)&0x7),(disp),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_shift_reg_imm_size(inst,opc,reg,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_shift_reg_imm((inst),(opc),((reg)&0x7),(imm)); x64_codegen_post(inst); } while (0)
#define x64_shift_mem_imm_size(inst,opc,mem,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_shift_mem_imm((inst),(opc),(mem),(imm)); x64_codegen_post(inst); } while (0)
#define x64_shift_membase_imm_size(inst,opc,basereg,disp,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_shift_membase_imm((inst),(opc),((basereg)&0x7),(disp),(imm)); x64_codegen_post(inst); } while (0)
#define x64_shift_reg_size(inst,opc,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_shift_reg((inst),(opc),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_shift_mem_size(inst,opc,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_shift_mem((inst),(opc),(mem)); x64_codegen_post(inst); } while (0)
#define x64_shift_membase_size(inst,opc,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_shift_membase((inst),(opc),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_shrd_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_shrd_reg((inst),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_shrd_reg_imm_size(inst,dreg,reg,shamt,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_shrd_reg_imm((inst),((dreg)&0x7),((reg)&0x7),(shamt)); x64_codegen_post(inst); } while (0)
#define x64_shld_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_shld_reg((inst),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_shld_reg_imm_size(inst,dreg,reg,shamt,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_shld_reg_imm((inst),((dreg)&0x7),((reg)&0x7),(shamt)); x64_codegen_post(inst); } while (0)
#define x64_mul_reg_size(inst,reg,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_mul_reg((inst),((reg)&0x7),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_mul_mem_size(inst,mem,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_mul_mem((inst),(mem),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_mul_membase_size(inst,basereg,disp,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_mul_membase((inst),((basereg)&0x7),(disp),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_imul_reg_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_imul_reg_reg((inst),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_imul_reg_mem_size(inst,reg,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_imul_reg_mem((inst),((reg)&0x7),(mem)); x64_codegen_post(inst); } while (0)
#define x64_imul_reg_membase_size(inst,reg,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_imul_reg_membase((inst),((reg)&0x7),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_imul_reg_reg_imm_size(inst,dreg,reg,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_imul_reg_reg_imm((inst),((dreg)&0x7),((reg)&0x7),(imm)); x64_codegen_post(inst); } while (0)
#define x64_imul_reg_mem_imm_size(inst,reg,mem,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_imul_reg_mem_imm((inst),((reg)&0x7),(mem),(imm)); x64_codegen_post(inst); } while (0)
#define x64_imul_reg_membase_imm_size(inst,reg,basereg,disp,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_imul_reg_membase_imm((inst),((reg)&0x7),((basereg)&0x7),(disp),(imm)); x64_codegen_post(inst); } while (0)
#define x64_div_reg_size(inst,reg,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_div_reg((inst),((reg)&0x7),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_div_mem_size(inst,mem,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_div_mem((inst),(mem),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_div_membase_size(inst,basereg,disp,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_div_membase((inst),((basereg)&0x7),(disp),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_mov_mem_reg_size(inst,mem,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_mov_mem_reg((inst),(mem),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
//#define x64_mov_regp_reg_size(inst,regp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(regp),0,(reg)); x86_mov_regp_reg((inst),(regp),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
//#define x64_mov_membase_reg_size(inst,basereg,disp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_mov_membase_reg((inst),((basereg)&0x7),(disp),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_mov_memindex_reg_size(inst,basereg,disp,indexreg,shift,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),(indexreg),(basereg)); x86_mov_memindex_reg((inst),((basereg)&0x7),(disp),((indexreg)&0x7),(shift),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_mov_reg_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_mov_reg_reg((inst),((dreg)&0x7),((reg)&0x7),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
//#define x64_mov_reg_mem_size(inst,reg,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_mov_reg_mem((inst),((reg)&0x7),(mem),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
//#define x64_mov_reg_membase_size(inst,reg,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_mov_reg_membase((inst),((reg)&0x7),((basereg)&0x7),(disp),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
//#define x64_mov_reg_memindex_size(inst,reg,basereg,disp,indexreg,shift,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),(indexreg),(basereg)); x86_mov_reg_memindex((inst),((reg)&0x7),((basereg)&0x7),(disp),((indexreg)&0x7),(shift),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_clear_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_clear_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
//#define x64_mov_reg_imm_size(inst,reg,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_mov_reg_imm((inst),((reg)&0x7),(imm)); x64_codegen_post(inst); } while (0)
#define x64_mov_mem_imm_size(inst,mem,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_mov_mem_imm((inst),(mem),(imm),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
//#define x64_mov_membase_imm_size(inst,basereg,disp,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_mov_membase_imm((inst),((basereg)&0x7),(disp),(imm),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_mov_memindex_imm_size(inst,basereg,disp,indexreg,shift,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,(indexreg),(basereg)); x86_mov_memindex_imm((inst),((basereg)&0x7),(disp),((indexreg)&0x7),(shift),(imm),(size) == 8 ? 4 : (size)); x64_codegen_post(inst); } while (0)
#define x64_lea_mem_size(inst,reg,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_lea_mem((inst),((reg)&0x7),(mem)); x64_codegen_post(inst); } while (0)
//#define x64_lea_membase_size(inst,reg,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_lea_membase((inst),((reg)&0x7),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_lea_memindex_size(inst,reg,basereg,disp,indexreg,shift,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),(indexreg),(basereg)); x86_lea_memindex((inst),((reg)&0x7),((basereg)&0x7),(disp),((indexreg)&0x7),(shift)); x64_codegen_post(inst); } while (0)
#define x64_widen_reg_size(inst,dreg,reg,is_signed,is_half,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_widen_reg((inst),((dreg)&0x7),((reg)&0x7),(is_signed),(is_half)); x64_codegen_post(inst); } while (0)
#define x64_widen_mem_size(inst,dreg,mem,is_signed,is_half,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,0); x86_widen_mem((inst),((dreg)&0x7),(mem),(is_signed),(is_half)); x64_codegen_post(inst); } while (0)
#define x64_widen_membase_size(inst,dreg,basereg,disp,is_signed,is_half,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(basereg)); x86_widen_membase((inst),((dreg)&0x7),((basereg)&0x7),(disp),(is_signed),(is_half)); x64_codegen_post(inst); } while (0)
#define x64_widen_memindex_size(inst,dreg,basereg,disp,indexreg,shift,is_signed,is_half,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),(indexreg),(basereg)); x86_widen_memindex((inst),((dreg)&0x7),((basereg)&0x7),(disp),((indexreg)&0x7),(shift),(is_signed),(is_half)); x64_codegen_post(inst); } while (0)
#define x64_cdq_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_cdq(inst); x64_codegen_post(inst); } while (0)
#define x64_wait_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_wait(inst); x64_codegen_post(inst); } while (0)
#define x64_fp_op_mem_size(inst,opc,mem,is_double,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fp_op_mem((inst),(opc),(mem),(is_double)); x64_codegen_post(inst); } while (0)
#define x64_fp_op_membase_size(inst,opc,basereg,disp,is_double,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fp_op_membase((inst),(opc),((basereg)&0x7),(disp),(is_double)); x64_codegen_post(inst); } while (0)
#define x64_fp_op_size(inst,opc,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fp_op((inst),(opc),(index)); x64_codegen_post(inst); } while (0)
#define x64_fp_op_reg_size(inst,opc,index,pop_stack,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fp_op_reg((inst),(opc),(index),(pop_stack)); x64_codegen_post(inst); } while (0)
#define x64_fp_int_op_membase_size(inst,opc,basereg,disp,is_int,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fp_int_op_membase((inst),(opc),((basereg)&0x7),(disp),(is_int)); x64_codegen_post(inst); } while (0)
#define x64_fstp_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fstp((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fcompp_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fcompp(inst); x64_codegen_post(inst); } while (0)
#define x64_fucompp_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fucompp(inst); x64_codegen_post(inst); } while (0)
#define x64_fnstsw_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fnstsw(inst); x64_codegen_post(inst); } while (0)
#define x64_fnstcw_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fnstcw((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_fnstcw_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_fnstcw_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_fldcw_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fldcw((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_fldcw_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fldcw_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_fchs_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fchs(inst); x64_codegen_post(inst); } while (0)
#define x64_frem_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_frem(inst); x64_codegen_post(inst); } while (0)
#define x64_fxch_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fxch((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fcomi_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fcomi((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fcomip_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fcomip((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fucomi_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fucomi((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fucomip_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fucomip((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fld_size(inst,mem,is_double,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fld((inst),(mem),(is_double)); x64_codegen_post(inst); } while (0)
//#define x64_fld_membase_size(inst,basereg,disp,is_double,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fld_membase((inst),((basereg)&0x7),(disp),(is_double)); x64_codegen_post(inst); } while (0)
#define x64_fld80_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fld80_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_fld80_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_fld80_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_fild_size(inst,mem,is_long,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fild((inst),(mem),(is_long)); x64_codegen_post(inst); } while (0)
#define x64_fild_membase_size(inst,basereg,disp,is_long,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fild_membase((inst),((basereg)&0x7),(disp),(is_long)); x64_codegen_post(inst); } while (0)
#define x64_fld_reg_size(inst,index,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fld_reg((inst),(index)); x64_codegen_post(inst); } while (0)
#define x64_fldz_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fldz(inst); x64_codegen_post(inst); } while (0)
#define x64_fld1_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fld1(inst); x64_codegen_post(inst); } while (0)
#define x64_fldpi_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fldpi(inst); x64_codegen_post(inst); } while (0)
#define x64_fst_size(inst,mem,is_double,pop_stack,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fst((inst),(mem),(is_double),(pop_stack)); x64_codegen_post(inst); } while (0)
#define x64_fst_membase_size(inst,basereg,disp,is_double,pop_stack,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fst_membase((inst),((basereg)&0x7),(disp),(is_double),(pop_stack)); x64_codegen_post(inst); } while (0)
#define x64_fst80_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fst80_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_fst80_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fst80_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_fist_pop_size(inst,mem,is_long,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_fist_pop((inst),(mem),(is_long)); x64_codegen_post(inst); } while (0)
#define x64_fist_pop_membase_size(inst,basereg,disp,is_long,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fist_pop_membase((inst),((basereg)&0x7),(disp),(is_long)); x64_codegen_post(inst); } while (0)
#define x64_fstsw_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_fstsw(inst); x64_codegen_post(inst); } while (0)
#define x64_fist_membase_size(inst,basereg,disp,is_int,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_fist_membase((inst),((basereg)&0x7),(disp),(is_int)); x64_codegen_post(inst); } while (0)
//#define x64_push_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_push_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_push_regp_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_push_regp((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_push_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_push_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
//#define x64_push_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_push_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_push_memindex_size(inst,basereg,disp,indexreg,shift,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,(indexreg),(basereg)); x86_push_memindex((inst),((basereg)&0x7),(disp),((indexreg)&0x7),(shift)); x64_codegen_post(inst); } while (0)
#define x64_push_imm_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_push_imm((inst),(imm)); x64_codegen_post(inst); } while (0)
//#define x64_pop_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_pop_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_pop_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_pop_mem((inst),(mem)); x64_codegen_post(inst); } while (0)
#define x64_pop_membase_size(inst,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_pop_membase((inst),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_pushad_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_pushad(inst); x64_codegen_post(inst); } while (0)
#define x64_pushfd_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_pushfd(inst); x64_codegen_post(inst); } while (0)
#define x64_popad_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_popad(inst); x64_codegen_post(inst); } while (0)
#define x64_popfd_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_popfd(inst); x64_codegen_post(inst); } while (0)
#define x64_loop_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_loop((inst),(imm)); x64_codegen_post(inst); } while (0)
#define x64_loope_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_loope((inst),(imm)); x64_codegen_post(inst); } while (0)
#define x64_loopne_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_loopne((inst),(imm)); x64_codegen_post(inst); } while (0)
#define x64_jump32_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_jump32((inst),(imm)); x64_codegen_post(inst); } while (0)
#define x64_jump8_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_jump8((inst),(imm)); x64_codegen_post(inst); } while (0)

#if !defined( __native_client_codegen__ )
/* Defined above for Native Client, so they can be used in other macros */
#define x64_jump_reg_size(inst,reg,size) do { x64_emit_rex ((inst),0,0,0,(reg)); x86_jump_reg((inst),((reg)&0x7)); } while (0)
#define x64_jump_mem_size(inst,mem,size) do { x64_emit_rex ((inst),(size),0,0,0); x86_jump_mem((inst),(mem)); } while (0)
#endif

#define x64_jump_disp_size(inst,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,0); x86_jump_disp((inst),(disp)); x64_codegen_post(inst); } while (0)
#define x64_branch8_size(inst,cond,imm,is_signed,size) do { x86_branch8((inst),(cond),(imm),(is_signed)); } while (0)
#define x64_branch32_size(inst,cond,imm,is_signed,size) do { x86_branch32((inst),(cond),(imm),(is_signed)); } while (0)
#define x64_branch_size_body(inst,cond,target,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_branch((inst),(cond),(target),(is_signed)); x64_codegen_post(inst); } while (0)

#if defined(__native_client_codegen__)
#define x64_branch_size(inst,cond,target,is_signed,size) \
	do { \
		/* x64_branch_size_body used twice in     */ \
		/* case of relocation by x64_codegen_post */ \
		uint8_t* branch_start; \
		x64_codegen_pre(inst); \
		x64_branch_size_body((inst),(cond),(target),(is_signed),(size)); \
		inst = x64_codegen_post(inst); \
		branch_start = inst; \
		x64_branch_size_body((inst),(cond),(target),(is_signed),(size)); \
		mono_x64_patch(branch_start, (target)); \
	} while (0)
#else
#define x64_branch_size(inst,cond,target,is_signed,size) do { x64_branch_size_body((inst),(cond),(target),(is_signed),(size)); } while (0)
#endif /* __native_client_codegen__ */

#define x64_branch_disp_size(inst,cond,disp,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_branch_disp((inst),(cond),(disp),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_set_reg_size(inst,cond,reg,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex((inst),1,0,0,(reg)); x86_set_reg((inst),(cond),((reg)&0x7),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_set_mem_size(inst,cond,mem,is_signed,size) do { x64_codegen_pre(inst); x86_set_mem((inst),(cond),(mem),(is_signed)); x64_codegen_post(inst); } while (0)
#define x64_set_membase_size(inst,cond,basereg,disp,is_signed,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),0,0,0,(basereg)); x86_set_membase((inst),(cond),((basereg)&0x7),(disp),(is_signed)); x64_codegen_post(inst); } while (0)
//#define x64_call_reg_size(inst,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_call_reg((inst),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_call_mem_size(inst,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_call_mem((inst),(mem)); x64_codegen_post(inst); } while (0)

#if defined(__native_client_codegen__)
/* Size is ignored for Native Client calls, we restrict jumping to 32-bits */
#define x64_call_imm_size(inst,disp,size)             \
  do {                                                  \
    x64_codegen_pre((inst));                          \
    x64_call_sequence_pre((inst));                    \
    x86_call_imm((inst),(disp));                        \
    x64_call_sequence_post((inst));                   \
    x64_codegen_post((inst));                         \
  } while (0)

/* x86_call_code is called twice below, first so we can get the size of the */
/* call sequence, and again so the exact offset from "inst" is used, since  */
/* the sequence could have moved from x64_call_sequence_post.             */
/* Size is ignored for Native Client jumps, we restrict jumping to 32-bits  */
#define x64_call_code_size(inst,target,size)          \
  do {                                                  \
    x64_codegen_pre((inst));                          \
    uint8_t* adjusted_start;                             \
    uint8_t* call_start;                                 \
    x64_call_sequence_pre((inst));                    \
    x86_call_code((inst),(target));                     \
    adjusted_start = x64_call_sequence_post((inst));  \
    call_start = adjusted_start;                        \
    x86_call_code(adjusted_start, (target));            \
    x64_codegen_post((inst));                         \
    mono_x64_patch(call_start, (target));             \
  } while (0)

#else

#define x64_call_imm_size(inst,disp,size) do { x86_call_imm((inst),(disp)); } while (0)
#define x64_call_code_size(inst,target,size) do { x86_call_code((inst),(target)); } while (0)

#endif /*__native_client_codegen__*/

//#define x64_ret_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_ret(inst); x64_codegen_post(inst); } while (0)
#define x64_ret_imm_size(inst,imm,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_ret_imm((inst),(imm)); x64_codegen_post(inst); } while (0)
#define x64_cmov_reg_size(inst,cond,is_signed,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_cmov_reg((inst),(cond),(is_signed),((dreg)&0x7),((reg)&0x7)); x64_codegen_post(inst); } while (0)
#define x64_cmov_mem_size(inst,cond,is_signed,reg,mem,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_cmov_mem((inst),(cond),(is_signed),((reg)&0x7),(mem)); x64_codegen_post(inst); } while (0)
#define x64_cmov_membase_size(inst,cond,is_signed,reg,basereg,disp,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(basereg)); x86_cmov_membase((inst),(cond),(is_signed),((reg)&0x7),((basereg)&0x7),(disp)); x64_codegen_post(inst); } while (0)
#define x64_enter_size(inst,framesize) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_enter((inst),(framesize)); x64_codegen_post(inst); } while (0)
//#define x64_leave_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_leave(inst); x64_codegen_post(inst); } while (0)
#define x64_sahf_size(inst,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_sahf(inst); x64_codegen_post(inst); } while (0)
#define x64_fsin_size(inst,size) do { x64_codegen_pre(inst); x86_fsin(inst); x64_codegen_post(inst); } while (0)
#define x64_fcos_size(inst,size) do { x64_codegen_pre(inst); x86_fcos(inst); x64_codegen_post(inst); } while (0)
#define x64_fabs_size(inst,size) do { x64_codegen_pre(inst); x86_fabs(inst); x64_codegen_post(inst); } while (0)
#define x64_ftst_size(inst,size) do { x64_codegen_pre(inst); x86_ftst(inst); x64_codegen_post(inst); } while (0)
#define x64_fxam_size(inst,size) do { x64_codegen_pre(inst); x86_fxam(inst); x64_codegen_post(inst); } while (0)
#define x64_fpatan_size(inst,size) do { x64_codegen_pre(inst); x86_fpatan(inst); x64_codegen_post(inst); } while (0)
#define x64_fprem_size(inst,size) do { x64_codegen_pre(inst); x86_fprem(inst); x64_codegen_post(inst); } while (0)
#define x64_fprem1_size(inst,size) do { x64_codegen_pre(inst); x86_fprem1(inst); x64_codegen_post(inst); } while (0)
#define x64_frndint_size(inst,size) do { x64_codegen_pre(inst); x86_frndint(inst); x64_codegen_post(inst); } while (0)
#define x64_fsqrt_size(inst,size) do { x64_codegen_pre(inst); x86_fsqrt(inst); x64_codegen_post(inst); } while (0)
#define x64_fptan_size(inst,size) do { x64_codegen_pre(inst); x86_fptan(inst); x64_codegen_post(inst); } while (0)
//#define x64_padding_size(inst,size) do { x64_codegen_pre(inst); x86_padding((inst),(size)); x64_codegen_post(inst); } while (0)
#define x64_prolog_size(inst,frame_size,reg_mask,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_prolog((inst),(frame_size),(reg_mask)); x64_codegen_post(inst); } while (0)
#define x64_epilog_size(inst,reg_mask,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,0); x86_epilog((inst),(reg_mask)); x64_codegen_post(inst); } while (0)
#define x64_xadd_reg_reg_size(inst,dreg,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(dreg),0,(reg)); x86_xadd_reg_reg ((inst), (dreg), (reg), (size)); x64_codegen_post(inst); } while (0)
#define x64_xadd_mem_reg_size(inst,mem,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),0,0,(reg)); x86_xadd_mem_reg((inst),(mem),((reg)&0x7), (size)); x64_codegen_post(inst); } while (0)
#define x64_xadd_membase_reg_size(inst,basereg,disp,reg,size) do { x64_codegen_pre(inst); x64_emit_rex ((inst),(size),(reg),0,(basereg)); x86_xadd_membase_reg((inst),((basereg)&0x7),(disp),((reg)&0x7),(size)); x64_codegen_post(inst); } while (0)




#define x64_breakpoint(inst) x64_breakpoint_size(inst,8)
#define x64_cld(inst) x64_cld_size(inst,8)
#define x64_stosb(inst) x64_stosb_size(inst,8)
#define x64_stosl(inst) x64_stosl_size(inst,8)
#define x64_stosd(inst) x64_stosd_size(inst,8)
#define x64_movsb(inst) x64_movsb_size(inst,8)
#define x64_movsl(inst) x64_movsl_size(inst,8)
#define x64_movsd(inst) x64_movsd_size(inst,8)
#define x64_prefix(inst,p) x64_prefix_size(inst,p,8)
#define x64_rdtsc(inst) x64_rdtsc_size(inst,8)
#define x64_cmpxchg_reg_reg(inst,dreg,reg) x64_cmpxchg_reg_reg_size(inst,dreg,reg,8)
#define x64_cmpxchg_mem_reg(inst,mem,reg) x64_cmpxchg_mem_reg_size(inst,mem,reg,8)
#define x64_cmpxchg_membase_reg(inst,basereg,disp,reg) x64_cmpxchg_membase_reg_size(inst,basereg,disp,reg,8)
#define x64_xchg_reg_reg(inst,dreg,reg,size) x64_xchg_reg_reg_size(inst,dreg,reg,size)
#define x64_xchg_mem_reg(inst,mem,reg,size) x64_xchg_mem_reg_size(inst,mem,reg,size)
#define x64_xchg_membase_reg(inst,basereg,disp,reg,size) x64_xchg_membase_reg_size(inst,basereg,disp,reg,size)
#define x64_xadd_reg_reg(inst,dreg,reg,size) x64_xadd_reg_reg_size(inst,dreg,reg,size)
#define x64_xadd_mem_reg(inst,mem,reg,size) x64_xadd_mem_reg_size(inst,mem,reg,size)
#define x64_xadd_membase_reg(inst,basereg,disp,reg,size) x64_xadd_membase_reg_size(inst,basereg,disp,reg,size)
#define x64_inc_mem(inst,mem) x64_inc_mem_size(inst,mem,8)
#define x64_inc_membase(inst,basereg,disp) x64_inc_membase_size(inst,basereg,disp,8)
#define x64_inc_reg(inst,reg) x64_inc_reg_size(inst,reg,8)
#define x64_dec_mem(inst,mem) x64_dec_mem_size(inst,mem,8)
#define x64_dec_membase(inst,basereg,disp) x64_dec_membase_size(inst,basereg,disp,8)
#define x64_dec_reg(inst,reg) x64_dec_reg_size(inst,reg,8)
#define x64_not_mem(inst,mem) x64_not_mem_size(inst,mem,8)
#define x64_not_membase(inst,basereg,disp) x64_not_membase_size(inst,basereg,disp,8)
#define x64_not_reg(inst,reg) x64_not_reg_size(inst,reg,8)
#define x64_neg_mem(inst,mem) x64_neg_mem_size(inst,mem,8)
#define x64_neg_membase(inst,basereg,disp) x64_neg_membase_size(inst,basereg,disp,8)
#define x64_neg_reg(inst,reg) x64_neg_reg_size(inst,reg,8)
#define x64_nop(inst) x64_nop_size(inst,8)
//#define x64_alu_reg_imm(inst,opc,reg,imm) x64_alu_reg_imm_size(inst,opc,reg,imm,8)
#define x64_alu_mem_imm(inst,opc,mem,imm) x64_alu_mem_imm_size(inst,opc,mem,imm,8)
#define x64_alu_membase_imm(inst,opc,basereg,disp,imm) x64_alu_membase_imm_size(inst,opc,basereg,disp,imm,8)
#define x64_alu_mem_reg(inst,opc,mem,reg) x64_alu_mem_reg_size(inst,opc,mem,reg,8)
#define x64_alu_membase_reg(inst,opc,basereg,disp,reg) x64_alu_membase_reg_size(inst,opc,basereg,disp,reg,8)
//#define x64_alu_reg_reg(inst,opc,dreg,reg) x64_alu_reg_reg_size(inst,opc,dreg,reg,8)
#define x64_alu_reg8_reg8(inst,opc,dreg,reg,is_dreg_h,is_reg_h) x64_alu_reg8_reg8_size(inst,opc,dreg,reg,is_dreg_h,is_reg_h,8)
#define x64_alu_reg_mem(inst,opc,reg,mem) x64_alu_reg_mem_size(inst,opc,reg,mem,8)
#define x64_alu_reg_membase(inst,opc,reg,basereg,disp) x64_alu_reg_membase_size(inst,opc,reg,basereg,disp,8)
#define x64_test_reg_imm(inst,reg,imm) x64_test_reg_imm_size(inst,reg,imm,8)
#define x64_test_mem_imm(inst,mem,imm) x64_test_mem_imm_size(inst,mem,imm,8)
#define x64_test_membase_imm(inst,basereg,disp,imm) x64_test_membase_imm_size(inst,basereg,disp,imm,8)
#define x64_test_reg_reg(inst,dreg,reg) x64_test_reg_reg_size(inst,dreg,reg,8)
#define x64_test_mem_reg(inst,mem,reg) x64_test_mem_reg_size(inst,mem,reg,8)
#define x64_test_membase_reg(inst,basereg,disp,reg) x64_test_membase_reg_size(inst,basereg,disp,reg,8)
#define x64_shift_reg_imm(inst,opc,reg,imm) x64_shift_reg_imm_size(inst,opc,reg,imm,8)
#define x64_shift_mem_imm(inst,opc,mem,imm) x64_shift_mem_imm_size(inst,opc,mem,imm,8)
#define x64_shift_membase_imm(inst,opc,basereg,disp,imm) x64_shift_membase_imm_size(inst,opc,basereg,disp,imm,8)
#define x64_shift_reg(inst,opc,reg) x64_shift_reg_size(inst,opc,reg,8)
#define x64_shift_mem(inst,opc,mem) x64_shift_mem_size(inst,opc,mem,8)
#define x64_shift_membase(inst,opc,basereg,disp) x64_shift_membase_size(inst,opc,basereg,disp,8)
#define x64_shrd_reg(inst,dreg,reg) x64_shrd_reg_size(inst,dreg,reg,8)
#define x64_shrd_reg_imm(inst,dreg,reg,shamt) x64_shrd_reg_imm_size(inst,dreg,reg,shamt,8)
#define x64_shld_reg(inst,dreg,reg) x64_shld_reg_size(inst,dreg,reg,8)
#define x64_shld_reg_imm(inst,dreg,reg,shamt) x64_shld_reg_imm_size(inst,dreg,reg,shamt,8)
#define x64_mul_reg(inst,reg,is_signed) x64_mul_reg_size(inst,reg,is_signed,8)
#define x64_mul_mem(inst,mem,is_signed) x64_mul_mem_size(inst,mem,is_signed,8)
#define x64_mul_membase(inst,basereg,disp,is_signed) x64_mul_membase_size(inst,basereg,disp,is_signed,8)
#define x64_imul_reg_reg(inst,dreg,reg) x64_imul_reg_reg_size(inst,dreg,reg,8)
#define x64_imul_reg_mem(inst,reg,mem) x64_imul_reg_mem_size(inst,reg,mem,8)
#define x64_imul_reg_membase(inst,reg,basereg,disp) x64_imul_reg_membase_size(inst,reg,basereg,disp,8)
#define x64_imul_reg_reg_imm(inst,dreg,reg,imm) x64_imul_reg_reg_imm_size(inst,dreg,reg,imm,8)
#define x64_imul_reg_mem_imm(inst,reg,mem,imm) x64_imul_reg_mem_imm_size(inst,reg,mem,imm,8)
#define x64_imul_reg_membase_imm(inst,reg,basereg,disp,imm) x64_imul_reg_membase_imm_size(inst,reg,basereg,disp,imm,8)
#define x64_div_reg(inst,reg,is_signed) x64_div_reg_size(inst,reg,is_signed,8)
#define x64_div_mem(inst,mem,is_signed) x64_div_mem_size(inst,mem,is_signed,8)
#define x64_div_membase(inst,basereg,disp,is_signed) x64_div_membase_size(inst,basereg,disp,is_signed,8)
//#define x64_mov_mem_reg(inst,mem,reg,size) x64_mov_mem_reg_size(inst,mem,reg,size)
//#define x64_mov_regp_reg(inst,regp,reg,size) x64_mov_regp_reg_size(inst,regp,reg,size)
//#define x64_mov_membase_reg(inst,basereg,disp,reg,size) x64_mov_membase_reg_size(inst,basereg,disp,reg,size)
#define x64_mov_memindex_reg(inst,basereg,disp,indexreg,shift,reg,size) x64_mov_memindex_reg_size(inst,basereg,disp,indexreg,shift,reg,size)
//#define x64_mov_reg_reg(inst,dreg,reg,size) x64_mov_reg_reg_size(inst,dreg,reg,size)
//#define x64_mov_reg_mem(inst,reg,mem,size) x64_mov_reg_mem_size(inst,reg,mem,size)
//#define x64_mov_reg_membase(inst,reg,basereg,disp,size) x64_mov_reg_membase_size(inst,reg,basereg,disp,size)
#define x64_mov_reg_memindex(inst,reg,basereg,disp,indexreg,shift,size) x64_mov_reg_memindex_size(inst,reg,basereg,disp,indexreg,shift,size)
#define x64_clear_reg(inst,reg) x64_clear_reg_size(inst,reg,8)
//#define x64_mov_reg_imm(inst,reg,imm) x64_mov_reg_imm_size(inst,reg,imm,8)
#define x64_mov_mem_imm(inst,mem,imm,size) x64_mov_mem_imm_size(inst,mem,imm,size)
//#define x64_mov_membase_imm(inst,basereg,disp,imm,size) x64_mov_membase_imm_size(inst,basereg,disp,imm,size)
#define x64_mov_memindex_imm(inst,basereg,disp,indexreg,shift,imm,size) x64_mov_memindex_imm_size(inst,basereg,disp,indexreg,shift,imm,size)
#define x64_lea_mem(inst,reg,mem) x64_lea_mem_size(inst,reg,mem,8)
//#define x64_lea_membase(inst,reg,basereg,disp) x64_lea_membase_size(inst,reg,basereg,disp,8)
#define x64_lea_memindex(inst,reg,basereg,disp,indexreg,shift) x64_lea_memindex_size(inst,reg,basereg,disp,indexreg,shift,8)
#define x64_widen_reg(inst,dreg,reg,is_signed,is_half) x64_widen_reg_size(inst,dreg,reg,is_signed,is_half,8)
#define x64_widen_mem(inst,dreg,mem,is_signed,is_half) x64_widen_mem_size(inst,dreg,mem,is_signed,is_half,8)
#define x64_widen_membase(inst,dreg,basereg,disp,is_signed,is_half) x64_widen_membase_size(inst,dreg,basereg,disp,is_signed,is_half,8)
#define x64_widen_memindex(inst,dreg,basereg,disp,indexreg,shift,is_signed,is_half) x64_widen_memindex_size(inst,dreg,basereg,disp,indexreg,shift,is_signed,is_half,8)
#define x64_cdq(inst) x64_cdq_size(inst,8)
#define x64_wait(inst) x64_wait_size(inst,8)
#define x64_fp_op_mem(inst,opc,mem,is_double) x64_fp_op_mem_size(inst,opc,mem,is_double,8)
#define x64_fp_op_membase(inst,opc,basereg,disp,is_double) x64_fp_op_membase_size(inst,opc,basereg,disp,is_double,8)
#define x64_fp_op(inst,opc,index) x64_fp_op_size(inst,opc,index,8)
#define x64_fp_op_reg(inst,opc,index,pop_stack) x64_fp_op_reg_size(inst,opc,index,pop_stack,8)
#define x64_fp_int_op_membase(inst,opc,basereg,disp,is_int) x64_fp_int_op_membase_size(inst,opc,basereg,disp,is_int,8)
#define x64_fstp(inst,index) x64_fstp_size(inst,index,8)
#define x64_fcompp(inst) x64_fcompp_size(inst,8)
#define x64_fucompp(inst) x64_fucompp_size(inst,8)
#define x64_fnstsw(inst) x64_fnstsw_size(inst,8)
#define x64_fnstcw(inst,mem) x64_fnstcw_size(inst,mem,8)
#define x64_fnstcw_membase(inst,basereg,disp) x64_fnstcw_membase_size(inst,basereg,disp,8)
#define x64_fldcw(inst,mem) x64_fldcw_size(inst,mem,8)
#define x64_fldcw_membase(inst,basereg,disp) x64_fldcw_membase_size(inst,basereg,disp,8)
#define x64_fchs(inst) x64_fchs_size(inst,8)
#define x64_frem(inst) x64_frem_size(inst,8)
#define x64_fxch(inst,index) x64_fxch_size(inst,index,8)
#define x64_fcomi(inst,index) x64_fcomi_size(inst,index,8)
#define x64_fcomip(inst,index) x64_fcomip_size(inst,index,8)
#define x64_fucomi(inst,index) x64_fucomi_size(inst,index,8)
#define x64_fucomip(inst,index) x64_fucomip_size(inst,index,8)
#define x64_fld(inst,mem,is_double) x64_fld_size(inst,mem,is_double,8)
#define x64_fld_membase(inst,basereg,disp,is_double)  x64_fld_membase_size(inst,basereg,disp,is_double,8) 
#define x64_fld80_mem(inst,mem) x64_fld80_mem_size(inst,mem,8)
#define x64_fld80_membase(inst,basereg,disp) x64_fld80_membase_size(inst,basereg,disp,8)
#define x64_fild(inst,mem,is_long) x64_fild_size(inst,mem,is_long,8)
#define x64_fild_membase(inst,basereg,disp,is_long) x64_fild_membase_size(inst,basereg,disp,is_long,8)
#define x64_fld_reg(inst,index) x64_fld_reg_size(inst,index,8)
#define x64_fldz(inst) x64_fldz_size(inst,8)
#define x64_fld1(inst) x64_fld1_size(inst,8)
#define x64_fldpi(inst) x64_fldpi_size(inst,8)
#define x64_fst(inst,mem,is_double,pop_stack) x64_fst_size(inst,mem,is_double,pop_stack,8)
#define x64_fst_membase(inst,basereg,disp,is_double,pop_stack) x64_fst_membase_size(inst,basereg,disp,is_double,pop_stack,8)
#define x64_fst80_mem(inst,mem) x64_fst80_mem_size(inst,mem,8)
#define x64_fst80_membase(inst,basereg,disp) x64_fst80_membase_size(inst,basereg,disp,8)
#define x64_fist_pop(inst,mem,is_long) x64_fist_pop_size(inst,mem,is_long,8)
#define x64_fist_pop_membase(inst,basereg,disp,is_long) x64_fist_pop_membase_size(inst,basereg,disp,is_long,8)
#define x64_fstsw(inst) x64_fstsw_size(inst,8)
#define x64_fist_membase(inst,basereg,disp,is_int) x64_fist_membase_size(inst,basereg,disp,is_int,8)
//#define x64_push_reg(inst,reg) x64_push_reg_size(inst,reg,8)
#define x64_push_regp(inst,reg) x64_push_regp_size(inst,reg,8)
#define x64_push_mem(inst,mem) x64_push_mem_size(inst,mem,8)
//#define x64_push_membase(inst,basereg,disp) x64_push_membase_size(inst,basereg,disp,8)
#define x64_push_memindex(inst,basereg,disp,indexreg,shift) x64_push_memindex_size(inst,basereg,disp,indexreg,shift,8)
#define x64_push_imm(inst,imm) x64_push_imm_size(inst,imm,8)
//#define x64_pop_reg(inst,reg) x64_pop_reg_size(inst,reg,8)
#define x64_pop_mem(inst,mem) x64_pop_mem_size(inst,mem,8)
#define x64_pop_membase(inst,basereg,disp) x64_pop_membase_size(inst,basereg,disp,8)
#define x64_pushad(inst) x64_pushad_size(inst,8)
#define x64_pushfd(inst) x64_pushfd_size(inst,8)
#define x64_popad(inst) x64_popad_size(inst,8)
#define x64_popfd(inst) x64_popfd_size(inst,8)
#define x64_loop(inst,imm) x64_loop_size(inst,imm,8)
#define x64_loope(inst,imm) x64_loope_size(inst,imm,8)
#define x64_loopne(inst,imm) x64_loopne_size(inst,imm,8)
#define x64_jump32(inst,imm) x64_jump32_size(inst,imm,8)
#define x64_jump8(inst,imm) x64_jump8_size(inst,imm,8)
#define x64_jump_reg(inst,reg) x64_jump_reg_size(inst,reg,8)
#define x64_jump_mem(inst,mem) x64_jump_mem_size(inst,mem,8)
#define x64_jump_membase(inst,basereg,disp) x64_jump_membase_size(inst,basereg,disp,8)
#define x64_jump_code(inst,target) x64_jump_code_size(inst,target,8)
#define x64_jump_disp(inst,disp) x64_jump_disp_size(inst,disp,8)
#define x64_branch8(inst,cond,imm,is_signed) x64_branch8_size(inst,cond,imm,is_signed,8)
#define x64_branch32(inst,cond,imm,is_signed) x64_branch32_size(inst,cond,imm,is_signed,8)
#define x64_branch(inst,cond,target,is_signed) x64_branch_size(inst,cond,target,is_signed,8)
#define x64_branch_disp(inst,cond,disp,is_signed) x64_branch_disp_size(inst,cond,disp,is_signed,8)
#define x64_set_reg(inst,cond,reg,is_signed) x64_set_reg_size(inst,cond,reg,is_signed,8)
#define x64_set_mem(inst,cond,mem,is_signed) x64_set_mem_size(inst,cond,mem,is_signed,8)
#define x64_set_membase(inst,cond,basereg,disp,is_signed) x64_set_membase_size(inst,cond,basereg,disp,is_signed,8)
#define x64_call_imm(inst,disp) x64_call_imm_size(inst,disp,8)
//#define x64_call_reg(inst,reg) x64_call_reg_size(inst,reg,8)
#define x64_call_mem(inst,mem) x64_call_mem_size(inst,mem,8)
#define x64_call_membase(inst,basereg,disp) x64_call_membase_size(inst,basereg,disp,8)
#define x64_call_code(inst,target) x64_call_code_size(inst,target,8)
//#define x64_ret(inst) x64_ret_size(inst,8)
#define x64_ret_imm(inst,imm) x64_ret_imm_size(inst,imm,8)
#define x64_cmov_reg(inst,cond,is_signed,dreg,reg) x64_cmov_reg_size(inst,cond,is_signed,dreg,reg,8)
#define x64_cmov_mem(inst,cond,is_signed,reg,mem) x64_cmov_mem_size(inst,cond,is_signed,reg,mem,8)
#define x64_cmov_membase(inst,cond,is_signed,reg,basereg,disp) x64_cmov_membase_size(inst,cond,is_signed,reg,basereg,disp,8)
#define x64_enter(inst,framesize) x64_enter_size(inst,framesize)
//#define x64_leave(inst) x64_leave_size(inst,8)
#define x64_sahf(inst) x64_sahf_size(inst,8)
#define x64_fsin(inst) x64_fsin_size(inst,8)
#define x64_fcos(inst) x64_fcos_size(inst,8)
#define x64_fabs(inst) x64_fabs_size(inst,8)
#define x64_ftst(inst) x64_ftst_size(inst,8)
#define x64_fxam(inst) x64_fxam_size(inst,8)
#define x64_fpatan(inst) x64_fpatan_size(inst,8)
#define x64_fprem(inst) x64_fprem_size(inst,8)
#define x64_fprem1(inst) x64_fprem1_size(inst,8)
#define x64_frndint(inst) x64_frndint_size(inst,8)
#define x64_fsqrt(inst) x64_fsqrt_size(inst,8)
#define x64_fptan(inst) x64_fptan_size(inst,8)
#define x64_padding(inst,size) x64_padding_size(inst,size)
#define x64_prolog(inst,frame,reg_mask) x64_prolog_size(inst,frame,reg_mask,8)
#define x64_epilog(inst,reg_mask) x64_epilog_size(inst,reg_mask,8)

#endif // X64_H
