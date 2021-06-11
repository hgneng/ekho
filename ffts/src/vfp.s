/*
 
 This file is part of FFTS -- The Fastest Fourier Transform in the South
  
 Copyright (c) 2012, 2013 Anthony M. Blake <amb@anthonix.com>
 Copyright (c) 2012, 2013 The University of Waikato 
 
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
	.fpu	vfp

@ assumes r0 = out 
@         r1 = in ? 
@
@         r12 = offsets
@         r3-r10 = data pointers
@         r11 = loop iterations
@         r2 = const pointer
@       & lr = temps

	.align	4
#ifdef __APPLE__
	.globl	_vfp_e
_vfp_e:
#else
	.globl	vfp_e
vfp_e:
#endif
_vfp_e_loop:
	vldr		s15, [r2, #8]
	vldr		s2, [r3] @ x0
	vldr		s0, [r3, #4]
	vldr		s4, [r4] @ x1
	vldr		s11, [r2]
	vldr		s10, [r7] @ x4
	vldr		s3, [r7, #4]
	vldr		s8, [r8] @ x5
	vldr		s1, [r8, #4]
	vldr		s14, [r9] @ x6
	vldr		s9, [r9, #4]
	vldr		s6, [r10] @ x7
	vldr		s12, [r10, #4]
	vsub.f32	s18, s3, s1
	vsub.f32	s7, s10, s8
	vsub.f32	s5, s14, s6
	vadd.f32	s6, s14, s6
	vldr		s24, [r5, #4]
	vsub.f32	s14, s9, s12
	vldr		s22, [r6, #4]
	vadd.f32	s8, s10, s8
	vldr		s28, [r6] @ x3
	vldr		s17, [r5] @ x2
	vadd.f32	s10, s9, s12
	vmul.f32	s13, s18, s15
	vmul.f32	s9, s7, s11
	vmul.f32	s16, s5, s11
	vmul.f32	s18, s18, s11
	vmul.f32	s30, s14, s11
	vldr		s11, [r4, #4]
	add			r3, r3, #8
	add			r4, r4, #8
	add			r5, r5, #8
	add			r6, r6, #8
	add			r7, r7, #8
	add			r8, r8, #8
	add			r9, r9, #8
	add			r10, r10, #8
	vmul.f32	s12, s5, s15
	vmul.f32	s20, s14, s15
	vadd.f32	s5, s2, s4
	vadd.f32	s3, s3, s1
	vmul.f32	s15, s7, s15
	vadd.f32	s1, s24, s22
	vsub.f32	s7, s24, s22
	vadd.f32	s24, s17, s28
	vadd.f32	s26, s0, s11
	vsub.f32	s14, s9, s13
	vsub.f32	s2, s2, s4
	vadd.f32	s4, s16, s20
	vsub.f32	s22, s0, s11
	vsub.f32	s16, s17, s28
	vadd.f32	s9, s5, s24
	vadd.f32	s28, s18, s15
	vadd.f32	s13, s8, s6
	vsub.f32	s5, s5, s24
	vsub.f32	s24, s8, s6
	vadd.f32	s11, s26, s1
	vsub.f32	s12, s30, s12
	vadd.f32	s20, s3, s10
	vsub.f32	s15, s3, s10
	vsub.f32	s3, s26, s1
	vadd.f32	s18, s9, s13
	vadd.f32	s10, s14, s4
	vadd.f32	s6, s2, s7      @
	vsub.f32	s0, s2, s7      @
	vadd.f32	s26, s11, s20
	vsub.f32	s4, s14, s4
	vsub.f32	s8, s22, s16    @
	vadd.f32	s1, s28, s12
	ldr			lr, [r12], #4
	add			lr, r0, lr, lsl #2
	subs		r11, r11, #1
	vstr		s18, [lr]
	vsub.f32	s2, s28, s12
	vadd.f32	s12, s22, s16   @
	vsub.f32	s16, s3, s24    @
	vsub.f32	s13, s9, s13
	vstr		s26, [lr, #4]
	vadd.f32	s28, s5, s15    @
	vsub.f32	s7, s5, s15     @
	vadd.f32	s14, s6, s10
	vadd.f32	s5, s8, s1      
	vadd.f32	s9, s0, s2      @
	vsub.f32	s2, s0, s2      @
	vsub.f32	s11, s11, s20
	vstr		s28, [lr, #16]
	vadd.f32	s3, s3, s24     @
	vstr		s16, [lr, #20]
	vsub.f32	s6, s6, s10
	vstr		s13, [lr, #32]
	vsub.f32	s13, s12, s4    @
	vsub.f32	s8, s8, s1
	vadd.f32	s0, s12, s4     @
	vstr		s11, [lr, #36]
	vstr		s7, [lr, #48]
	vstr		s3, [lr, #52]
	vstr		s14, [lr, #8]
	vstr		s5, [lr, #12]
	vstr		s9, [lr, #24]
	vstr		s13, [lr, #28]
	vstr		s6, [lr, #40]
	vstr		s8, [lr, #44]
	vstr		s2, [lr, #56]
	vstr		s0, [lr, #60]
	bne			_vfp_e_loop

@ assumes r0 = out 
@         r1 = in ? 
@
@         r12 = offsets
@         r3-r10 = data pointers
@         r11 = loop iterations
@         r2 & lr = temps
	.align 4
#ifdef __APPLE__
	.globl	_vfp_o
_vfp_o:
#else
	.globl	vfp_o
vfp_o:
#endif
 _vfp_o_loop: 
	vldr s4, [r3] @ x0 
	vldr	s0, [r3, #4]
	vldr s6, [r4] @ x1 
	vldr	s5, [r4, #4]
	vldr s7, [r5] @ x2 
	vldr	s1, [r5, #4]
	vldr s3, [r6] @ x3 
	vldr	s8, [r6, #4]
   subs r11, r11, #1   
	 ldr r2, [r12], #4
	 add r2, r0, r2, lsl #2
	vadd.f32	s2, s4, s6
	vadd.f32	s14, s0, s5
	vadd.f32	s10, s1, s8
	vsub.f32	s4, s4, s6
	vsub.f32	s0, s0, s5
	vadd.f32	s12, s7, s3
	vsub.f32	s6, s7, s3
	vsub.f32	s8, s1, s8
	vadd.f32	s5, s14, s10
	vsub.f32	s10, s14, s10
	vadd.f32	s7, s2, s12
	vsub.f32	s1, s0, s6     @
	vsub.f32	s12, s2, s12
	vadd.f32	s3, s4, s8     @
	vsub.f32	s2, s4, s8     @
	vadd.f32	s0, s0, s6     @
	vstr	s7, [r2]
	vldr s7, [r9] @ x2 
	vstr	s5, [r2, #4]
	vstr	s3, [r2, #8]
	vstr	s1, [r2, #12]
	vstr	s12, [r2, #16]
	vstr	s10, [r2, #20]
	vstr	s2, [r2, #24]
	vstr	s0, [r2, #28]
	vldr s4, [r7] @ x0 
	vldr	s0, [r7, #4]
	vldr s6, [r8] @ x1 
	vldr	s5, [r8, #4]
	vldr s3, [r10] @ x3 
	vldr	s8, [r10, #4]
	vldr	s1, [r9, #4]
    add r3, r3, #8
    add r4, r4, #8
    add r5, r5, #8
    add r6, r6, #8
    add r7, r7, #8
    add r8, r8, #8
    add r9, r9, #8
    add r10, r10, #8
	vadd.f32	s2, s4, s6
	vadd.f32	s14, s0, s5
	vadd.f32	s10, s1, s8
	vsub.f32	s4, s4, s6
	vsub.f32	s0, s0, s5
	vadd.f32	s12, s7, s3
	vsub.f32	s6, s7, s3
	vsub.f32	s8, s1, s8
	vadd.f32	s5, s14, s10
	vsub.f32	s10, s14, s10
	vadd.f32	s7, s2, s12
	vsub.f32	s1, s0, s6     @
	vsub.f32	s12, s2, s12
	vadd.f32	s3, s4, s8     @
	vsub.f32	s2, s4, s8     @
	vadd.f32	s0, s0, s6     @
	vstr	s7, [r2, #32]
	vstr	s5, [r2, #36]
	vstr	s3, [r2, #40]
	vstr	s1, [r2, #44]
	vstr	s12, [r2, #48]
	vstr	s10, [r2, #52]
	vstr	s2, [r2, #56]
	vstr	s0, [r2, #60]
    bne _vfp_o_loop
	
	.align	4
#ifdef __APPLE__
	.globl	_vfp_x4
_vfp_x4:
#else
	.globl	vfp_x4
vfp_x4:
#endif
	add r3, r0, #0
	add r7, r2, #0
	add r4, r0, r1, lsl #1
	add r5, r0, r1, lsl #2
	add r6, r4, r1, lsl #2
	mov r11, #4	
_vfp_x4_loop:

	vldr	s8, [r3, #0]
	vldr	s9, [r3, #4]
	vldr	s10, [r4, #0]
	vldr	s11, [r4, #4]
	vldr	s12, [r5, #0]
	vldr	s13, [r5, #4]
	vldr	s14, [r6, #0]
	vldr	s15, [r6, #4]
	vldr	s2, [r7, #0]
	vldr	s3, [r7, #4]
    add r7, r7, #8	
   subs r11, r11, #1   
	vmul.f32	s0, s13, s3
	vmul.f32	s5, s12, s2
	vmul.f32	s1, s14, s2
	vmul.f32	s4, s14, s3
	vmul.f32	s14, s12, s3
	vmul.f32	s13, s13, s2
	vmul.f32	s12, s15, s3
	vmul.f32	s2, s15, s2
	vsub.f32	s0, s5, s0
	vadd.f32	s13, s13, s14
	vadd.f32	s12, s12, s1
	vsub.f32	s1, s2, s4
	vadd.f32	s15, s0, s12
	vsub.f32	s12, s0, s12
	vadd.f32	s14, s13, s1
	vsub.f32	s13, s13, s1
	vadd.f32	s0, s8, s15
	vadd.f32	s1, s9, s14
	vadd.f32	s2, s10, s13  @
	vsub.f32	s4, s8, s15
	vsub.f32	s3, s11, s12  @
	vstr	s0, [r3, #0]
	vstr	s1, [r3, #4]
	 add r3, r3, #8 
	vsub.f32	s5, s9, s14
	vsub.f32	s6, s10, s13  @
	vadd.f32	s7, s11, s12  @
	vstr	s2, [r4, #0]
	vstr	s3, [r4, #4]
	 add r4, r4, #8 
	vstr	s4, [r5, #0]
	vstr	s5, [r5, #4]
	 add r5, r5, #8 
	vstr	s6, [r6, #0]
	vstr	s7, [r6, #4]
	 add r6, r6, #8 
	bne _vfp_x4_loop	
	bx lr
	
	.align 4
#ifdef __APPLE__
	.globl	_vfp_x8
_vfp_x8:
#else
	.globl	vfp_x8
vfp_x8:
#endif
	mov r11, #0
	add r3, r0, #0           @ data0
	add r5, r0, r1, lsl #1   @ data2
	add r4, r0, r1           @ data1
	add r7, r5, r1, lsl #1   @ data4
	add r6, r5, r1           @ data3
	add r9, r7, r1, lsl #1   @ data6
	add r8, r7, r1           @ data5
	add r10, r9, r1          @ data7
	add r12, r2, #0          @ LUT

	sub r11, r11, r1, lsr #3
_vfp_x8_loop: 
	vldr s10, [r3, #0] @ x0-re 
	vldr s8, [r3, #4] @ x0-im 
	vldr s2, [r4, #0] @ x1-re 
	vldr s0, [r4, #4] @ x1-im 
	vldr s6, [r5, #0] @ x2-re 
	vldr s4, [r5, #4] @ x2-im 
	vldr s13, [r6, #0] @ x3-re 
	vldr s15, [r6, #4] @ x3-im 
	vldr	s7, [r12]
	vldr	s11, [r12, #4]
	vldr s5, [r7, #0] @ x4-re 
	vldr s1, [r7, #4] @ x4-im 
	vldr s28, [r9, #0] @ x6-re 
	vldr s18, [r9, #4] @ x6-im 
    adds	r11, r11, #1
	vmul.f32	s14, s15, s7
	vldr	s24, [r12, #12]
	vmul.f32	s12, s13, s11
	vmul.f32	s26, s13, s7
	vldr	s13, [r12, #8]
	vmul.f32	s3, s4, s11
	vmul.f32	s15, s15, s11
	vmul.f32	s16, s4, s7
	vmul.f32	s9, s6, s7
	vmul.f32	s11, s6, s11
	vmul.f32	s7, s18, s24
	vmul.f32	s20, s1, s24
	vmul.f32	s30, s5, s13
	vadd.f32	s4, s26, s15
	vsub.f32	s12, s14, s12
	vsub.f32	s6, s9, s3
	vadd.f32	s14, s16, s11
	vmul.f32	s22, s28, s13
	vmul.f32	s26, s28, s24
	vmul.f32	s18, s18, s13
	vmul.f32	s5, s5, s24
	vmul.f32	s1, s1, s13
	vsub.f32	s9, s30, s20
	vadd.f32	s16, s14, s12
	vadd.f32	s3, s22, s7
	vadd.f32	s15, s6, s4
	vsub.f32	s11, s18, s26
	vadd.f32	s18, s1, s5
	vadd.f32	s13, s8, s16
	vadd.f32	s1, s9, s3
	vadd.f32	s7, s10, s15
	vsub.f32	s15, s10, s15
	vsub.f32	s10, s9, s3
	vadd.f32	s5, s18, s11
	vsub.f32	s11, s18, s11
	vsub.f32	s8, s8, s16
	vadd.f32	s20, s7, s1
	vsub.f32	s7, s7, s1
	vadd.f32	s18, s13, s5
	vadd.f32	s16, s15, s11   @
	vsub.f32	s9, s8, s10     @
	vsub.f32	s3, s13, s5
	vsub.f32	s1, s15, s11    @
	vstr	s20, [r3]
	vadd.f32	s8, s8, s10     @
	vstr	s18, [r3, #4]
	add r3, r3, #8
	vstr	s16, [r5]
	vstr	s9, [r5, #4]
	add r5, r5, #8
	vstr	s7, [r7]
	vstr	s3, [r7, #4]
	add r7, r7, #8
	vstr	s1, [r9]
	vstr	s8, [r9, #4]
	add r9, r9, #8
	vldr s10, [r8, #0] @ x5-re 
	vldr s8, [r8, #4] @ x5-im 
	vldr s5, [r10, #0] @ x7-re 
	vldr s11, [r10, #4] @ x7-im 
	vldr	s1, [r12, #16]
	vldr	s15, [r12, #20]
	add	r12, r12, #24
	vmul.f32	s9, s5, s1
	vmul.f32	s3, s11, s15
	vmul.f32	s13, s10, s1
	vmul.f32	s7, s8, s15
	vmul.f32	s5, s5, s15
	vmul.f32	s11, s11, s1
	vmul.f32	s10, s10, s15
	vmul.f32	s15, s8, s1
	vsub.f32	s1, s14, s12
	vadd.f32	s8, s9, s3
	vsub.f32	s3, s6, s4
	vsub.f32	s12, s13, s7
	vsub.f32	s5, s11, s5
	vadd.f32	s7, s15, s10
	vadd.f32	s4, s2, s1    @
	vsub.f32	s2, s2, s1    @
	vsub.f32	s6, s0, s3    @
	vadd.f32	s10, s12, s8
	vsub.f32	s9, s12, s8
	vadd.f32	s0, s0, s3    @ 
	vsub.f32	s1, s7, s5
	vadd.f32	s14, s7, s5
	vadd.f32	s7, s4, s10
	vsub.f32	s8, s4, s10
	vsub.f32	s12, s0, s9   @
	vadd.f32	s3, s2, s1    @
	vadd.f32	s5, s6, s14
	vsub.f32	s4, s6, s14
	vsub.f32	s2, s2, s1    @
	vadd.f32	s0, s0, s9    @
	vstr	s7, [r4]
	vstr	s5, [r4, #4]
	add r4, r4, #8
	vstr	s3, [r6]
	vstr	s12, [r6, #4]
	add r6, r6, #8
	vstr	s8, [r8]
	vstr	s4, [r8, #4]
	add r8, r8, #8
	vstr	s2, [r10]
	vstr	s0, [r10, #4]
	add r10, r10, #8
	bne _vfp_x8_loop	
	bx lr
	
	.align 4
#ifdef __APPLE__
	.globl	_vfp_end
_vfp_end:
#else
	.globl	vfp_end
vfp_end:
#endif
	bx lr
