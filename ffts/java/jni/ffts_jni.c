/*
 * This file is part of FFTS -- The Fastest Fourier Transform in the South
 *
 * Copyright (c) 2013, Michael Zucchi <notzed@gmail.com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the organization nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL ANTHONY M. BLAKE BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"

#include <ffts.h>
#include <alloca.h>

// Bit of a hack for android, as we can't build the *.h without
// the classes ... but we can't build the project without the jni.
#ifdef ANDROID
#include <jni.h>
#define NEEDS_ALIGNED
#undef HAVE_DECL_POSIX_MEMALIGN
#else
#include "nz_ac_waikato_ffts_FFTS.h"
#endif

// TODO: feature tests instead
#ifdef HAVE_SSE
#define NEEDS_ALIGNED
#endif

#ifdef NEEDS_ALIGNED
#define ALIGN_MASK 15

static void *
xmemalign(size_t align, size_t size) {
#if defined(HAVE_DECL_POSIX_MEMALIGN)
	void *r;

	if (posix_memalign(&r, align, size) != 0)
		return NULL;
	return r;	
#elif defined(HAVE_DECL_MEMALIGN)
	return memalign(align, size);
#else
#error "Require an aligning malloc"
#endif
}
#endif

static void
throwOutOfMemoryError(JNIEnv *env, const char *msg) {
	jclass jc = (*env)->FindClass(env, "java/lang/OutOfMemoryError");

	if (jc)
		(*env)->ThrowNew(env, jc, msg);
}

JNIEXPORT jlong JNICALL Java_nz_ac_waikato_ffts_FFTS_complex_11d
(JNIEnv *env, jclass jc, jint N, jint sign) {
	ffts_plan_t *plan;

	plan = ffts_init_1d(N, sign);
	if (!plan)
		throwOutOfMemoryError(env, NULL);

	return (jlong)plan;
}

JNIEXPORT jlong JNICALL Java_nz_ac_waikato_ffts_FFTS_complex_12d
(JNIEnv *env, jclass jc, jint N1, jint N2, jint sign) {
	ffts_plan_t *plan;

	plan = ffts_init_2d(N1, N2, sign);
	if (!plan)
		throwOutOfMemoryError(env, NULL);

	return (jlong)plan;
}

JNIEXPORT jlong JNICALL Java_nz_ac_waikato_ffts_FFTS_complex_1nd
(JNIEnv *env, jclass jc, jintArray jNs, jint sign) {
	ffts_plan_t *plan;
	int n = (*env)->GetArrayLength(env, jNs);
	int *cNs;
	size_t *Ns;
	int i;

	// Needs to convert java int array to size_t array
	// Get the int elements and conver to C type
	Ns = alloca(sizeof(*Ns) * n);
	cNs = alloca(sizeof(int) * n);
	(*env)->GetIntArrayRegion(env, jNs, 0, n, cNs);
	for (i=0;i<n;i++)
		Ns[i] = cNs[i];

	plan = ffts_init_nd(n, Ns, sign);
	if (!plan)
		throwOutOfMemoryError(env, NULL);

	return (jlong)plan;
}

JNIEXPORT jlong JNICALL Java_nz_ac_waikato_ffts_FFTS_real_11d
(JNIEnv *env, jclass jc, jint N, jint sign) {
	ffts_plan_t *plan;

	plan = ffts_init_1d_real(N, sign);
	if (!plan)
		throwOutOfMemoryError(env, NULL);

	return (jlong)plan;
}

JNIEXPORT jlong JNICALL Java_nz_ac_waikato_ffts_FFTS_real_12d
(JNIEnv *env, jclass jc, jint N1, jint N2, jint sign) {
	ffts_plan_t *plan;

	plan = ffts_init_2d_real(N1, N2, sign);
	if (!plan)
		throwOutOfMemoryError(env, NULL);

	return (jlong)plan;
}

JNIEXPORT jlong JNICALL Java_nz_ac_waikato_ffts_FFTS_real_1nd
(JNIEnv *env, jclass jc, jintArray jNs, jint sign) {
	ffts_plan_t *plan;
	int n = (*env)->GetArrayLength(env, jNs);
	int *cNs;
	size_t *Ns;
	int i;

	// Needs to convert java int array to size_t array
	// Get the int elements and conver to C type
	Ns = alloca(sizeof(*Ns) * n);
	cNs = alloca(sizeof(int) * n);
	(*env)->GetIntArrayRegion(env, jNs, 0, n, cNs);
	for (i=0;i<n;i++)
		Ns[i] = cNs[i];

	plan = ffts_init_nd_real(n, Ns, sign);
	if (!plan)
		throwOutOfMemoryError(env, NULL);

	return (jlong)plan;
}

JNIEXPORT void JNICALL Java_nz_ac_waikato_ffts_FFTS_execute__JJ_3FI_3FI
(JNIEnv *env, jclass jc, jlong p, jlong size, jfloatArray jsrc, jint soff, jfloatArray jdst, jint doff) {
	ffts_plan_t *plan = (ffts_plan_t *)p;

	// TODO: check performance on android/arm
#ifdef NEEDS_ALIGNED
	// On oracle jvm this is faster than GetFloatArrayElements()
	void *src, *dst;

	src = xmemalign(64, size * 4);
	if (!src) {
		throwOutOfMemoryError(env, NULL);
		return;
	}
	dst = xmemalign(64, size * 4);
	if (!dst) {
		free(src);
		throwOutOfMemoryError(env, NULL);
		return;
	}

	(*env)->GetFloatArrayRegion(env, jsrc, 0, size, src + soff);
	ffts_execute(plan, src, dst);
	(*env)->SetFloatArrayRegion(env, jdst, 0, size, dst + doff);

	free(dst);
	free(src);
#else
	// This is the fastest with oracle jvm, but doesn't work with sse ...
	void *src = (*env)->GetPrimitiveArrayCritical(env, jsrc, NULL);
	void *dst = (*env)->GetPrimitiveArrayCritical(env, jdst, NULL);

	ffts_execute(plan, src + soff, dst + doff);

	(*env)->ReleasePrimitiveArrayCritical(env, jdst, dst, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, jsrc, src, 0);
#endif

#if 0
	// This is the slowest
	void *src = (*env)->GetFloatArrayElements(env, jsrc, NULL);
	void *dst = (*env)->GetFloatArrayElements(env, jdst, NULL);

	ffts_execute(plan, src + soff, dst + doff);

	(*env)->ReleaseFloatArrayElements(env, jdst, dst, 0);
	(*env)->ReleaseFloatArrayElements(env, jsrc, src, 0);
#endif
}

JNIEXPORT void JNICALL Java_nz_ac_waikato_ffts_FFTS_execute__JJLjava_nio_FloatBuffer_2Ljava_nio_FloatBuffer_2
(JNIEnv *env, jclass jc, jlong p, jlong size, jobject jsrc, jobject jdst) {
	ffts_plan_t *plan = (ffts_plan_t *)p;
	void *src = (*env)->GetDirectBufferAddress(env, jsrc);
	void *dst = (*env)->GetDirectBufferAddress(env, jdst);

	// Bounds checking etc is in java side.

	ffts_execute(plan, src, dst);
}

JNIEXPORT void JNICALL Java_nz_ac_waikato_ffts_FFTS_free
(JNIEnv *env, jclass jc, jlong p) {
	ffts_plan_t *plan = (ffts_plan_t *)p;

	ffts_free(plan);
}

// vim: set autoindent noexpandtab tabstop=3 shiftwidth=3:
