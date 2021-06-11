/*
 *  This file is part of FFTS -- The Fastest Fourier Transform in the South
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
package nz.ac.waikato.ffts;

import java.nio.FloatBuffer;

/**
 * A java wrapper for ffts plans.
 *
 * Plans must currently be freed explicitly.
 *
 * @author notzed
 */
public class FFTS {

	/**
	 * C pointer
	 */
	private long p;
	/**
	 * Minimum size of input
	 */
	final protected long inSize;
	/**
	 * Minimum size of output
	 */
	final protected long outSize;

	private FFTS(long p, long inSize) {
		this(p, inSize, inSize);
	}

	private FFTS(long p, long inSize, long outSize) {
		this.p = p;
		this.inSize = inSize;
		this.outSize = inSize;
	}
	/**
	 * The sign to use for a forward transform.
	 */
	public static final int FORWARD = -1;
	/**
	 * The sign to use for a backward transform.
	 */
	public static final int BACKWARD = 1;

	/**
	 * Create a FFT plan for a 1-dimensional complex transform.
	 *
	 * The src and dst parameters to execute() use complex data.
	 *
	 * @param sign The direction of the transform.
	 * @param N The size of the transform.
	 * @return
	 */
	public static FFTS complex(int sign, int N) {
		return new FFTS(complex_1d(N, sign), N * 2);
	}

	/**
	 * Create a FFT plan for a 2-dimensional complex transform.
	 * @param sign The direction of the transform.
	 * @param N1 The size of the transform.
	 * @param N2 The size of the transform.
	 * @return
	 */
	public static FFTS complex(int sign, int N1, int N2) {
		return new FFTS(complex_2d(N1, N2, sign), N1 * N2 * 2);
	}

	public static FFTS complex(int sign, int... Ns) {
		return new FFTS(complex_nd(Ns, sign), size(Ns) * 2);
	}

	public static FFTS real(int sign, int N) {
		return new FFTS(real_1d(N, sign), sign == FORWARD ? N : (N / 2 + 1) * 2, sign == FORWARD ? (N / 2 + 1) * 2 : N);
	}

	public static FFTS real(int sign, int N1, int N2) {
		return new FFTS(real_2d(N1, N2, sign), sign == FORWARD ? N1 * N2 : (N1 * N2 / 2 + 1) * 2, sign == FORWARD ? (N1 * N2 / 2 + 1) * 2 : N1 * N2);
	}

	public static FFTS real(int sign, int... Ns) {
		return new FFTS(real_nd(Ns, sign), sign == FORWARD ? size(Ns) : (size(Ns) / 2 + 1) * 2, sign == FORWARD ? (size(Ns) / 2 + 1) * 2 : size(Ns));
	}

	/**
	 * Execute this plan with the given array data.
	 *
	 * @param src
	 * @param dst
	 */
	public void execute(float[] src, float[] dst) {
		execute(src, 0, dst, 0);
	}

	/**
	 * Execute this plan with the given array data.
	 * @param src
	 * @param soff Start offset into src array.
	 * @param dst
	 * @param doff Start offset into dst array.
	 */
	public void execute(float[] src, int soff, float[] dst, int doff) {
		if (src.length - soff < inSize || dst.length - doff < outSize)
			throw new ArrayIndexOutOfBoundsException();
		if (p == 0)
			throw new NullPointerException();

		execute(p, inSize, src, soff, dst, doff);
	}

	/**
	 * Execute this plan with the given nio buffers.  The bufffers
	 * must be derived from direct buffers.
	 *
	 * The buffer position and limits are ignored.
	 *
	 * @param src
	 * @param dst
	 */
	public void execute(FloatBuffer src, FloatBuffer dst) {
		if (src.capacity() < inSize || dst.capacity() < outSize)
			throw new ArrayIndexOutOfBoundsException();
		if (p == 0)
			throw new NullPointerException();

		execute(p, inSize, src, dst);
	}

	/**
	 * Free the plan.
	 */
	public void free() {
		if (p == 0)
			throw new NullPointerException();
		free(p);
	}

	/*
	 * Calculate the number of elements required to store one
	 * set of n-dimensional data.
	 */
	protected static long size(int[] Ns) {
		long s = Ns[0];
		for (int i = 1; i < Ns.length; i++)
			s *= Ns[i];
		return s;
	}

	static {
		System.loadLibrary("ffts_jni");
	}

	/*
	 * Native interface
	 */
	protected static native long complex_1d(int N, int sign);

	protected static native long complex_2d(int N1, int N2, int sign);

	protected static native long complex_nd(int[] Ns, int sign);

	protected static native long real_1d(int N, int sign);

	protected static native long real_2d(int N1, int N2, int sign);

	protected static native long real_nd(int[] Ns, int sign);

	protected static native void execute(long p, long size, float[] src, int soff, float[] dst, int doff);

	protected static native void execute(long p, long size, FloatBuffer src, FloatBuffer dst);

	protected static native void free(long p);
}
