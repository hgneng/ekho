# YIN

YIN analyzed. Plots generated with gnuplot, numbers generated from the Go code in this directory.

The paper describing YIN is: _YIN, a fundamental frequency estimator for speech and music_ by Alain de Cheveigné and Hideki Kawahara.

The sample used in the YIN analysis is a clip of F#4 classical guitar, taken from freesound.org (and chopped up with pydub and numpy).

### Autocorrelation

As with last time, let's see how autocorrelation fails to give a good result:

![f_sharp_autocorr](./.github/acf.png)

The value of 44 lags here is produced with naive peak picking. Count the number of peaks and divide by their occurences.

This result is incorrect (as expected).

### Difference

Difference function presented in the YIN paper:

![f_sharp_diff](./.github/df.png)

### Cumulative mean normalized difference

The CMND function presented in the YIN paper:

![f_sharp_cmndf](./.github/cmndf.png)

There are two possible values of YIN pitch:

* 129 lags => pitch = 372.09Hz, produced by only using the Absolute Threshold function (described in the YIN paper)
* The value of 128.6 lags, shown in the plot, produced by applying Parabolic Interpolation to the result of the Absolute Threshold function, again as described in the YIN paper

### Comparison to MPM

Similarities with MPM:

* Normalization of the intermediate buffer
* Parabolic interpolation to choose a more refined lag perodicity

Differences with MPM:

* Not autocorrelation based

### Yin FFT

I implemented FFT YIN successfully in the [main YIN code](../../src/yin.cpp), following equation 7 from the YIN paper.

The YIN paper defines the formulation for the difference function, `d_t`, for a lag tau, as expressed in terms of the autocorrelation `r_t`:

```
d_t(tau) = r_t(0) + r_(t+tau)(0) - 2*r_t(tau)
```

The term `r_(t+tau)` was confusing for me, so in my code I substituted it for

```
d_t(tau) = 2*r_t(0) - 2*r_t(tau)
```

, where `r_t(tau)` is the same FFT-based autocorrelation function I use for MPM.

This is a big performance win for YIN from **O(N^2)** to **O(NlogN)**, and all of the tests are still passing.

The accuracy didn't take a huge hit.

Time-domain YIN result:
```
$ time ./bin/stdin --sample_rate 44100 --algo yin <./samples/F-4_48000_classicalguitar.txt
Size: 174759    pitch: 342.271
closest note: F4 (349.2)

real    0m8.066s
user    0m8.020s
sys     0m0.004s
```

FFT YIN result:

```
$ time ./bin/stdin --sample_rate 44100 --algo yin <./samples/F-4_48000_classicalguitar.txt
Size: 174759    pitch: 342.238
closest note: F4 (349.2)

real    0m0.262s
user    0m0.216s
sys     0m0.043s
```

Time-domain YIN benchmark:

```
---------------------------------------------------------------
Benchmark                        Time           CPU Iterations
---------------------------------------------------------------
BM_Yin_Sinewave/1024         60976 ns      60878 ns      11130
BM_Yin_Sinewave/4096       1002633 ns    1000951 ns        688
BM_Yin_Sinewave/32768     64198002 ns   64096428 ns         11
BM_Yin_Sinewave/262144  4125974701 ns 4114026064 ns          1
BM_Yin_Sinewave/1048576 67683358752 ns 67461192598 ns          1
BM_Yin_Sinewave_BigO          0.06 N^2       0.06 N^2
BM_Yin_Sinewave_RMS              0 %          0 %
```

FFT YIN benchmark:

```
---------------------------------------------------------------
Benchmark                        Time           CPU Iterations
---------------------------------------------------------------
BM_Yin_Sinewave/1024         60657 ns      60202 ns      11223
BM_Yin_Sinewave/4096        241264 ns     239170 ns       2967
BM_Yin_Sinewave/32768      2077014 ns    2054092 ns        340
BM_Yin_Sinewave/262144    19811215 ns   19568974 ns         36
BM_Yin_Sinewave/1048576   95163598 ns   93849952 ns          7
BM_Yin_Sinewave_BigO          4.52 NlgN       4.46 NlgN
BM_Yin_Sinewave_RMS              3 %          3 %
```

# Bonus - degraded Viola E3

CMNDF of clean viola clip (deg0):

![clean_cmndf](./.github/cmndf-viola-clean.png)

CMNDF of unclean viola clip (deg4):

![degraded_cmndf](./.github/cmndf-viola-dirty.png)
