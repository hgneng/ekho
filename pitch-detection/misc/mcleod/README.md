# What is pitch detection?

>A pitch detection algorithm (PDA) is an algorithm designed to estimate the pitch or fundamental frequency of a quasiperiodic or oscillating signal, usually a digital recording of speech or a musical note or tone. This can be done in the time domain, the frequency domain, or both.

- https://en.wikipedia.org/wiki/Pitch_detection_algorithm

In short, I have a recording of a guitar note, and I want to recognize what the pitch is - what the note being played is.

Musical notes map to frequency in Hz. Integer multiples of a frequency are known as octaves - e.g., E is 20.6, 41.2, 82.4 (open low E on guitar), 164.8, 329.6 (open high E on guitar), ...

Surprise tidbit: https://dsp.stackexchange.com/questions/19379/improving-an-auto-correlation-based-guitar-pitch-detector

# The McLeod Pitch Method

The McLeod Pitch Method is a method for pitch detection for musical instruments and voices. Naive autocorrelation may be tripped up by overtones:

>Most oscillators, from a guitar string to a flute, will naturally vibrate at a series of distinct frequencies known as normal modes. The lowest normal mode frequency is known as the fundamental frequency, while the higher frequencies are called overtones. Often, when an oscillator is excited — for example, by plucking a guitar string — it will oscillate at several of its modal frequencies at the same time. So when a note is played, this gives the sensation of hearing other frequencies (overtones) above the lowest frequency (the fundamental).
- https://en.wikipedia.org/wiki/Overtone


The paper describing the McLeod Pitch Method is: _A Smarter Way to Find Pitch_ by Philip McLeod and Geoff Wyvill.

I'll display examples of using naive autocorrelation on an 83Hz sine wave, and an 82.4Hz open low E string on a guitar in standard EADGBE tuning, and introduce the various improvements from the McLeod Pitch Method to show how it's better at detecting the pitch of the guitar string.

The code in this repository implements various formulae presented in the paper, piecemeal. The plots presented here are all generated with `cargo test -- --nocapture` to print the samples to stdout and plotted using gnuplot.

# 2 - Autocorrelation

## 83Hz sinewave

Basic autocorrelation is described in section 2 of the paper.

The autocorrelation tapers as the lag increases. Qualitatively, think of this as the fact that a signal starts at full overlap (so the signal is as similar to itself as possible) and as lag increases, the overlap decreases, so the signal starts being less similar to itself.

This is the autocorrelation function of an 83Hz sinewave at a 44100Hz sample rate with 4096 samples - the x-axis represents the lag, so:

* x=0 is the signal autocorrelated with itself
* x=1 is the signal autocorrelated with a version of it shifted by 1 sample
* ...

![sine_autocorrelation](./.github/sine_autocorrelation.png)

Peaks for the above autocorrelation graph (i.e. points of lag where the autocorrelation is highest) are:

```
[
    529,
    1060,
    1591,
    2122,
    2652,
    3181,
    3702
]
```

Take the lag increment - here it's ~531 samples (529-0, 1060-529=531, 1591-1060=531). 44100Hz, the sample rate, divided by 531, gives ~83Hz, which is the frequency of the sine wave.

Here's a graph of the sine wave with a copy at each of these lags:

![lags](./.github/sine_peak_lags.png)

Overlaid, we can see that these lagged copies have the same periodicity.

## 83Hz open E string on guitar

Now we look at the deficiencies of straight autocorrelation to set the stage for why the McLeod method's improvements matter.

Results of the computation:

```
282.3442501846836
test autocorrelation::tests::test_autocorrelation2_periodicity_open_e ... ok
282.3442501846836
test autocorrelation::tests::test_autocorrelation1_periodicity_open_e ... ok
```

282 is more in the realm of the high E string i.e. 82.41 * 4 = 329.64Hz, but it's pretty incorrect - it's not an integer multiple of 82.4 - 3.4x - so you can't make the argument that it's thrown off by the overtones. It's just wrong.

Autocorrelation plot for the open E string pluck:

![open_e_autocorrelation](./.github/open_e_autocorrelation.png)

The signal overlaid with the peak lagged versions of itself demonstrates the poor result of the autocorrelation:

![lags](./.github/open_e_peak_lags_wrong.png)

What's happening here? Overtones, and maybe noise (although I picked a clean-ish recording of the open E string, from [this video](https://www.youtube.com/watch?v=bKS_m7JObxg)).

# 4 - NSDF function

We're going to briefly show the SDF function to show that it needs normalization and only NSDF will be considered going forward.

## 83Hz sinewave

Apply the same periodicity calculation as above but to the SDF instead of the autocorrelation:

![sine_sdf](./.github/sine_sdf.png)

![sine_nsdf](./.github/sine_nsdf.png)

Outputs:

```
83.83233532934132
test autocorrelation::tests::test_autocorrelation2_periodicity_sinewave ... ok
83.83233532934132
test autocorrelation::tests::test_autocorrelation1_periodicity_sinewave ... ok
94.11764705882354
test sdf::tests::test_sdf_periodicity_sinewave ... ok
83.00395256916995
test nsdf::tests::test_nsdf_periodicity_sinewave ... ok
```

SDF is in a weird spot - it needs normalization. NSDF gets closer to the exact value - 83.004 vs. 83.832. Graphed with NSDF, the lags looks mostly identical so I won't show it again.

## 83Hz open E string on guitar

Results:

```
282.3442501846836
test autocorrelation::tests::test_autocorrelation2_periodicity_open_e ... ok
282.3442501846836
test autocorrelation::tests::test_autocorrelation1_periodicity_open_e ... ok
238.55421686746988
test sdf::tests::test_sdf_periodicity_open_e ... ok
249.76606747106624
test nsdf::tests::test_nsdf_periodicity_open_e ... ok
```

249 is still not 82.4Hz, but at least it's closer to an integer multiple, 3x. NSDF has improved our values - they're now "correct" - anything periodic with 83Hz is also periodic for integer multiples of 83Hz - but get tripped up by overtones:

![open_e_nsdf](./.github/open_e_nsdf.png)

Overlaid with its lags, it matches up slightly better:

![lags](./.github/open_e_peak_lags_better.png)

# 5 - Peak picking

There's a better method of peak picking as described in the paper, with and without parabolic interpolation. In the code, they're represented as an enum `PeakPickingStrategy::Naive, Better, Best`:

```
167.680608365019
test peak_picking::tests::test_peak_picking_open_e_autocorrelation_better ... ok
282.3442501846836
test peak_picking::tests::test_peak_picking_open_e_autocorrelation_naive ... ok
```

With better peak picking on the open E, the detected frequency reaches 167, which is 2x the correct answer - the integer multiple is closer to reality. Overlaid with lags, the overlaying is _slightly_ better:

![lags](./.github/open_e_peak_picking_lags_autocorr.png)

## 5.1 - Peak picking with parabolic interpolation - putting it all together

Parabolic interpolation provides the best results - almost entirely correct. The improvements brought about by nsdf alone are overshadowed:

```
82.58426966292134
test peak_picking::tests::test_peak_picking_open_e_autocorrelation_best ... ok
82.58426966292134
test peak_picking::tests::test_peak_picking_open_e_nsdf_best ... ok
```

With parabolic interpolation, we get a single value rather than a collection of lags, but the idea is the same - it's a lag increment of sorts, so we can create an array of lags that fall within `0, len(signal)`:

```
[
    534,
    1068,
    1602,
    2136,
    2670,
    3204,
    3738
]
```

The correctness of the answer is obvious when the signal is overlaid with these lags:

![lags](./.github/open_e_nsdf_peak_lags_best.png)

Almost a perfect match.

# Bonus - degraded Viola E3

NSDF of clean viola clip (deg0):

![clean_nsdf](./.github/undegraded_e3_nsdf.png)

NSDF of unclean viola clip (deg4):

![degraded_nsdf](./.github/degraded_e3_nsdf.png)
