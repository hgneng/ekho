### Pitch detection algorithms

Autocorrelation-based C++ pitch detection algorithms with **O(nlogn) or lower** running time:

* McLeod pitch method - [2005 paper](http://miracle.otago.ac.nz/tartini/papers/A_Smarter_Way_to_Find_Pitch.pdf) - [visualization](./misc/mcleod)
* YIN(-FFT) - [2002 paper](http://audition.ens.fr/adc/pdf/2002_JASA_YIN.pdf) - [visualization](./misc/yin)
* Probabilistic YIN - [2014 paper](https://www.eecs.qmul.ac.uk/~simond/pub/2014/MauchDixon-PYIN-ICASSP2014.pdf)
* Probabilistic MPM - [my own invention](./misc/probabilistic-mcleod)
* SWIPE' - [2007 paper](https://pdfs.semanticscholar.org/0fd2/6e267cfa9b6d519967ea00db4ffeac272777.pdf) - [transliterated to C++ from kylebgorman's C implementation](https://github.com/kylebgorman/swipe)\*, \*\*

\*: SWIPE' appears to be O(n) but with an enormous constant factor. The implementation complexity is much higher than MPM and YIN and it brings in additional dependencies (BLAS + LAPACK).

\*\*: There's a parallel version of SWIPE, [Aud-SWIPE-P](https://github.com/saul-calderonramirez/Aud-SWIPE-P).

Suggested usage of this library can be seen in the utility [wav_analyzer](./wav_analyzer), which divides a wav file into chunks of 0.01s and checks the pitch of each chunk. Sample output of wav_analyzer:

```
At t: 0.5
        mpm: 162.529
        yin: 162.543
        swipe: 162.183
        pmpm: 162.529
        pyin: 162.543
```

### Degraded audio tests

All testing files are [here](./degraded_audio_tests) - the progressive degradations are described by the respective numbered JSON file, generated using [audio-degradation-toolbox](https://github.com/sevagh/audio-degradation-toolbox). The original clip is a Viola playing E3 from the [University of Iowa MIS](http://theremin.music.uiowa.edu/MIS.html).

The results come from parsing the output of wav_analyzer to count how many 0.1s slices of the input clip were in the ballpark of the expected value of 164.81 - I considered anything 160-169 to be acceptable:

| Degradation level | MPM # correct | YIN # correct | SWIPE' # correct |
| ------------- | ------------- | ------------- | ------------- |
| 0 | 26 | 22 | 5 |
| 1 | 23 | 21 | 13 |
| 2 | 19 | 21 | 9 |
| 3 | 18 | 19 | 7 |
| 4 | 19 | 19 | 6 |
| 5 | 18 | 19 | 5 |

### Build and install

Using this project should be as easy as `make && sudo make install` on Linux with a modern GCC - I don't officially support other platforms.

This project depends on [ffts](https://github.com/anthonix/ffts), BLAS/LAPACK, and mlpack. To run the tests, you need [googletest](https://github.com/google/googletest), and run `make -C test/ && ./test/test`. To run the bench, you need [google benchmark](https://github.com/google/benchmark), and run `make -C test/ bench && ./test/bench`.

Build and install pitch_detection, run the tests, and build the sample application, wav_analyzer:

```bash
# build libpitch_detection.so
make clean all

# build tests and benches
make -C test clean all

# run tests and benches 
./test/test
./test/bench

# install the library and headers to `/usr/local/lib` and `/usr/local/include`
sudo make install

# build and run C++ sample
make -C wav_analyzer clean all
./wav_analyzer/wav_analyzer
```

#### Docker

To simplify the setup, there's a [Dockerfile](./Dockerfile) that sets up a Ubuntu container with all the dependencies for compiling the library and running the included tests and benchmarks. You can build the image or pull it from DockerHub ([esimkowitz/pitchdetection](https://hub.docker.com/repository/docker/esimkowitz/pitchdetection)):

```bash
# build
$ docker build --rm --pull -f "Dockerfile" -t pitchdetection:latest "."
$ docker run --rm --init -it pitchdetection:latest

# pull
$ docker pull esimkowitz/pitchdetection:latest
$ docker run --rm --init -it esimkowitz/pitchdetection:latest
```

Once you're in the container, run the tests and benches:

```bash
./test/test
./test/bench
```

### Usage

Read the [header](./include/pitch_detection/pitch_detection.h) and [sample wav_analyzer](./wav_analyzer).

The namespaces are `pitch` and `pitch_alloc`. The functions and classes are templated for `<double>` and `<float>` support.

The `pitch` namespace functions perform automatic buffer allocation, while `pitch_alloc::{Yin, Mpm}` give you a reusable object (useful for computing pitch for multiple uniformly-sized buffers):

```c++
#include <pitch_detection.h>

std::vector<double> audio_buffer(8092);

double pitch_yin = pitch::yin<double>(audio_buffer, 48000);
double pitch_mpm = pitch::mpm<double>(audio_buffer, 48000);
double pitch_pyin = pitch::pyin<double>(audio_buffer, 48000);
double pitch_pmpm = pitch::pmpm<double>(audio_buffer, 48000);
double pitch_swipe = pitch::swipe<double>(audio_buffer, 48000);

pitch_alloc::Mpm<double> ma(8092);
pitch_alloc::Yin<double> ya(8092);

for (int i = 0; i < 10000; ++i) {
        auto pitch_yin = ya.pitch(audio_buffer, 48000);
        auto pitch_mpm = ma.pitch(audio_buffer, 48000);
        auto pitch_pyin = ya.probabilistic_pitch(audio_buffer, 48000);
        auto pitch_pmpm = ma.probabilistic_pitch(audio_buffer, 48000);
}
```
