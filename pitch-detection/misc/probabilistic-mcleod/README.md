# probabilistic-mcleod

An attempted probabilistic version of the McLeod pitch method, inspired by probabilistic YIN.

Mcleod paper: http://miracle.otago.ac.nz/tartini/papers/A_Smarter_Way_to_Find_Pitch.pdf
PYIN paper: https://www.eecs.qmul.ac.uk/~simond/pub/2014/MauchDixon-PYIN-ICASSP2014.pdf

### Motivation

I recently implemented Probabilistic YIN as described in the paper. YIN operates with a constant, `threshold`, which is up to the implementer to define (I used `0.20`, taken from TarsosDSP). In PYIN, `threshold` is distributed between `0.01` and `1.0`, with probabilities computed with some [strange](https://stats.stackexchange.com/questions/398511/cant-understand-the-beta-distribution-as-described-in-this-paper) beta distributions. The idea is that if a pitch candidate "wins" for multiple values of threshold, the probability that this is the correct pitch increases.

I attempted to apply the same reasoning to the McLeod pitch method. One of the parameters in MPM is the constant `k`, referred to in the code as `Cutoff`:

>From the key maxima we define a threshold which is
equal to the value of the highest maximum, nmax, multiplied by a constant k. We then take the first key maximum
which is above this threshold and assign its delay, τ , as
the pitch period. The constant, k, has to be large enough
to avoid choosing peaks caused by strong harmonics, such
as those in Figure 2, but low enough to not choose the unwanted beat or sub-harmonic signals. Choosing an incorrect key maximum causes a pitch error, usually a ’wrong
octave’. Pitch is a subjective quantity and impossible to
get correct all the time. In special cases, the pitch of a
given note will be judged differently by different, expert
listeners. We can endeavour to get the pitch agreed by the
user/musician as most often as possible. The value of k
can be adjusted to achieve this, usually in the range 0.8 to
1.0.

So, `0.8 < k < 1.0`. In my regular MPM code, I use `k = 0.93`. In my attempt of probabilistic MPM, I choose values of k in that range, in steps of `0.01`, i.e. 20 total increments, with a uniform probability distribution of `probability[20] = 0.05`. The total probability sums to 1.0, and each increment between 0.8 and 1.0 has the same probability of being correct.

### Results

In one of the test cases, `Classical guitar F#4`, the probabilistic MPM output multiple candidates:

```
[ RUN      ] PMpmInstrumentTest.Classical_FSharp4_48000
PMpm pitch: 372.387     probability: 0.9
PMpm pitch: 186.137     probability: 0.1
```

Comparatively, probabilistic YIN gives:

```
[ RUN      ] PYinInstrumentTest.Classical_FSharp4_48000
PYin pitch: 13610.8     probability: 0.305056
PYin pitch: 741.156     probability: 0.104046
PYin pitch: 372.504     probability: 0.52492
PYin pitch: 186.147     probability: 0.030646
PYin pitch: 0.549331    probability: 0.035329
```
