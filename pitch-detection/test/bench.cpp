#include "pitch_detection.h"
#include "util.h"
#include <benchmark/benchmark.h>

static void
BM_Swipe_Sinewave(benchmark::State &state)
{
	auto data = test_util::sinewave(state.range(0), 1337, 48000);
	for (auto _ : state)
		pitch::swipe(data, 48000);
	state.SetComplexityN(state.range(0));
}

static void
BM_Yin_Sinewave(benchmark::State &state)
{
	auto data = test_util::sinewave(state.range(0), 1337, 48000);
	for (auto _ : state)
		pitch::yin<double>(data, 48000);
	state.SetComplexityN(state.range(0));
}

static void
BM_Mpm_Sinewave(benchmark::State &state)
{
	auto data = test_util::sinewave(state.range(0), 1337, 48000);
	for (auto _ : state)
		pitch::mpm<double>(data, 48000);
	state.SetComplexityN(state.range(0));
}

static void
BM_Yin_Sinewave_Alloc(benchmark::State &state)
{
	auto data = test_util::sinewave(state.range(0), 1337, 48000);
	pitch_alloc::Yin<double> ya(data.size());
	for (auto _ : state)
		ya.pitch(data, 48000);
	state.SetComplexityN(state.range(0));
}

static void
BM_Mpm_Sinewave_Alloc(benchmark::State &state)
{
	auto data = test_util::sinewave(state.range(0), 1337, 48000);
	pitch_alloc::Mpm<double> ma(data.size());
	for (auto _ : state)
		ma.pitch(data, 48000);
	state.SetComplexityN(state.range(0));
}

BENCHMARK(BM_Swipe_Sinewave)->Range(1 << 10, 1 << 20)->Complexity();
BENCHMARK(BM_Yin_Sinewave)->Range(1 << 10, 1 << 20)->Complexity();
BENCHMARK(BM_Mpm_Sinewave)->Range(1 << 10, 1 << 20)->Complexity();

BENCHMARK(BM_Yin_Sinewave_Alloc)->Range(1 << 10, 1 << 20)->Complexity();
BENCHMARK(BM_Mpm_Sinewave_Alloc)->Range(1 << 10, 1 << 20)->Complexity();

BENCHMARK_MAIN();
