#include "pitch_detection.h"
#include "util.h"
#include <gtest/gtest.h>

class MpmSinewaveTest : public testing::TestWithParam<double>
{
};

class YinSinewaveTest : public testing::TestWithParam<double>
{
};

class SwipeSinewaveTest : public testing::TestWithParam<double>
{
};

class PMpmSinewaveTest : public testing::TestWithParam<double>
{
};

class PYinSinewaveTest : public testing::TestWithParam<double>
{
};

TEST_P(PYinSinewaveTest, GetFreqManualAlloc)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	pitch_alloc::Yin<double> pya(data.size());
	double pitch = pya.probabilistic_pitch(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

INSTANTIATE_TEST_CASE_P(PYinSinewave, PYinSinewaveTest,
    ::testing::Values(77.0, 100.0, 233.0, 298.0, 1583.0, 3398.0, 4200.0));

TEST_P(PMpmSinewaveTest, GetFreqManualAlloc)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	pitch_alloc::Mpm<double> pma(data.size());
	double pitch = pma.probabilistic_pitch(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

INSTANTIATE_TEST_CASE_P(PMpmSinewave, PMpmSinewaveTest,
    ::testing::Values(100.0, 233.0, 298.0, 1583.0, 3398.0, 4200.0));

TEST_P(MpmSinewaveTest, GetFreq)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	double pitch = pitch::mpm<double>(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

TEST_P(SwipeSinewaveTest, GetFreq)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	double pitch = pitch::mpm<double>(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

TEST_P(YinSinewaveTest, GetFreq)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	double pitch = pitch::yin<double>(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

TEST_P(MpmSinewaveTest, GetFreqManualAlloc)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	pitch_alloc::Mpm<double> ma(data.size());
	double pitch = ma.pitch(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

TEST_P(YinSinewaveTest, GetFreqManualAlloc)
{
	double freq = GetParam();
	auto data = test_util::sinewave(8092, freq, 48000);
	pitch_alloc::Yin<double> ya(data.size());
	double pitch = ya.pitch(data, 48000);
	EXPECT_NEAR(freq, pitch, 0.01 * freq);
}

TEST(MpmSinewaveTestManualAlloc, OneAllocMultipleFreq)
{
	auto data1 = test_util::sinewave(8092, 150.0, 48000);
	auto data2 = test_util::sinewave(8092, 250.0, 48000);
	auto data3 = test_util::sinewave(8092, 350.0, 48000);

	pitch_alloc::Mpm<double> ma(data1.size());

	double pitch1 = ma.pitch(data1, 48000);
	double pitch2 = ma.pitch(data2, 48000);
	double pitch3 = ma.pitch(data3, 48000);

	EXPECT_NEAR(150.0, pitch1, 0.01 * 150.0);
	EXPECT_NEAR(250.0, pitch2, 0.01 * 250.0);
	EXPECT_NEAR(350.0, pitch3, 0.01 * 350.0);
}

TEST(YinSinewaveTestManualAlloc, OneAllocMultipleFreq)
{
	auto data1 = test_util::sinewave(8092, 150.0, 48000);
	auto data2 = test_util::sinewave(8092, 250.0, 48000);
	auto data3 = test_util::sinewave(8092, 350.0, 48000);

	pitch_alloc::Yin<double> ya(data1.size());

	double pitch1 = ya.pitch(data1, 48000);
	double pitch2 = ya.pitch(data2, 48000);
	double pitch3 = ya.pitch(data3, 48000);

	EXPECT_NEAR(150.0, pitch1, 0.01 * 150.0);
	EXPECT_NEAR(250.0, pitch2, 0.01 * 250.0);
	EXPECT_NEAR(350.0, pitch3, 0.01 * 350.0);
}

// no 77.0hz for mpm because it can't
INSTANTIATE_TEST_CASE_P(MpmSinewave, MpmSinewaveTest,
    ::testing::Values(100.0, 233.0, 298.0, 1583.0, 3398.0, 4200.0));

INSTANTIATE_TEST_CASE_P(YinSinewave, YinSinewaveTest,
    ::testing::Values(77.0, 100.0, 233.0, 298.0, 1583.0, 3398.0, 4200.0));

INSTANTIATE_TEST_CASE_P(SwipeSinewave, SwipeSinewaveTest,
    ::testing::Values(100.0, 233.0, 298.0, 1583.0, 3398.0, 4200.0));
