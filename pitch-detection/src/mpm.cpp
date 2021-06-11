#include "pitch_detection.h"
#include <algorithm>
#include <complex>
#include <float.h>
#include <map>
#include <numeric>
#include <vector>

#define MPM_CUTOFF 0.93
#define MPM_SMALL_CUTOFF 0.5
#define MPM_LOWER_PITCH_CUTOFF 80.0
#define PMPM_PA 0.01
#define PMPM_N_CUTOFFS 20
#define PMPM_PROB_DIST 0.05
#define PMPM_CUTOFF_BEGIN 0.8
#define PMPM_CUTOFF_STEP 0.01

template <typename T>
static std::vector<int>
peak_picking(const std::vector<T> &nsdf)
{
	std::vector<int> max_positions{};
	int pos = 0;
	int cur_max_pos = 0;
	ssize_t size = nsdf.size();

	while (pos < (size - 1) / 3 && nsdf[pos] > 0)
		pos++;
	while (pos < size - 1 && nsdf[pos] <= 0.0)
		pos++;

	if (pos == 0)
		pos = 1;

	while (pos < size - 1) {
		if (nsdf[pos] > nsdf[pos - 1] && nsdf[pos] >= nsdf[pos + 1] &&
		    (cur_max_pos == 0 || nsdf[pos] > nsdf[cur_max_pos])) {
			cur_max_pos = pos;
		}
		pos++;
		if (pos < size - 1 && nsdf[pos] <= 0) {
			if (cur_max_pos > 0) {
				max_positions.push_back(cur_max_pos);
				cur_max_pos = 0;
			}
			while (pos < size - 1 && nsdf[pos] <= 0.0) {
				pos++;
			}
		}
	}
	if (cur_max_pos > 0) {
		max_positions.push_back(cur_max_pos);
	}
	return max_positions;
}

template <typename T>
T
pitch_alloc::Mpm<T>::probabilistic_pitch(
    const std::vector<T> &audio_buffer, int sample_rate)
{
	util::acorr_r(audio_buffer, this);

	std::map<T, T> t0_with_probability;
	std::vector<std::pair<T, T>> f0_with_probability;

	T cutoff = PMPM_CUTOFF_BEGIN;

	for (int n = 0; n < PMPM_N_CUTOFFS; ++n) {
		std::vector<int> max_positions = peak_picking(this->out_real);
		std::vector<std::pair<T, T>> estimates;

		T highest_amplitude = -DBL_MAX;

		for (int i : max_positions) {
			highest_amplitude = std::max(highest_amplitude, this->out_real[i]);
			if (this->out_real[i] > MPM_SMALL_CUTOFF) {
				auto x = util::parabolic_interpolation(this->out_real, i);
				estimates.push_back(x);
				highest_amplitude = std::max(highest_amplitude, std::get<1>(x));
			}
		}

		if (estimates.empty())
			continue;

		T actual_cutoff = cutoff * highest_amplitude;
		T period = 0;

		for (auto i : estimates) {
			if (std::get<1>(i) >= actual_cutoff) {
				period = std::get<0>(i);
				break;
			}
		}

		auto a = period != 0 ? 1 : PMPM_PA;

		t0_with_probability[period] += a * PMPM_PROB_DIST;

		cutoff += MPM_CUTOFF;
	}

	for (auto tau_estimate : t0_with_probability) {
		if (tau_estimate.first == 0.0) {
			continue;
		}
		auto f0 = (sample_rate / tau_estimate.first);

		f0 = (f0 > MPM_LOWER_PITCH_CUTOFF) ? f0 : -1;

		if (f0 != -1.0) {
			f0_with_probability.push_back(
			    std::make_pair(f0, tau_estimate.second));
		}
	}
	this->clear();

	return util::pitch_from_hmm(this->hmm, f0_with_probability);
}

template <typename T>
T
pitch_alloc::Mpm<T>::pitch(const std::vector<T> &audio_buffer, int sample_rate)
{
	util::acorr_r(audio_buffer, this);

	std::vector<int> max_positions = peak_picking(this->out_real);
	std::vector<std::pair<T, T>> estimates;

	T highest_amplitude = -DBL_MAX;

	for (int i : max_positions) {
		highest_amplitude = std::max(highest_amplitude, this->out_real[i]);
		if (this->out_real[i] > MPM_SMALL_CUTOFF) {
			auto x = util::parabolic_interpolation(this->out_real, i);
			estimates.push_back(x);
			highest_amplitude = std::max(highest_amplitude, std::get<1>(x));
		}
	}

	if (estimates.empty())
		return -1;

	T actual_cutoff = MPM_CUTOFF * highest_amplitude;
	T period = 0;

	for (auto i : estimates) {
		if (std::get<1>(i) >= actual_cutoff) {
			period = std::get<0>(i);
			break;
		}
	}

	T pitch_estimate = (sample_rate / period);

	this->clear();

	return (pitch_estimate > MPM_LOWER_PITCH_CUTOFF) ? pitch_estimate : -1;
}

template <typename T>
T
pitch::mpm(const std::vector<T> &audio_buffer, int sample_rate)
{
	pitch_alloc::Mpm<T> ma(audio_buffer.size());
	return ma.pitch(audio_buffer, sample_rate);
}

template <typename T>
T
pitch::pmpm(const std::vector<T> &audio_buffer, int sample_rate)
{
	pitch_alloc::Mpm<T> ma(audio_buffer.size());
	return ma.probabilistic_pitch(audio_buffer, sample_rate);
}

template class pitch_alloc::Mpm<double>;
template class pitch_alloc::Mpm<float>;

template double
pitch::mpm<double>(const std::vector<double> &audio_buffer, int sample_rate);

template float
pitch::mpm<float>(const std::vector<float> &audio_buffer, int sample_rate);

template double
pitch::pmpm<double>(const std::vector<double> &audio_buffer, int sample_rate);

template float
pitch::pmpm<float>(const std::vector<float> &audio_buffer, int sample_rate);
