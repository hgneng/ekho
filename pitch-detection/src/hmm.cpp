#include <cfloat>
#include <cmath>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <mlpack/core.hpp>
#include <mlpack/methods/hmm/hmm.hpp>

#include "pitch_detection.h"

#define F0 440.0
#define N_BINS 108
#define N_NOTES 12
#define NOTE_OFFSET 57

#define YIN_TRUST 0.5

#define TRANSITION_WIDTH 13
#define SELF_TRANS 0.99

std::vector<double> PITCH_BINS(N_BINS);
std::vector<double> REAL_PITCHES(N_BINS);

const double A = std::pow(2.0, 1.0 / 12.0);

// 108 bins - C0 -> B8
void
detail::init_pitch_bins()
{
	for (int i = 0; i < N_BINS; ++i) {
		auto fi = F0 * std::pow(A, i - NOTE_OFFSET);
		PITCH_BINS[i] = fi;
	}
}

template <typename T>
std::vector<size_t>
detail::bin_pitches(const std::vector<std::pair<T, T>> pitch_candidates)
{
	arma::vec pitch_probs(2 * N_BINS + 1, arma::fill::zeros);
	std::vector<size_t> possible_bins;

	T prob_pitched = 0.0;

	for (auto pitch_candidate : pitch_candidates) {
		// find the most appropriate bin
		T delta = DBL_MAX;
		T prev_delta = DBL_MAX;
		for (int i = 0; i < N_BINS; ++i) {
			delta = std::abs(pitch_candidate.first - PITCH_BINS[i]);
			if (prev_delta < delta) {
				pitch_probs[i - 1] = pitch_candidate.second;
				prob_pitched += pitch_probs[i - 1];
				REAL_PITCHES[i - 1] = pitch_candidate.first;
				break;
			}
			prev_delta = delta;
		}
	}

	T prob_really_pitched = YIN_TRUST * prob_pitched;

	for (int i = 0; i < N_BINS; ++i) {
		if (prob_pitched > 0) {
			pitch_probs[i] *= prob_really_pitched / prob_pitched;
		}
		pitch_probs[i + N_BINS] = (1 - prob_really_pitched) / N_BINS;
	}

	for (size_t i = 0; i < pitch_probs.size(); ++i) {
		auto pitch_probability = pitch_probs[i];
		for (size_t j = 0; j < size_t(100.0 * pitch_probability); ++j)
			possible_bins.push_back(i);
	}

	return possible_bins;
}

mlpack::hmm::HMM<mlpack::distribution::DiscreteDistribution>
detail::build_hmm()
{
	size_t hmm_size = 2 * N_BINS + 1;
	// initial
	arma::vec initial(hmm_size);
	initial.fill(1.0 / double(hmm_size));

	arma::mat transition(hmm_size, hmm_size, arma::fill::zeros);

	// transitions
	for (int i = 0; i < N_BINS; ++i) {
		int half_transition = static_cast<int>(TRANSITION_WIDTH / 2.0);
		int theoretical_min_next_pitch = i - half_transition;
		int min_next_pitch = i > half_transition ? i - half_transition : 0;
		int max_next_pitch =
		    i < N_BINS - half_transition ? i + half_transition : N_BINS - 1;

		double weight_sum = 0.0;
		std::vector<double> weights;

		for (int j = min_next_pitch; j <= max_next_pitch; ++j) {
			if (j <= i) {
				weights.push_back(j - theoretical_min_next_pitch + 1);
			} else {
				weights.push_back(i - theoretical_min_next_pitch + 1 - j + i);
			}
			weight_sum += weights[weights.size() - 1];
		}

		for (int j = min_next_pitch; j <= max_next_pitch; ++j) {
			transition(i, j) =
			    (weights[j - min_next_pitch] / weight_sum * SELF_TRANS);
			transition(i, j + N_BINS) =
			    (weights[j - min_next_pitch] / weight_sum * (1.0 - SELF_TRANS));
			transition(i + N_BINS, j + N_BINS) =
			    (weights[j - min_next_pitch] / weight_sum * SELF_TRANS);
			transition(i + N_BINS, j) =
			    (weights[j - min_next_pitch] / weight_sum * (1.0 - SELF_TRANS));
		}
	}

	// the only valid emissions are exact notes
	// i.e. an identity matrix of emissions
	std::vector<mlpack::distribution::DiscreteDistribution> emissions(hmm_size);

	for (size_t i = 0; i < hmm_size; ++i) {
		emissions[i] = mlpack::distribution::DiscreteDistribution(
		    std::vector<arma::vec>{arma::vec(hmm_size, arma::fill::zeros)});
		emissions[i].Probabilities()[i] = 1.0;
	}

	auto hmm = mlpack::hmm::HMM(initial, transition, emissions);
	return hmm;
}

template <typename T>
T
util::pitch_from_hmm(
    mlpack::hmm::HMM<mlpack::distribution::DiscreteDistribution> hmm,
    const std::vector<std::pair<T, T>> pitch_candidates)
{
	if (pitch_candidates.size() == 0) {
		return -1.0;
	}

	std::vector<T> observation_;

	for (auto obs_to_add : detail::bin_pitches(pitch_candidates)) {
		observation_.push_back(obs_to_add);
	}

	if (observation_.size() == 0) {
		return -1.0;
	}

	arma::mat observation(1, observation_.size());
	for (size_t i = 0; i < observation_.size(); ++i) {
		observation(0, i) = observation_[i];
	}

	arma::Row<size_t> state;
	// auto viterbi_out = hmm.Predict(observation, state);
	hmm.Predict(observation, state);

	// count state with most appearances
	std::map<size_t, size_t> counts;
	for (auto state_ : state) {
		counts[state_]++;
	}

	size_t most_frequent;
	size_t max = 0;
	for (auto map_pair : counts) {
		auto state_ = map_pair.first;
		auto count = map_pair.second;
		if (count > max) {
			most_frequent = state_;
			max = count;
		}
	}

	return REAL_PITCHES[most_frequent];
}

template double
util::pitch_from_hmm<double>(
    mlpack::hmm::HMM<mlpack::distribution::DiscreteDistribution> hmm,
    const std::vector<std::pair<double, double>> pitch_candidates);

template float
util::pitch_from_hmm<float>(
    mlpack::hmm::HMM<mlpack::distribution::DiscreteDistribution> hmm,
    const std::vector<std::pair<float, float>> pitch_candidates);
