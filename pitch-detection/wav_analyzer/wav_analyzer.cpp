#include <algorithm>
#include <functional>
#include <iostream>

#include "pitch_detection.h"
#include <gflags/gflags.h>
#include <libnyquist/Decoders.h>

DEFINE_double(timeslice, 0.1, "Time slice");

template <class T>
std::vector<T>
get_chunks(T container, size_t k)
{
	std::vector<T> ret;

	size_t size = container.size();
	size_t i = 0;

	if (size > k) {
		for (; i < size - k; i += k) {
			ret.push_back(T(container.begin() + i, container.begin() + i + k));
		}
	}

	if (i % k) {
		ret.push_back(
		    T(container.begin() + i, container.begin() + i + (i % k)));
	}

	return ret;
}

int
main(int argc, char **argv)
{
	gflags::SetUsageMessage("help\n");
	gflags::ParseCommandLineFlags(&argc, &argv, true);

	std::cout << "Slicing wav file into chunks of " << FLAGS_timeslice
	          << " seconds..." << std::endl;

	nqr::NyquistIO loader;

	if (argc != 2) {
		std::cerr << "Usage: wav_analyzer /path/to/audio/file" << std::endl;
		return -1;
	}

	std::shared_ptr<nqr::AudioData> file_data =
	    std::make_shared<nqr::AudioData>();
	loader.Load(file_data.get(), argv[1]);

	std::cout << "Audio file info:" << std::endl;

	std::cout << "\tsample rate: " << file_data->sampleRate << std::endl;
	std::cout << "\tlen samples: " << file_data->samples.size() << std::endl;
	std::cout << "\tframe size: " << file_data->frameSize << std::endl;
	std::cout << "\tseconds: " << file_data->lengthSeconds << std::endl;
	std::cout << "\tchannels: " << file_data->channelCount << std::endl;

	std::transform(file_data->samples.begin(), file_data->samples.end(),
	    file_data->samples.begin(),
	    std::bind(std::multiplies<float>(), std::placeholders::_1, 10000));

	std::vector<float> audio;

	if (file_data->channelCount == 2) {
		// convert stereo to mono
		std::vector<float> audio_copy(file_data->samples.size() / 2);
		nqr::StereoToMono(file_data->samples.data(), audio_copy.data(),
		    file_data->samples.size());
		audio = std::vector<float>(audio_copy.begin(), audio_copy.end());
	} else {
		audio = std::vector<float>(
		    file_data->samples.begin(), file_data->samples.end());
	}

	auto sample_rate = file_data->sampleRate;

	auto chunk_size = size_t(sample_rate * FLAGS_timeslice);

	auto chunks = get_chunks(audio, chunk_size);

	std::cout << "Slicing buffer size " << audio.size() << " into "
	          << chunks.size() << " chunks of size " << chunk_size << std::endl;

	double t = 0.;

	for (auto chunk : chunks) {
		std::cout << "At t: " << t << std::endl;

		auto pitch_mpm = pitch::mpm<float>(chunk, sample_rate);
		auto pitch_yin = pitch::yin<float>(chunk, sample_rate);
		auto pitch_pmpm = pitch::pmpm<float>(chunk, sample_rate);
		auto pitch_pyin = pitch::pyin<float>(chunk, sample_rate);
		auto pitch_swipe = pitch::swipe<float>(chunk, sample_rate);

		std::cout << "\tmpm: " << pitch_mpm << "\n\tyin: " << pitch_yin
		          << "\n\tswipe: " << pitch_swipe << "\n\tpmpm: " << pitch_pmpm
		          << "\n\tpyin: " << pitch_pyin << std::endl;

		t += FLAGS_timeslice;
	}

	return 0;
}
